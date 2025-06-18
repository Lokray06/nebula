// File: src/main/java/com/juanpa/nebula/transpiler/parser/NebulaParser.java

package com.juanpa.nebula.transpiler.parser;

import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.ast.declarations.ClassDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.MethodDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.NamespaceDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.ConstructorDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.FieldDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.ImportDirective;
import com.juanpa.nebula.transpiler.ast.expressions.*;
import com.juanpa.nebula.transpiler.ast.statements.*;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType;
import com.juanpa.nebula.transpiler.util.ErrorReporter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The NebulaParser is responsible for performing syntactic analysis.
 * It takes a list of tokens from the lexer and attempts to build an
 * Abstract Syntax Tree (AST) based on the Nebula language grammar.
 * This parser uses a recursive-descent approach.
 */
public class NebulaParser
{
	private final List<Token> tokens; // The list of tokens from the lexer
	private final ErrorReporter errorReporter; // For reporting parsing errors
	private int current = 0; // Current position in the token list

	// Field to keep track of the current fully qualified namespace being parsed
	private String currentNamespaceFqn = ""; // Initialized to empty string for global scope
	String[] words = new String[2];

	private record TypeSpecifier(
			Token baseType,
			int rank)
	{
	}

	/**
	 * Constructs a NebulaParser.
	 *
	 * @param tokens        The list of tokens produced by the lexer.
	 * @param errorReporter An instance of ErrorReporter for handling parsing errors.
	 */
	public NebulaParser(List<Token> tokens, ErrorReporter errorReporter)
	{
		this.errorReporter = errorReporter;
		this.tokens = preprocessTokensForAliases(tokens);
	}

	/**
	 * Pre-processes the token stream to handle `alias` declarations.
	 * This method performs two passes:
	 * 1. Finds all valid `alias` declarations and stores them in a map.
	 * 2. Creates a new token stream where alias declarations are removed and
	 * alias usages are replaced with their expanded token sequences.
	 * This allows aliases to be used before they are declared.
	 *
	 * @param originalTokens The raw token stream from the lexer.
	 * @return A new token stream with aliases resolved.
	 */
	private List<Token> preprocessTokensForAliases(List<Token> originalTokens)
	{
		Map<String, List<Token>> discoveredAliases = new HashMap<>();
		List<Token> nonAliasTokens = new ArrayList<>();
		int cursor = 0;

		// Pass 1: Discover all aliases and collect non-alias tokens.
		while(cursor < originalTokens.size() && originalTokens.get(cursor).getType() != TokenType.EOF)
		{
			if(originalTokens.get(cursor).getType() == TokenType.ALIAS)
			{
				Token aliasKeyword = originalTokens.get(cursor++);

				if(cursor >= originalTokens.size() || originalTokens.get(cursor).getType() != TokenType.IDENTIFIER)
				{
					errorReporter.report(aliasKeyword.getLine(), aliasKeyword.getColumn(), "Expected an identifier for the alias name after 'alias' keyword.");
					// Skip until semicolon to recover
					while(cursor < originalTokens.size() && originalTokens.get(cursor).getType() != TokenType.SEMICOLON)
						cursor++;
					if(cursor < originalTokens.size())
						cursor++; // skip semicolon
					continue;
				}
				Token aliasName = originalTokens.get(cursor++);

				List<Token> replacement = new ArrayList<>();
				int replacementStartCursor = cursor;
				while(cursor < originalTokens.size() && originalTokens.get(cursor).getType() != TokenType.SEMICOLON)
				{
					Token part = originalTokens.get(cursor);
					if(part.getType() != TokenType.IDENTIFIER && part.getType() != TokenType.DOT)
					{
						errorReporter.report(part.getLine(), part.getColumn(), "Alias replacement can only consist of identifiers and dots.");
						// Invalidate this alias and skip to semicolon
						replacement.clear();
						while(cursor < originalTokens.size() && originalTokens.get(cursor).getType() != TokenType.SEMICOLON)
							cursor++;
						break;
					}
					replacement.add(part);
					cursor++;
				}

				if(cursor < originalTokens.size() && originalTokens.get(cursor).getType() == TokenType.SEMICOLON)
				{
					cursor++; // consume ';'
					if(replacement.isEmpty() && replacementStartCursor == cursor - 1)
					{ // Check if replacement part was empty
						errorReporter.report(aliasName.getLine(), aliasName.getColumn(), "Alias replacement cannot be empty.");
					}
					else if(!replacement.isEmpty())
					{
						if(discoveredAliases.containsKey(aliasName.getLexeme()))
						{
							errorReporter.report(aliasName.getLine(), aliasName.getColumn(), "Alias '" + aliasName.getLexeme() + "' is already defined.");
						}
						else
						{
							discoveredAliases.put(aliasName.getLexeme(), replacement);
						}
					}
				}
				else
				{
					errorReporter.report(aliasKeyword.getLine(), aliasKeyword.getColumn(), "Malformed alias declaration, expected ';'.");
				}
			}
			else
			{
				nonAliasTokens.add(originalTokens.get(cursor));
				cursor++;
			}
		}

		// Pass 2: Replace aliases in the collected tokens.
		List<Token> finalTokens = new ArrayList<>();
		for(Token token : nonAliasTokens)
		{
			if(token.getType() == TokenType.IDENTIFIER && discoveredAliases.containsKey(token.getLexeme()))
			{
				List<Token> replacementTokens = discoveredAliases.get(token.getLexeme());
				for(Token replacementToken : replacementTokens)
				{
					// Create a new token to carry over the position info of the original alias usage
					finalTokens.add(new Token(
							replacementToken.getType(),
							replacementToken.getLexeme(),
							replacementToken.getLiteral(),
							token.getLine(),
							token.getColumn() // Column info is approximate for expanded aliases
					));
				}
			}
			else
			{
				finalTokens.add(token);
			}
		}

		// Add the EOF token at the end
		if(!originalTokens.isEmpty())
		{
			finalTokens.add(originalTokens.get(originalTokens.size() - 1));
		}

		return finalTokens;
	}

	/**
	 * Starts the parsing process for the entire Nebula program.
	 *
	 * @return The root of the parsed AST (a Program node), or null if parsing fails significantly.
	 */
	public Program parse()
	{
		Program program = new Program();
		while(!isAtEnd())
		{
			try
			{
				// Prioritize parsing 'import' directives before namespace declarations
				if(check(TokenType.IMPORT) || check(TokenType.GLOBAL) || check(TokenType.STATIC))
				{
					int tempCurrent = current;
					boolean hasImport = false;
					while(tempCurrent < tokens.size())
					{
						if(tokens.get(tempCurrent).getType() == TokenType.IMPORT)
						{
							hasImport = true;
							break;
						}
						if(!(tokens.get(tempCurrent).getType() == TokenType.GLOBAL || tokens.get(tempCurrent).getType() == TokenType.STATIC))
						{
							break;
						}
						tempCurrent++;
					}

					if(hasImport)
					{
						program.addImportDirective(importDirective());
						continue;
					}
				}

				if(check(TokenType.NAMESPACE))
				{
					program.addNamespace(namespaceDeclaration());
				}
				else
				{
					error(peek(), "Expected an 'import' directive or a namespace declaration at the top level.");
					synchronize();
				}
			}
			catch(SyntaxError e)
			{
				synchronize();
			}
		}
		return program;
	}

	/**
	 * Parses an 'import' directive.
	 * Grammar: `(GLOBAL | STATIC)* IMPORT QUALIFIED_NAME ;`
	 *
	 * @return An ImportDirective AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private ImportDirective importDirective() throws SyntaxError
	{
		List<Token> modifiers = new ArrayList<>();
		while(check(TokenType.GLOBAL) || check(TokenType.STATIC))
		{
			modifiers.add(advance());
		}

		Token importKeyword = consume(TokenType.IMPORT, "Expected 'import' keyword.");
		Expression qualifiedNameExpr = qualifiedName();
		consume(TokenType.SEMICOLON, "Expected ';' after 'import' directive.");

		return new ImportDirective(modifiers, importKeyword, qualifiedNameExpr);
	}


	/**
	 * Parses a qualified name (e.g., `Com.MyCompany.App`).
	 * Grammar: `IDENTIFIER (DOT IDENTIFIER)*`
	 *
	 * @return An Expression representing the qualified name (e.g., a chain of DotExpressions).
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private Expression qualifiedName() throws SyntaxError
	{
		Expression namePart = new IdentifierExpression(consume(TokenType.IDENTIFIER, "Expected identifier for qualified name."));

		while(match(TokenType.DOT))
		{
			Token dotToken = previous();
			Token rightPart = consume(TokenType.IDENTIFIER, "Expected identifier after '.'.");
			namePart = new DotExpression(namePart, dotToken, rightPart);
		}
		return namePart;
	}

	/**
	 * Parses a namespace declaration.
	 * Grammar: `NAMESPACE QUALIFIED_NAME { (CLASS_DECLARATION | ...)* }`
	 *
	 * @return A NamespaceDeclaration AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private NamespaceDeclaration namespaceDeclaration() throws SyntaxError
	{
		consume(TokenType.NAMESPACE, "Expected 'namespace' keyword.");
		Expression namespaceNameExpr = qualifiedName();

		// Get the FQN string of the current namespace being entered
		String newNamespaceSegment = getExpressionFQN(namespaceNameExpr);
		String previousNamespaceFqn = this.currentNamespaceFqn; // Save previous FQN

		// Update currentNamespaceFqn for nested parsing
		if(this.currentNamespaceFqn.isEmpty())
		{
			this.currentNamespaceFqn = newNamespaceSegment;
		}
		else
		{
			this.currentNamespaceFqn += "." + newNamespaceSegment;
		}

		consume(TokenType.LEFT_BRACE, "Expected '{' after namespace name.");

		NamespaceDeclaration namespaceDecl = new NamespaceDeclaration(namespaceNameExpr);

		while(!check(TokenType.RIGHT_BRACE) && !isAtEnd())
		{
			List<Token> classModifiers = new ArrayList<>();
			while(check(TokenType.PUBLIC) || check(TokenType.PRIVATE) || check(TokenType.STATIC))
			{
				classModifiers.add(advance());
			}

			if(check(TokenType.CLASS))
			{
				// Pass the current fully qualified namespace to classDeclaration
				namespaceDecl.addClass(classDeclaration(classModifiers, this.currentNamespaceFqn));
			}
			else
			{
				error(peek(), "Expected 'class' declaration or '}' to close namespace body.");
				synchronizeNamespaceBody();
			}
		}

		consume(TokenType.RIGHT_BRACE, "Expected '}' after namespace body.");
		this.currentNamespaceFqn = previousNamespaceFqn; // Restore previous FQN
		return namespaceDecl;
	}

	/**
	 * Helper to get the fully qualified name string from an Expression (Identifier or DotExpression).
	 * This is useful for building the 'currentNamespaceFqn' string.
	 */
	private String getExpressionFQN(Expression expr)
	{
		if(expr instanceof IdentifierExpression)
		{
			return ((IdentifierExpression) expr).getName().getLexeme();
		}
		else if(expr instanceof DotExpression)
		{
			DotExpression dotExpr = (DotExpression) expr;
			return getExpressionFQN(dotExpr.getLeft()) + "." + dotExpr.getMemberName().getLexeme();
		}
		return "";
	}


	/**
	 * Helper to synchronize parsing within a namespace body in case of an error.
	 * Skips tokens until a known class declaration start or closing brace.
	 */
	private void synchronizeNamespaceBody()
	{
		advance();

		while(!isAtEnd())
		{
			TokenType type = peek().getType();
			switch(type)
			{
				case CLASS:
				case RIGHT_BRACE:
				case PUBLIC:
				case PRIVATE:
				case STATIC:
					return;
				default:
					advance();
			}
		}
	}

	/**
	 * Parses a class declaration, including its name, optional superclass, and members.
	 * Grammar: (MODIFIERS)* 'class' IDENTIFIER ('extends' IDENTIFIER)? '{' (MEMBERS)* '}'
	 *
	 * @param modifiers           The list of modifiers (public, private, static, const) for the class.
	 * @param containingNamespace The fully qualified name of the namespace this class is declared within.
	 * @return A ClassDeclaration AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private ClassDeclaration classDeclaration(List<Token> modifiers, String containingNamespace) throws SyntaxError
	{
		Token classKeyword = consume(TokenType.CLASS, "Expected 'class' keyword.");
		Token className = consume(TokenType.IDENTIFIER, "Expected class name.");

		Token extendsKeyword = null;
		Token superClassName = null;
		if(check(TokenType.EXTENDS))
		{
			extendsKeyword = advance();
			superClassName = consume(TokenType.IDENTIFIER, "Expected superclass name after 'extends'.");
		}

		Token leftBrace = consume(TokenType.LEFT_BRACE, "Expected '{' after class name.");

		List<FieldDeclaration> fields = new ArrayList<>();
		List<MethodDeclaration> methods = new ArrayList<>();
		List<ConstructorDeclaration> constructors = new ArrayList<>();

		while(!check(TokenType.RIGHT_BRACE) && !isAtEnd())
		{
			List<Token> memberModifiers = new ArrayList<>();
			while(check(TokenType.PUBLIC) || check(TokenType.PRIVATE) ||
					check(TokenType.STATIC) || check(TokenType.CONST))
			{
				memberModifiers.add(advance());
			}

			Token nextTokenAfterModifiers = peek();

			if(nextTokenAfterModifiers.getType() == TokenType.IDENTIFIER &&
					nextTokenAfterModifiers.getLexeme().equals(className.getLexeme()) &&
					check(1, TokenType.LEFT_PAREN))
			{
				constructors.add(constructorDeclaration(memberModifiers, className));
			}
			else if(isTypeToken(nextTokenAfterModifiers.getType()) &&
					check(1, TokenType.OPERATOR) &&
					isOperatorToken(peek(2).getType()))
			{
				methods.add(methodDeclaration(memberModifiers, true));
			}
			else if(isTypeToken(nextTokenAfterModifiers.getType()) &&
					check(1, TokenType.IDENTIFIER) &&
					check(2, TokenType.LEFT_PAREN))
			{
				methods.add(methodDeclaration(memberModifiers, false));
			}
			else if((isTypeToken(nextTokenAfterModifiers.getType()) || nextTokenAfterModifiers.getType() == TokenType.VAR) &&
					check(1, TokenType.IDENTIFIER) &&
					!check(2, TokenType.LEFT_PAREN))
			{
				fields.add(fieldDeclaration(memberModifiers));
			}
			else
			{
				error(peek(), "Expected field, method, constructor, or operator declaration inside class.");
				synchronizeClassBody();
			}
		}

		Token rightBrace = consume(TokenType.RIGHT_BRACE, "Expected '}' after class body.");

		return new ClassDeclaration(modifiers, classKeyword, className, extendsKeyword, superClassName,
				leftBrace, fields, methods, constructors, rightBrace, containingNamespace);
	}

	/**
	 * Helper to synchronize parsing within a class body in case of an error.
	 * Skips tokens until a known class member start or closing brace.
	 */
	private void synchronizeClassBody()
	{
		advance();

		while(!isAtEnd())
		{
			TokenType type = peek().getType();
			switch(type)
			{
				case PUBLIC:
				case PRIVATE:
				case STATIC:
				case CONST:
				case CLASS:
				case RIGHT_BRACE:
				case OPERATOR:
					return;
				default:
					if(isTypeToken(type) || type == TokenType.VAR)
					{
						if(check(1, TokenType.IDENTIFIER) || (check(1, TokenType.OPERATOR) && isOperatorToken(peek(2).getType())))
						{
							return;
						}
					}
					advance();
			}
		}
	}

	/**
	 * Parses a constructor declaration.
	 * Grammar: `(MODIFIERS)* CLASS_NAME ( (TYPE IDENTIFIER)* ) BLOCK_STATEMENT`
	 *
	 * @param modifiers      List of modifier tokens (e.g., public, private) already consumed.
	 * @param classNameToken The token representing the name of the class (for constructor name validation).
	 * @return A ConstructorDeclaration AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private ConstructorDeclaration constructorDeclaration(List<Token> modifiers, Token classNameToken) throws SyntaxError
	{
		Token constructorName = consume(TokenType.IDENTIFIER, "Expected constructor name (must match class name).");

		if(!constructorName.getLexeme().equals(classNameToken.getLexeme()))
		{
			errorReporter.report(constructorName.getLine(), constructorName.getColumn(),
					"Constructor name '" + constructorName.getLexeme() + "' does not match class name '" + classNameToken.getLexeme() + "'.");
			throw new SyntaxError();
		}

		consume(TokenType.LEFT_PAREN, "Expected '(' after constructor name.");

		List<Token> parameters = new ArrayList<>();
		if(!check(TokenType.RIGHT_PAREN))
		{
			do
			{
				parameters.add(consume(new TokenType[]{
						TokenType.INT, TokenType.STRING_KEYWORD, TokenType.BOOL, TokenType.FLOAT,
						TokenType.DOUBLE, TokenType.BYTE, TokenType.CHAR, TokenType.IDENTIFIER
				}, "Expected parameter type."));
				parameters.add(consume(TokenType.IDENTIFIER, "Expected parameter name."));
			}
			while(match(TokenType.COMMA));
		}

		consume(TokenType.RIGHT_PAREN, "Expected ')' after parameters.");

		BlockStatement body = blockStatement();
		return new ConstructorDeclaration(modifiers, constructorName, parameters, body);
	}


	/**
	 * Parses a method declaration.
	 * Grammar: (MODIFIERS)* TYPE (IDENTIFIER | 'operator' OPERATOR_TOKEN) '(' (PARAM_LIST)* ')' (BLOCK_STATEMENT | ';')
	 *
	 * @param modifiers          The list of modifiers for the method.
	 * @param isOperatorOverload True if this method is an operator overload declaration.
	 * @return A MethodDeclaration AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private MethodDeclaration methodDeclaration(List<Token> modifiers, boolean isOperatorOverload) throws SyntaxError
	{
		Token returnType = advance();
		Token methodName;
		Token operatorKeyword = null;

		if(isOperatorOverload)
		{
			operatorKeyword = consume(TokenType.OPERATOR, "Expected 'operator' keyword for operator overloading.");
			methodName = advance();
			if(!isOperatorToken(methodName.getType()))
			{
				throw error(methodName, "Expected an operator symbol after 'operator' keyword.");
			}
		}
		else
		{
			methodName = consume(TokenType.IDENTIFIER, "Expected method name.");
		}


		consume(TokenType.LEFT_PAREN, "Expected '(' after method name or operator.");

		List<Token> parameters = new ArrayList<>();
		if(!check(TokenType.RIGHT_PAREN))
		{
			do
			{
				Token paramType = advance();
				if(!isTypeToken(paramType.getType()))
				{
					throw error(paramType, "Expected parameter type.");
				}
				Token paramName = consume(TokenType.IDENTIFIER, "Expected parameter name.");
				parameters.add(paramType);
				parameters.add(paramName);
			}
			while(match(TokenType.COMMA));
		}

		consume(TokenType.RIGHT_PAREN, "Expected ')' after method parameters.");

		BlockStatement body = null;
		Token semicolon = null;
		if(check(TokenType.LEFT_BRACE))
		{
			body = blockStatement();
		}
		else
		{
			semicolon = consume(TokenType.SEMICOLON, "Expected method body or ';' after method declaration.");
		}

		return new MethodDeclaration(modifiers, returnType, methodName, parameters, body, semicolon, operatorKeyword);
	}

	/**
	 * Helper to check if a TokenType represents an operator that can be overloaded.
	 * This will expand as more operators are supported for overloading.
	 */
	private boolean isOperatorToken(TokenType type)
	{
		switch(type)
		{
			case PLUS:
			case MINUS:
			case STAR:
			case SLASH:
			case MODULO:
			case EQUAL_EQUAL:
			case BANG_EQUAL:
			case LESS:
			case GREATER:
			case LESS_EQUAL:
			case GREATER_EQUAL:
				return true;
			default:
				return false;
		}
	}

	/**
	 * Parses a field declaration.
	 * Grammar: `(MODIFIERS)* TYPE IDENTIFIER ('=' EXPRESSION)? ;`
	 *
	 * @param modifiers List of modifier tokens for the field.
	 * @return A FieldDeclaration AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private FieldDeclaration fieldDeclaration(List<Token> modifiers) throws SyntaxError
	{
		Token type = advance();
		Token name = consume(TokenType.IDENTIFIER, "Expected field name.");

		Expression initializer = null;
		if(match(TokenType.ASSIGN))
		{
			initializer = expression();
		}

		consume(TokenType.SEMICOLON, "Expected ';' after field declaration.");
		return new FieldDeclaration(modifiers, type, name, initializer);
	}


	/**
	 * Parses a block of statements enclosed in curly braces.
	 * Grammar: `{ STATEMENT* }`
	 *
	 * @return A BlockStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private BlockStatement blockStatement() throws SyntaxError
	{
		consume(TokenType.LEFT_BRACE, "Expected '{' before block statement.");
		List<Statement> statements = new ArrayList<>();

		while(!check(TokenType.RIGHT_BRACE) && !isAtEnd())
		{
			statements.add(statement());
		}

		consume(TokenType.RIGHT_BRACE, "Expected '}' after block statement.");
		return new BlockStatement(statements);
	}

	/**
	 * Parses a single statement.
	 * This method acts as a dispatcher for different statement types.
	 *
	 * @return A Statement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private Statement statement() throws SyntaxError
	{
		if(check(TokenType.LEFT_BRACE))
		{
			return blockStatement();
		}

		List<Token> modifiers = new ArrayList<>();
		while(check(TokenType.CONST) || check(TokenType.PUBLIC) || check(TokenType.PRIVATE) || check(TokenType.STATIC))
		{
			modifiers.add(advance());
		}

		if(isTypeToken(peek().getType()))
		{
			int lookahead = 1;
			while(check(lookahead, TokenType.LEFT_BRACKET))
			{
				lookahead += 2; // Skip past a "[]"
			}
			if(check(lookahead, TokenType.IDENTIFIER))
			{
				return variableDeclarationStatement(modifiers);
			}
		}

		if(check(TokenType.THIS) && check(1, TokenType.LEFT_PAREN))
		{
			return constructorChainingCallStatement();
		}
		if(match(TokenType.IF))
		{
			return ifStatement();
		}
		if(match(TokenType.WHILE))
		{
			return whileStatement();
		}
		if(match(TokenType.FOR))
		{
			return forStatement();
		}
		if(match(TokenType.RETURN))
		{
			return returnStatement();
		}
		if(match(TokenType.SWITCH))
		{
			return switchStatement();
		}

		if(!modifiers.isEmpty())
		{
			errorReporter.report(peek().getLine(), peek().getColumn(), "Modifiers are only allowed on declarations (variables, fields, methods).");
		}

		return expressionStatement();
	}

	/**
	 * Parses a constructor chaining call (e.g., `this(...)`).
	 * This is a special type of statement that can only appear as the first statement in a constructor.
	 * Grammar: `THIS '(' (EXPRESSION (',' EXPRESSION)*)? ')' ;`
	 *
	 * @return A ConstructorChainingCallStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private ConstructorChainingCallStatement constructorChainingCallStatement() throws SyntaxError
	{
		Token thisKeyword = consume(TokenType.THIS, "Expected 'this' keyword for constructor call.");
		Token leftParen = consume(TokenType.LEFT_PAREN, "Expected '(' after 'this' for constructor call.");

		List<Expression> arguments = new ArrayList<>();
		if(!check(TokenType.RIGHT_PAREN))
		{
			do
			{
				arguments.add(expression());
			}
			while(match(TokenType.COMMA));
		}
		Token rightParen = consume(TokenType.RIGHT_PAREN, "Expected ')' after constructor call arguments.");
		consume(TokenType.SEMICOLON, "Expected ';' after constructor call.");

		return new ConstructorChainingCallStatement(thisKeyword, leftParen, arguments, rightParen);
	}

	/**
	 * Parses an if statement.
	 * Grammar: `IF '(' EXPRESSION ')' (BLOCK_STATEMENT | STATEMENT) ('else' (BLOCK_STATEMENT | STATEMENT))?`
	 *
	 * @return An IfStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private IfStatement ifStatement() throws SyntaxError
	{
		Token ifKeyword = previous();

		consume(TokenType.LEFT_PAREN, "Expected '(' after 'if'.");
		Expression condition = expression();
		consume(TokenType.RIGHT_PAREN, "Expected ')' after if condition.");

		Statement thenBranch;
		if(check(TokenType.LEFT_BRACE))
		{
			thenBranch = blockStatement();
		}
		else
		{
			thenBranch = statement();
		}

		Statement elseBranch = null;
		if(match(TokenType.ELSE))
		{
			if(check(TokenType.LEFT_BRACE))
			{
				elseBranch = blockStatement();
			}
			else
			{
				elseBranch = statement();
			}
		}

		return new IfStatement(ifKeyword, condition, thenBranch, elseBranch);
	}

	/**
	 * Parses a while loop statement.
	 * Grammar: `WHILE ( EXPRESSION ) BLOCK_STATEMENT`
	 *
	 * @return A WhileStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private WhileStatement whileStatement() throws SyntaxError
	{
		consume(TokenType.LEFT_PAREN, "Expected '(' after 'while'.");
		Expression condition = expression();
		consume(TokenType.RIGHT_PAREN, "Expected ')' after while condition.");

		BlockStatement body = blockStatement();
		return new WhileStatement(condition, body);
	}

	/**
	 * Parses a for loop statement.
	 * Grammar: `FOR ( (VAR_DECL | EXPR_STMT | ;) EXPR_STMT ; (EXPRESSION)? ) BLOCK_STATEMENT`
	 * A more robust for loop would parse:
	 * `for ( (variableDeclaration | expressionStatement | ';') expression? ';' expression? ) statement`
	 * For simplicity, this will parse: `for ( VAR_DECL_OR_EXPR ; CONDITION ; INCREMENT ) BLOCK_STATEMENT`
	 *
	 * @return A ForStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private ForStatement forStatement() throws SyntaxError
	{
		consume(TokenType.LEFT_PAREN, "Expected '(' after 'for'.");

		Statement initializer = null;
		if(match(TokenType.SEMICOLON))
		{
			// No initializer
		}
		else if(isTypeToken(peek().getType()))
		{
			// **FIXED**: Call variableDeclarationStatement with only modifiers
			initializer = variableDeclarationStatement(new ArrayList<>());
		}
		else
		{
			initializer = expressionStatement();
		}

		Expression condition = null;
		if(!check(TokenType.SEMICOLON))
		{
			condition = expression();
		}
		consume(TokenType.SEMICOLON, "Expected ';' after for loop condition.");

		Expression increment = null;
		if(!check(TokenType.RIGHT_PAREN))
		{
			increment = expression();
		}
		consume(TokenType.RIGHT_PAREN, "Expected ')' after for loop clauses.");

		BlockStatement body = blockStatement();
		return new ForStatement(initializer, condition, increment, body);
	}


	/**
	 * Parses a return statement.
	 * Grammar: `RETURN (EXPRESSION)? ;`
	 *
	 * @return A ReturnStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private ReturnStatement returnStatement() throws SyntaxError
	{
		Token returnKeyword = previous();
		Expression value = null;
		if(!check(TokenType.SEMICOLON))
		{
			value = expression();
		}
		consume(TokenType.SEMICOLON, "Expected ';' after return statement.");
		return new ReturnStatement(returnKeyword, value);
	}

	/**
	 * Parses a variable declaration statement. This is the entry point
	 * from `statement()` when modifiers are parsed *before* the type token.
	 * <p>
	 * Grammar: `(MODIFIERS)* (TYPE | VAR) IDENTIFIER (= EXPRESSION)? ;`
	 *
	 * @return A VariableDeclarationStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private VariableDeclarationStatement variableDeclarationStatement(List<Token> modifiers) throws SyntaxError
	{
		TypeSpecifier typeSpec = parseTypeSpecifier();
		Token name = consume(TokenType.IDENTIFIER, "Expected variable name.");
		Expression initializer = null;
		if(match(TokenType.ASSIGN))
		{
			initializer = expression();
		}
		consume(TokenType.SEMICOLON, "Expected ';' after variable declaration.");
		return new VariableDeclarationStatement(modifiers, typeSpec.baseType(), typeSpec.rank(), name, initializer);
	}

	/**
	 * Parses an expression statement.
	 * Grammar: `EXPRESSION ;`
	 *
	 * @return An ExpressionStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private ExpressionStatement expressionStatement() throws SyntaxError
	{
		Expression expr = expression();
		consume(TokenType.SEMICOLON, "Expected ';' after expression.");
		return new ExpressionStatement(expr);
	}

	/**
	 * Parses an expression based on operator precedence.
	 * This is a chain of methods, each handling a lower precedence level.
	 * Order of precedence (highest to lowest, then left-associativity):
	 * Assignment -> Logical OR -> Logical AND -> Equality -> Is -> Comparison -> Additive -> Multiplicative -> Unary -> Call/Member Access/Postfix -> Primary
	 *
	 * @return The parsed Expression AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private Expression expression() throws SyntaxError
	{
		return assignment();
	}

	private Expression assignment() throws SyntaxError
	{
		Expression expr = or();

		if(match(TokenType.ASSIGN, TokenType.PLUS_ASSIGN, TokenType.MINUS_ASSIGN, TokenType.STAR_ASSIGN, TokenType.SLASH_ASSIGN, TokenType.MODULO_ASSIGN))
		{
			Token operator = previous();
			Expression value = assignment();

			if(expr instanceof IdentifierExpression || expr instanceof DotExpression || expr instanceof ArrayAccessExpression)
			{
				return new AssignmentExpression(expr, operator, value);
			}
			else
			{
				errorReporter.report(operator.getLine(), operator.getColumn(), "Invalid assignment target.");
				throw new SyntaxError();
			}
		}
		return expr;
	}

	private Expression or() throws SyntaxError
	{
		Expression expr = and();

		while(match(TokenType.PIPE_PIPE))
		{
			Token operator = previous();
			Expression right = and();
			expr = new BinaryExpression(expr, operator, right);
		}
		return expr;
	}

	private Expression and() throws SyntaxError
	{
		Expression expr = equality();

		while(match(TokenType.AMPERSAND_AMPERSAND))
		{
			Token operator = previous();
			Expression right = equality();
			expr = new BinaryExpression(expr, operator, right);
		}
		return expr;
	}

	private Expression equality() throws SyntaxError
	{
		Expression expr = isExpression();

		while(match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL))
		{
			Token operator = previous();
			Expression right = isExpression();
			expr = new BinaryExpression(expr, operator, right);
		}
		return expr;
	}

	private Expression isExpression() throws SyntaxError
	{
		Expression expr = comparison();

		if(match(TokenType.IS))
		{
			Token isKeyword = previous();
			Token typeToken = consume(new TokenType[]{
					TokenType.INT, TokenType.BOOL, TokenType.CHAR, TokenType.BYTE,
					TokenType.FLOAT, TokenType.DOUBLE, TokenType.STRING_KEYWORD,
					TokenType.IDENTIFIER
			}, "Expected a valid type name (e.g., 'int', 'string', or a class name) after 'is' operator.");
			return new IsExpression(expr, isKeyword, typeToken);
		}
		return expr;
	}

	private Expression comparison() throws SyntaxError
	{
		Expression expr = additive();

		while(match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL))
		{
			Token operator = previous();
			Expression right = additive();
			expr = new BinaryExpression(expr, operator, right);
		}
		return expr;
	}

	private Expression additive() throws SyntaxError
	{
		Expression expr = multiplicative();

		while(match(TokenType.MINUS, TokenType.PLUS))
		{
			Token operator = previous();
			Expression right = multiplicative();
			expr = new BinaryExpression(expr, operator, right);
		}
		return expr;
	}

	private Expression multiplicative() throws SyntaxError
	{
		Expression expr = unary();

		while(match(TokenType.SLASH, TokenType.STAR, TokenType.MODULO))
		{
			Token operator = previous();
			Expression right = unary();
			expr = new BinaryExpression(expr, operator, right);
		}
		return expr;
	}

	/**
	 * Parses unary expressions (e.g., `-x`, `!isTrue`, `++i`, `--j`).
	 * Grammar: `(UNARY_OPERATOR)* CALL`
	 *
	 * @return A UnaryExpression AST node or a higher precedence expression.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private Expression unary() throws SyntaxError
	{
		if(match(TokenType.BANG, TokenType.MINUS, TokenType.PLUS_PLUS, TokenType.MINUS_MINUS))
		{
			Token operator = previous();
			Expression right = unary();
			return new UnaryExpression(operator, right);
		}
		return call();
	}

	/**
	 * Handles method calls, member access (dot operator), and postfix unary operators.
	 * This method is left-associative for chaining.
	 * Grammar: `PRIMARY ( ( . IDENTIFIER ) | ( ( ARGUMENTS ) ) | ( ++ | -- ) | ( [ EXPRESSION ] ) )*`
	 *
	 * @return The parsed Expression AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private Expression call() throws SyntaxError
	{
		Expression expr = primary();

		while(true)
		{
			if(match(TokenType.DOT))
			{
				Token dot = previous();
				Token memberName = consume(TokenType.IDENTIFIER, "Expected member name after '.'.");
				expr = new DotExpression(expr, dot, memberName);
			}
			else if(check(TokenType.LEFT_PAREN))
			{
				Token paren = advance();
				List<Expression> arguments = new ArrayList<>();
				if(!check(TokenType.RIGHT_PAREN))
				{
					do
					{
						arguments.add(expression());
					}
					while(match(TokenType.COMMA));
				}
				consume(TokenType.RIGHT_PAREN, "Expected ')' after arguments.");
				expr = new CallExpression(expr, paren, arguments);
			}
			else if(match(TokenType.LEFT_BRACKET))
			{
				Token leftBracket = previous();
				Expression index = expression();
				Token rightBracket = consume(TokenType.RIGHT_BRACKET, "Expected ']' after index expression.");
				expr = new ArrayAccessExpression(expr, index, rightBracket);
			}
			else if(match(TokenType.PLUS_PLUS, TokenType.MINUS_MINUS))
			{
				Token operator = previous();
				if(expr instanceof IdentifierExpression || expr instanceof DotExpression || expr instanceof ArrayAccessExpression)
				{
					expr = new PostfixUnaryExpression(expr, operator);
				}
				else
				{
					errorReporter.report(operator.getLine(), operator.getColumn(), "Invalid target for postfix increment/decrement operator. Must be a variable, property, or array element.");
					throw new SyntaxError();
				}
			}
			else
			{
				break;
			}
		}
		return expr;
	}

	/**
	 * Parses the most basic expressions: literals, identifiers, 'this', parenthesized expressions, and 'new' expressions.
	 * Grammar: `LITERAL | IDENTIFIER | THIS | ( EXPRESSION ) | NEW_EXPRESSION | ARRAY_INITIALIZER`
	 *
	 * @return The parsed primary Expression AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private Expression primary() throws SyntaxError
	{
		if(match(TokenType.LEFT_BRACE))
		{
			Token leftBrace = previous();  // already consumed
			return arrayInitializer(leftBrace);
		}
		if(match(TokenType.INTEGER_LITERAL, TokenType.STRING_LITERAL, TokenType.CHAR_LITERAL, TokenType.BOOLEAN_LITERAL, TokenType.FLOAT_LITERAL, TokenType.DOUBLE_LITERAL, TokenType.NULL))
		{
			return new LiteralExpression(previous().getLiteral(), previous());
		}
		if(match(TokenType.IDENTIFIER))
		{
			return new IdentifierExpression(previous());
		}
		if(match(TokenType.THIS))
		{
			return new ThisExpression(previous());
		}
		if(match(TokenType.NEW))
		{
			Token newKeyword = previous();
			// For 'new' expressions, we'll parse the base type first.
			// Array creation (e.g., `new int[5]`) will be handled by looking for `[` directly after the type.
			// Object creation (e.g., `new MyClass()`) will be handled by looking for `(` directly after the type.

			// This *advances* the base type (e.g., 'float' in 'new float[1]')
			Token baseTypeToken = advance();

			if(baseTypeToken == null || !isTypeToken(baseTypeToken.getType()))
			{
				throw error(peek(), "Expected a type name after 'new' keyword.");
			}

			int arrayRankForNew = 0; // This will store the explicit array dimension for 'new Type[size]'

			// Check if it's an array creation like `new float[...]`
			if(check(TokenType.LEFT_BRACKET))
			{
				Token leftBracket = consume(TokenType.LEFT_BRACKET, "Expected '[' for array size in 'new' expression.");
				Expression sizeExpr = expression();
				Token rightBracket = consume(TokenType.RIGHT_BRACKET, "Expected ']' after array size in 'new' expression.");
				arrayRankForNew = 1; // Mark as a 1-dimensional array creation for now

				// You'll need to extend this for multi-dimensional array creation like `new int[5][2]`
				// while(check(TokenType.LEFT_BRACKET) && check(1, TokenType.RIGHT_BRACKET)) {
				//     consume(TokenType.LEFT_BRACKET, "Expected '[' for additional array dimension.");
				//     consume(TokenType.RIGHT_BRACKET, "Expected ']' for additional array dimension.");
				//     arrayRankForNew++;
				// }

				return new ArrayCreationExpression(newKeyword, baseTypeToken, arrayRankForNew, sizeExpr);
			}
			else if(match(TokenType.LEFT_PAREN))
			{ // new MyClass(...)
				Token paren = previous(); // This is the '(' token
				List<Expression> arguments = new ArrayList<>();
				if(!check(TokenType.RIGHT_PAREN))
				{
					do
					{
						arguments.add(expression());
					}
					while(match(TokenType.COMMA));
				}
				Token rightParen = consume(TokenType.RIGHT_PAREN, "Expected ')' after constructor's arguments.");
				Expression classNameExpr = new IdentifierExpression(baseTypeToken); // baseTypeToken is the class name
				return new NewExpression(newKeyword, classNameExpr, paren, arguments);
			}
			else
			{
				throw error(peek(), "Expected '(' for constructor call or '[' for array creation after 'new' type.");
			}
		}
		if(match(TokenType.LEFT_PAREN))
		{
			Token leftParen = previous();
			Expression expr = expression();
			consume(TokenType.RIGHT_PAREN, "Expected ')' after expression.");
			return new GroupingExpression(leftParen, expr);
		}

		throw error(peek(), "Expected expression.");
	}

	/**
	 * Consumes the current token if its type matches any of the given types.
	 *
	 * @param types The TokenType(s) to match against.
	 * @return True if a match was found and the token was consumed, false otherwise.
	 */
	private boolean match(TokenType... types)
	{
		for(TokenType type : types)
		{
			if(check(type))
			{
				advance();
				return true;
			}
		}
		return false;
	}

	/**
	 * Checks if the current token is of the expected type without consuming it.
	 * If not, it reports a syntax error and throws a SyntaxError.
	 *
	 * @param type    The expected TokenType.
	 * @param message The error message to report if the type doesn't match.
	 * @return The consumed Token if it matches the expected type.
	 * @throws SyntaxError if the current token's type does not match the expected type.
	 */
	private Token consume(TokenType type, String message) throws SyntaxError
	{
		if(check(type))
		{
			return advance();
		}
		throw error(peek(), message);
	}

	/**
	 * Overloaded consume method to check for multiple expected types.
	 *
	 * @param types   The expected TokenTypes.
	 * @param message The error message to report if the type doesn't match.
	 * @return The consumed Token if it matches one of the expected types.
	 * @throws SyntaxError if the current token's type does not match any of the expected types.
	 */
	private Token consume(TokenType[] types, String message) throws SyntaxError
	{
		for(TokenType type : types)
		{
			if(check(type))
			{
				return advance();
			}
		}
		throw error(peek(), message);
	}

	/**
	 * Checks if the current token's type matches any of the given types.
	 *
	 * @param types The TokenType(s) to check against.
	 * @return True if the current token matches any of the types, false otherwise.
	 */
	private boolean check(TokenType... types)
	{
		if(isAtEnd())
			return false;
		TokenType currentType = peek().getType();
		for(TokenType type : types)
		{
			if(currentType == type)
			{
				return true;
			}
		}
		return false;
	}

	/**
	 * Helper method to check a token type at a given offset from the current position.
	 *
	 * @param offset The offset from the current token (0 for current, 1 for next, etc.)
	 * @param type   The TokenType to check for.
	 * @return True if the token at the offset exists and matches the type, false otherwise.
	 */
	private boolean check(int offset, TokenType type)
	{
		if(current + offset >= tokens.size())
		{
			return false;
		}
		Token tokenAtOffset = tokens.get(current + offset);
		boolean matches = tokenAtOffset.getType() == type;
		return matches;
	}


	/**
	 * Helper method to determine if a TokenType represents a valid type (e.g., int, String, custom class, or 'var').
	 *
	 * @param type The TokenType to check.
	 * @return True if the type is a valid data type (including 'var'), false otherwise.
	 */
	private boolean isTypeToken(TokenType type)
	{
		return type == TokenType.VOID || type == TokenType.STRING_KEYWORD || type == TokenType.INT ||
				type == TokenType.BOOL || type == TokenType.FLOAT || type == TokenType.DOUBLE ||
				type == TokenType.BYTE || type == TokenType.CHAR || type == TokenType.IDENTIFIER || type == TokenType.VAR;
	}

	/**
	 * Consumes the current token and returns it.
	 *
	 * @return The consumed Token.
	 */
	private Token advance()
	{
		if(!isAtEnd())
			current++;
		return previous();
	}

	/**
	 * Looks at the token at a given offset from the current position without consuming it.
	 *
	 * @param offset The offset from the current token (0 for current, 1 for next, etc.).
	 * @return The Token at the specified offset, or EOF if past the end of the token list.
	 */
	private Token peek(int offset)
	{
		if(current + offset >= tokens.size())
		{
			return tokens.get(tokens.size() - 1);
		}
		return tokens.get(current + offset);
	}

	/**
	 * Looks at the current token without consuming it.
	 * (Delegates to peek(0))
	 *
	 * @return The current Token.
	 */
	private Token peek()
	{
		return peek(0);
	}

	/**
	 * Looks at the previous token (the one just consumed).
	 *
	 * @return The previous Token.
	 */
	private Token previous()
	{
		return tokens.get(current - 1);
	}

	/**
	 * Checks if the parser has reached the end of the token stream.
	 *
	 * @return True if at the end, false otherwise.
	 */
	private boolean isAtEnd()
	{
		return peek().getType() == TokenType.EOF;
	}

	/**
	 * Reports a parsing error and creates a SyntaxError.
	 *
	 * @param token   The token where the error occurred.
	 * @param message The error message.
	 * @return A new SyntaxError instance.
	 */
	private SyntaxError error(Token token, String message)
	{
		errorReporter.report(token.getLine(), token.getColumn(), message);
		return new SyntaxError();
	}

	/**
	 * Attempts to synchronize the parser after an error to continue parsing.
	 * This method skips tokens until it finds a likely synchronization point
	 * (e.g., a semicolon, or the start of a new declaration/statement keyword).
	 */
	private void synchronize()
	{
		advance();

		while(!isAtEnd())
		{
			if(previous().getType() == TokenType.SEMICOLON)
				return;
			if(peek().getType() == TokenType.RIGHT_BRACE)
			{
				advance();
				return;
			}

			switch(peek().getType())
			{
				case CLASS:
				case NAMESPACE:
				case PUBLIC:
				case PRIVATE:
				case STATIC:
				case IF:
				case FOR:
				case WHILE:
				case RETURN:
				case IMPORT:
				case GLOBAL:
				case SWITCH:
				case CASE:
				case DEFAULT:
					return;
				default:
			}
			advance();
		}
	}

	/**
	 * Custom exception for handling parsing errors.
	 * This is an unchecked exception, used internally by the parser
	 * to unwind the stack when a syntax error is found.
	 */
	private static class SyntaxError extends RuntimeException
	{
		// No special fields or constructors needed for this basic error type
	}

	/**
	 * Parses a switch statement.
	 * Grammar: `SWITCH ( EXPRESSION ) { ( CASE CONSTANT_EXPRESSION : STATEMENT* )* ( DEFAULT : STATEMENT* )? }`
	 *
	 * @return A SwitchStatement AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private SwitchStatement switchStatement() throws SyntaxError
	{
		Token switchKeyword = previous();
		consume(TokenType.LEFT_PAREN, "Expected '(' after 'switch'.");
		Expression switchExpr = expression();
		consume(TokenType.RIGHT_PAREN, "Expected ')' after switch expression.");
		consume(TokenType.LEFT_BRACE, "Expected '{' for switch body.");

		List<SwitchCase> cases = new ArrayList<>();
		BlockStatement defaultBlock = null;

		while(!check(TokenType.RIGHT_BRACE) && !isAtEnd())
		{
			if(match(TokenType.CASE))
			{
				cases.add(switchCase());
			}
			else if(match(TokenType.DEFAULT))
			{
				if(defaultBlock != null)
				{
					error(previous(), "A 'default' clause is already defined in this switch statement.");
				}
				Token defaultKeyword = previous();
				consume(TokenType.COLON, "Expected ':' after 'default' keyword.");
				List<Statement> defaultBodyStatements = new ArrayList<>();
				while(!check(TokenType.CASE) && !check(TokenType.DEFAULT) && !check(TokenType.RIGHT_BRACE) && !isAtEnd())
				{
					defaultBodyStatements.add(statement());
				}
				defaultBlock = new BlockStatement(defaultBodyStatements);
			}
			else
			{
				error(peek(), "Expected 'case' or 'default' keyword within a switch statement.");
				synchronize();
			}
		}
		consume(TokenType.RIGHT_BRACE, "Expected '}' to close the switch statement.");
		return new SwitchStatement(switchKeyword, switchExpr, cases, defaultBlock);
	}

	/**
	 * Parses a single 'case' clause within a switch statement.
	 * Grammar: `CASE CONSTANT_EXPRESSION : STATEMENT*`
	 *
	 * @return A SwitchCase AST node.
	 * @throws SyntaxError if a syntax error occurs.
	 */
	private SwitchCase switchCase() throws SyntaxError
	{
		Token caseKeyword = previous();
		Expression caseValue = expression();
		consume(TokenType.COLON, "Expected ':' after 'case' value.");

		List<Statement> caseBodyStatements = new ArrayList<>();
		while(!check(TokenType.CASE) && !check(TokenType.DEFAULT) && !check(TokenType.RIGHT_BRACE) && !isAtEnd())
		{
			caseBodyStatements.add(statement());
		}
		return new SwitchCase(caseKeyword, caseValue, caseBodyStatements);
	}

	/**
	 * Parses an array initializer expression, e.g., {1, 2, 3}
	 *
	 * @return An ArrayInitializerExpression node.
	 * @throws SyntaxError if the syntax is incorrect.
	 */
	private Expression arrayInitializer(Token leftBrace) throws SyntaxError
	{
		List<Expression> elements = new ArrayList<>();
		if(!check(TokenType.RIGHT_BRACE))
		{
			do
			{
				elements.add(expression());
			}
			while(match(TokenType.COMMA));
		}
		consume(TokenType.RIGHT_BRACE, "Expected '}' to end array initializer.");
		return new ArrayInitializerExpression(leftBrace, elements);
	}

	/**
	 * Parses a type specifier. For declarations (e.g., int[] var;), this will consume array brackets.
	 * For 'new' expressions where the brackets enclose a size (e.g., new int[5]), this should
	 * only parse the base type, and the array brackets for the size will be parsed separately.
	 *
	 * @return A TypeSpecifier record containing the base type token and the array rank.
	 */
	private TypeSpecifier parseTypeSpecifier() throws SyntaxError
	{
		if(!isTypeToken(peek().getType()))
		{
			throw error(peek(), "Expected a type name.");
		}
		Token baseType = advance(); // Consume the base type (e.g., int, float, MyClass)

		int rank = 0;
		// This loop is primarily for declarations like `int[] myArr;` or `MyClass[][] anotherArr;`
		// For `new MyType[size]`, the brackets for `[size]` are handled separately in primary().
		while(check(TokenType.LEFT_BRACKET)) // Use check, not match, to decide if it's a type array specifier
		{
			// Peek ahead to ensure it's a closing bracket, not an expression bracket.
			// This is a heuristic and might need refinement for complex cases.
			if(check(1, TokenType.RIGHT_BRACKET))
			{
				advance(); // Consume LEFT_BRACKET
				advance(); // Consume RIGHT_BRACKET
				rank++;
			}
			else
			{
				break; // It's not a type array specifier like `[]`, but possibly `[expression]`
			}
		}
		return new TypeSpecifier(baseType, rank);
	}
}