// File: src/main/java/com/juanpa.nebula.transpiler/ast/declarations/MethodDeclaration.java

package com.juanpa.nebula.transpiler.ast.declarations;

import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.statements.BlockStatement;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType; // Needed for TokenType reference
import com.juanpa.nebula.transpiler.semantics.MethodSymbol; // Import MethodSymbol

import java.util.ArrayList;
import java.util.Collections; // Needed for unmodifiableList
import java.util.List;
import java.util.stream.Collectors;

/**
 * AST node representing a method declaration within a class.
 * This includes the method's name (or operator symbol), return type, parameters, and its body (a block statement or semicolon).
 */
public class MethodDeclaration implements ASTNode
{
	private final List<Token> modifiers; // e.g., PUBLIC, STATIC
	private final Token returnType;      // e.g., VOID, INT, String, IDENTIFIER for custom types
	private final Token name;            // IDENTIFIER token for the method name, OR operator token (e.g., PLUS, EQUAL_EQUAL)
	private final Token operatorKeyword; // The 'operator' keyword token, null if not an overload
	private final List<Token> parameters; // Type, Name, Type, Name...
	private final BlockStatement body;   // The method's code block (null if semicolon present)
	private final Token semicolon;       // The ';' token (null if block body present)

	// NEW: Field for resolved MethodSymbol (to link AST node to semantic info)
	private MethodSymbol resolvedSymbol;

	private final boolean isWrapper; // ADD THIS
	private final Token cppTarget;   // ADD THIS

	// Constructor for regular methods (no operatorKeyword, must have body OR semicolon)
	public MethodDeclaration(List<Token> modifiers, Token returnType, Token name,
	                         List<Token> parameters, BlockStatement body, Token semicolon)
	{
		this(modifiers, returnType, name, parameters, body, semicolon, null, false, null);
	}


	/**
	 * Comprehensive constructor for MethodDeclaration, used by the parser.
	 *
	 * @param modifiers       The list of modifiers for the method.
	 * @param returnType      The return type token.
	 * @param name            The method name identifier token or the operator symbol token.
	 * @param parameters      List of parameter tokens (type, name, type, name...).
	 * @param body            The block statement for the method body (null if semicolon present).
	 * @param semicolon       The semicolon token (null if body present).
	 * @param operatorKeyword The 'operator' keyword token (null for regular methods).
	 */
	public MethodDeclaration(List<Token> modifiers, Token returnType, Token name,
							 List<Token> parameters, BlockStatement body, Token semicolon,
							 Token operatorKeyword, boolean isWrapper, Token cppTarget)
	{
		this.modifiers = new ArrayList<>(modifiers);
		this.returnType = returnType;
		this.name = name;
		this.parameters = new ArrayList<>(parameters);
		this.body = body;
		this.semicolon = semicolon;
		this.operatorKeyword = operatorKeyword;
		this.isWrapper = isWrapper;       // ADD THIS
		this.cppTarget = cppTarget;

		// Basic validation (optional, can be done in semantic analysis too)
		if(body == null && semicolon == null)
		{
			throw new IllegalArgumentException("MethodDeclaration must have either a body or a semicolon.");
		}
		if(body != null && semicolon != null)
		{
			throw new IllegalArgumentException("MethodDeclaration cannot have both a body and a semicolon.");
		}
		// If it's a regular method, name should be IDENTIFIER. If operator, name is operator token.
		if(operatorKeyword == null && name.getType() != TokenType.IDENTIFIER)
		{
			throw new IllegalArgumentException("Regular method name must be an IDENTIFIER token.");
		}
	}

	public List<Token> getModifiers()
	{
		return Collections.unmodifiableList(modifiers);
	}

	public Token getReturnType()
	{
		return returnType;
	}

	public Token getName()
	{
		return name;
	}

	public boolean isWrapper()
	{
		return isWrapper;
	}

	public Token getCppTarget()
	{
		return cppTarget;
	}

	public Token getOperatorKeyword()
	{
		return operatorKeyword;
	}

	public List<Token> getParameters()
	{
		return Collections.unmodifiableList(parameters);
	}

	public BlockStatement getBody()
	{
		return body;
	}

	public Token getSemicolon()
	{
		return semicolon;
	}

	// NEW: Getter for resolved MethodSymbol
	public MethodSymbol getResolvedSymbol()
	{
		return resolvedSymbol;
	}

	// NEW: Setter for resolved MethodSymbol
	public void setResolvedSymbol(MethodSymbol resolvedSymbol)
	{
		this.resolvedSymbol = resolvedSymbol;
	}


	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		String mods = modifiers.isEmpty() ? "" : modifiers.stream()
				.map(Token::getLexeme)
				.collect(Collectors.joining(" ")) + " ";
		sb.append(mods).append(returnType.getLexeme()).append(" ");

		if(operatorKeyword != null)
		{ // Handle operator overloading in toString
			sb.append(operatorKeyword.getLexeme()).append(name.getLexeme()); // e.g., "operator+"
		}
		else
		{
			sb.append(name.getLexeme());
		}

		sb.append("(").append(formatParameters(parameters)).append(")");
		if(body != null)
		{
			sb.append(" {\n");
			sb.append(indent(body.toString(), 1)); // Indent the body
			sb.append("}\n");
		}
		else if(semicolon != null)
		{
			sb.append(";\n");
		}
		return sb.toString();
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitMethodDeclaration(this);
	}

	private String formatParameters(List<Token> params)
	{
		if(params.isEmpty())
			return "";
		StringBuilder sb = new StringBuilder();
		for(int i = 0; i < params.size(); i += 2)
		{ // Assuming pairs of (type, name)
			sb.append(params.get(i).getLexeme()).append(" ").append(params.get(i + 1).getLexeme());
			if(i + 2 < params.size())
			{
				sb.append(", ");
			}
		}
		return sb.toString();
	}

	private String indent(String text, int level)
	{
		StringBuilder indentedText = new StringBuilder();
		String prefix = "  ".repeat(level);
		// Split by newline and append prefix to each line
		for(String line : text.split("\\R"))
		{ // Use \\R for any Unicode newline
			indentedText.append(prefix).append(line).append("\n");
		}
		return indentedText.toString();
	}

	public Token getFirstToken()
	{
		// Return the first modifier if present, otherwise the return type token, or operatorKeyword if present.
		if(!modifiers.isEmpty())
		{
			return modifiers.get(0);
		}
		if(operatorKeyword != null)
		{
			return operatorKeyword; // If it's an operator overload and no modifiers
		}
		return returnType; // Default to return type token
	}
}
