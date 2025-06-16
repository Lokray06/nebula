// File: src/main/java/com/juanpa.nebula.transpiler/semantics/SemanticAnalyzer.java

package com.juanpa.nebula.transpiler.semantics;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.ast.declarations.ClassDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.ConstructorDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.FieldDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.MethodDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.NamespaceDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.ImportDirective;
import com.juanpa.nebula.transpiler.ast.expressions.*;
import com.juanpa.nebula.transpiler.ast.statements.BlockStatement;
import com.juanpa.nebula.transpiler.ast.statements.ConstructorChainingCallStatement;
import com.juanpa.nebula.transpiler.ast.statements.ExpressionStatement;
import com.juanpa.nebula.transpiler.ast.statements.ForStatement;
import com.juanpa.nebula.transpiler.ast.statements.IfStatement;
import com.juanpa.nebula.transpiler.ast.statements.ReturnStatement;
import com.juanpa.nebula.transpiler.ast.statements.Statement;
import com.juanpa.nebula.transpiler.ast.statements.SwitchCase;
import com.juanpa.nebula.transpiler.ast.statements.SwitchStatement;
import com.juanpa.nebula.transpiler.ast.statements.VariableDeclarationStatement;
import com.juanpa.nebula.transpiler.ast.statements.WhileStatement;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType;
import com.juanpa.nebula.transpiler.util.ErrorReporter;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Performs semantic analysis with a two-phase approach:
 * Phase 1: Discover all classes and register them.
 * Phase 2: Define members (fields, methods, constructors) once all classes are known.
 */
public class SemanticAnalyzer implements ASTVisitor<Type>
{
	private final ErrorReporter errorReporter;
	private SymbolTable currentScope;
	private ClassSymbol currentClass;
	private MethodSymbol currentMethod;
	private String currentNamespacePrefix = "";
	private boolean inStaticContext;

	private final Map<String, ClassSymbol> declaredClasses;
	private final List<String> importedNamespaces;
	private final Map<String, ClassSymbol> importedClasses;
	private final Map<String, Symbol> importedStaticMembers;
	private final List<ClassSymbol> importedStaticClasses;
	private MethodSymbol currentConstructor;

	public SemanticAnalyzer(ErrorReporter errorReporter)
	{
		this.errorReporter = errorReporter;
		this.declaredClasses = new LinkedHashMap<>();
		this.importedNamespaces = new ArrayList<>();
		this.importedClasses = new HashMap<>();
		this.importedStaticMembers = new HashMap<>();
		this.importedStaticClasses = new ArrayList<>();
	}

	public boolean analyze(Program program)
	{
		// Setup global scope
		SymbolTable globalScope = new SymbolTable(null, "Global");
		enterScope(globalScope);
		// Predefine primitives
		defineGlobalType(PrimitiveType.VOID);
		defineGlobalType(PrimitiveType.INT);
		defineGlobalType(PrimitiveType.BOOL);
		defineGlobalType(PrimitiveType.FLOAT);
		defineGlobalType(PrimitiveType.DOUBLE);
		defineGlobalType(PrimitiveType.BYTE);
		defineGlobalType(PrimitiveType.CHAR);

		// Phase 1: Class discovery
		System.out.println("--- Semantic Analysis: First Pass - Class Discovery ---");
		for(NamespaceDeclaration nsDecl : program.getNamespaceDeclarations())
		{
			String oldNs = this.currentNamespacePrefix;
			// Build the current namespace prefix string
			String rawNamespaceName = getQualifiedNameFromExpression(nsDecl.getNameExpression());
			if(oldNs.isEmpty())
			{
				this.currentNamespacePrefix = rawNamespaceName;
			}
			else
			{
				this.currentNamespacePrefix = oldNs + "." + rawNamespaceName;
			}

			for(ClassDeclaration cd : nsDecl.getClassDeclarations())
			{
				String simpleName = cd.getName().getLexeme();
				String fullName = currentNamespacePrefix.isEmpty() ? simpleName : currentNamespacePrefix + "." + simpleName;
				ClassSymbol cs = new ClassSymbol(simpleName,
						new ClassType(fullName, null, null),
						cd.getName(), // Token
						new SymbolTable(globalScope, "Class:" + simpleName)); // Class scope parented to global for now
				((ClassType) cs.getType()).classSymbol = cs;
				declaredClasses.put(fullName, cs);
				globalScope.define(cs); // Define class symbol in global scope for lookup

				// CRITICAL FIX: Set the resolved symbol for the ClassDeclaration's name expression.
				// This ensures CppGenerator can retrieve the ClassSymbol directly.
				cd.getNameExpression().setResolvedSymbol(cs);
			}
			this.currentNamespacePrefix = oldNs; // Restore old namespace prefix
		}

		// Phase 2: Member definition
		System.out.println("--- Semantic Analysis: First Pass - Member Definition ---");
		for(NamespaceDeclaration nsDecl : program.getNamespaceDeclarations())
		{
			String oldNs = this.currentNamespacePrefix;
			String rawNamespaceName = getQualifiedNameFromExpression(nsDecl.getNameExpression());
			if(oldNs.isEmpty())
			{
				this.currentNamespacePrefix = rawNamespaceName;
			}
			else
			{
				this.currentNamespacePrefix = oldNs + "." + rawNamespaceName;
			}

			for(ClassDeclaration cd : nsDecl.getClassDeclarations())
			{
				String fullName = currentNamespacePrefix.isEmpty() ? cd.getName().getLexeme() : currentNamespacePrefix + "." + cd.getName().getLexeme();
				ClassSymbol cs = declaredClasses.get(fullName);
				// Define fields
				for(FieldDeclaration fd : cd.getFields())
				{
					Type t = getTypeFromToken(fd.getType());
					if(t == null)
						t = ErrorType.INSTANCE;
					boolean isStatic = fd.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.STATIC);
					boolean isPublic = fd.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.PUBLIC);
					boolean isPrivate = !isPublic;
					VariableSymbol vs = new VariableSymbol(fd.getName().getLexeme(), t, fd.getName(), fd.getInitializer() != null, isStatic, false, isPublic);
					vs.setOwnerClass(cs); // Set owner class for fields
					cs.getClassScope().define(vs);
				}
				// Define constructors
				for(ConstructorDeclaration ctor : cd.getConstructors())
				{
					List<Type> params = new ArrayList<>();
					for(int i = 0; i < ctor.getParameters().size(); i += 2)
					{
						Type pt = getTypeFromToken(ctor.getParameters().get(i));
						if(pt == null)
							pt = ErrorType.INSTANCE;
						params.add(pt);
					}
					MethodSymbol ms = new MethodSymbol(ctor.getName().getLexeme(), params, ctor.getName(),
							new SymbolTable(cs.getClassScope(), "Constructor:" + ctor.getName() + params.hashCode()),
							ctor.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.PUBLIC));
					ms.setOwnerClass(cs);
					cs.defineMethod(ms);
				}
				// Synthetic default constructor
				if(cd.getConstructors().isEmpty())
				{
					MethodSymbol dft = new MethodSymbol(cd.getName().getLexeme(), Collections.emptyList(), cd.getName(),
							new SymbolTable(cs.getClassScope(), "ImplicitCtor"), true);
					dft.setOwnerClass(cs);
					cs.defineMethod(dft);
				}
				// Define methods
				for(MethodDeclaration md : cd.getMethods())
				{
					Type rt = getTypeFromToken(md.getReturnType());
					if(rt == null)
						rt = ErrorType.INSTANCE;
					List<Type> params = new ArrayList<>();
					for(int i = 0; i < md.getParameters().size(); i += 2)
					{
						Type pt = getTypeFromToken(md.getParameters().get(i));
						if(pt == null)
							pt = ErrorType.INSTANCE;
						params.add(pt);
					}
					boolean isStatic = md.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.STATIC);
					boolean isPublic = md.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.PUBLIC);
					MethodSymbol ms = new MethodSymbol(md.getName().getLexeme(), rt, params, md.getName(),
							new SymbolTable(cs.getClassScope(), "Method:" + md.getName() + params.hashCode()), isStatic, isPublic);
					ms.setOwnerClass(cs);
					cs.defineMethod(ms);
				}
			}
			this.currentNamespacePrefix = oldNs;
		}
		System.out.println("--- First Pass Complete ---");

		// Process imports
		for(ImportDirective id : program.getImportDirectives())
			id.accept(this);

		// Second pass: full analysis
		program.accept(this);
		exitScope();
		return !errorReporter.hasErrors();
	}

	/**
	 * Helper to define a global type (e.g., int, bool).
	 *
	 * @param type The type to define globally.
	 */
	private void defineGlobalType(Type type)
	{
		currentScope.define(new Symbol(type.getName(), type, null, true)
		{ // Pass true for isPublic
			// Anonymous class for simple type symbols
			@Override
			public String toString()
			{
				return "Type: " + getName();
			}

			// All predefined primitive types are considered static for symbol resolution
			@Override
			public boolean isStatic()
			{
				return true;
			}
		});
	}

	/**
	 * Enters a new scope, setting it as the current scope.
	 *
	 * @param newScope The scope to enter.
	 */
	private void enterScope(SymbolTable newScope)
	{
		currentScope = newScope;
	}

	/**
	 * Exits the current scope, reverting to the enclosing scope.
	 */
	private void exitScope()
	{
		if(currentScope != null)
		{
			currentScope = currentScope.getEnclosingScope();
		}
	}

	/**
	 * Reports a semantic error.
	 *
	 * @param token   The token associated with the error.
	 * @param message The error message.
	 */
	private void error(Token token, String message)
	{
		errorReporter.report(token.getLine(), token.getColumn(), "[Semantic Error] " + message);
	}

	/**
	 * Helper to get the actual Type object for a given TokenType or identifier.
	 *
	 * @param token The token representing the type (e.g., INT, STRING_KEYWORD, IDENTIFIER for a custom class name).
	 * @return The corresponding Type object, or null if the type is not found/invalid.
	 */
	public Type getTypeFromToken(Token token)
	{
		switch(token.getType())
		{
			case INT:
				return PrimitiveType.INT;
			case BOOL:
				return PrimitiveType.BOOL;
			case CHAR:
				return PrimitiveType.CHAR;
			case STRING_KEYWORD: // The 'string' keyword maps to the Nebula.Lang.String class
				ClassSymbol stringClass = declaredClasses.get("nebula.core.String");
				if(stringClass != null)
				{
					return stringClass.getType();
				}
				error(token, "Internal error: 'string' keyword could not be mapped to Nebula.Lang.String class.");
				return ErrorType.INSTANCE; // Return ErrorType instead of null for better propagation
			case DOUBLE:
				return PrimitiveType.DOUBLE;
			case FLOAT:
				return PrimitiveType.FLOAT;
			case BYTE:
				return PrimitiveType.BYTE;
			case VOID:
				return PrimitiveType.VOID;
			case VAR:
				error(token, "'var' cannot be used as an explicit type declaration.");
				return ErrorType.INSTANCE; // Return ErrorType
			case IDENTIFIER:
				String className = token.getLexeme();
				String fullyQualifiedName = getFullyQualifiedClassName(className);

				ClassSymbol classSymbol = declaredClasses.get(fullyQualifiedName);
				if(classSymbol != null)
				{
					return classSymbol.getType();
				}
				error(token, "Undefined type: '" + token.getLexeme() + "'. Ensure the namespace is imported or the type is fully qualified.");
				return ErrorType.INSTANCE; // Return ErrorType
			default:
				error(token, "Invalid type token: '" + token.getLexeme() + "'.");
				return ErrorType.INSTANCE; // Return ErrorType
		}
	}

	/**
	 * Attempts to construct a fully qualified class name for a simple identifier
	 * based on known classes in the `declaredClasses` map and the current namespace/imports.
	 *
	 * @param simpleClassName The simple class name (e.g., "Point", "Console", "Program").
	 * @return The fully qualified name if resolved, or the simple name if not found.
	 */
	private String getFullyQualifiedClassName(String simpleClassName)
	{
		// 1. Check if the simple name directly maps to a fully qualified class (e.g., if it's already an FQN, or if `String` is globally known)
		if(declaredClasses.containsKey(simpleClassName))
		{
			return simpleClassName;
		}

		// 2. Check within the current namespace prefix
		if(!currentNamespacePrefix.isEmpty())
		{
			String qualifiedInCurrentNamespace = currentNamespacePrefix + "." + simpleClassName;
			if(declaredClasses.containsKey(qualifiedInCurrentNamespace))
			{
				return qualifiedInCurrentNamespace;
			}
		}

		// 3. Check imported namespaces (from 'import' directives)
		for(String importedNs : importedNamespaces)
		{
			String qualifiedName = importedNs + "." + simpleClassName;
			if(declaredClasses.containsKey(qualifiedName))
			{
				return qualifiedName;
			}
		}

		// 4. Check well-known core namespaces (e.g., Nebula.Lang, Com.NebulaLang.Core)
		String[] coreNamespaces = {"nebula.core", "nebula.io"}; // Added nebula.io as a common core namespace
		for(String ns : coreNamespaces)
		{
			String qualifiedName = ns + "." + simpleClassName;
			if(declaredClasses.containsKey(qualifiedName))
			{
				return qualifiedName;
			}
		}

		// Return: if not found, return the simple name.
		return simpleClassName;
	}


	/**
	 * Extracts the qualified name from a DotExpression or IdentifierExpression.
	 * This is a recursive helper function to build full names like "Com.NebulaLang.Core.Console".
	 * It does NOT resolve symbols, it just builds the string path.
	 *
	 * @param expression The expression (Identifier or Dot) from which to extract the name.
	 * @return The string of the fully qualified name, or null if the expression type is unexpected.
	 */
	private String getQualifiedNameFromExpression(Expression expression)
	{
		if(expression instanceof IdentifierExpression)
		{
			return ((IdentifierExpression) expression).getName().getLexeme();
		}
		else if(expression instanceof DotExpression)
		{
			DotExpression dot = (DotExpression) expression;
			String leftPart = getQualifiedNameFromExpression(dot.getLeft());
			if(leftPart == null)
			{
				return null;
			}
			return leftPart + "." + dot.getMemberName().getLexeme();
		}
		return null;
	}


	/**
	 * Determines the resulting type of binary operation.
	 *
	 * @param operator  The operator token.
	 * @param leftType  The type of the left operand.
	 * @param rightType The type of the right operand.
	 * @return The resulting Type, or ErrorType.INSTANCE if incompatible.
	 */
	private Type getBinaryExpressionResultType(Token operator, Type leftType, Type rightType)
	{
		if(leftType instanceof ErrorType || rightType instanceof ErrorType)
		{
			return ErrorType.INSTANCE;
		}

		// Handle string concatenation first (String is a ClassType: Nebula.Lang.String)
		ClassSymbol stringClassSymbol = declaredClasses.get("nebula.core.String");
		ClassType stringClassType = null;
		if(stringClassSymbol != null)
		{
			stringClassType = (ClassType) stringClassSymbol.getType();
		}

		if(operator.getType() == TokenType.PLUS && (leftType.equals(stringClassType) || rightType.equals(stringClassType)))
		{
			return stringClassType;
		}

		// Check general compatibility using the isCompatibleWith method.
		if(!leftType.isCompatibleWith(rightType))
		{
			error(operator, "Incompatible types for binary operation '" + operator.getLexeme() + "': " + leftType.getName() + " and " + rightType.getName() + ".");
			return ErrorType.INSTANCE;
		}

		// Arithmetic operations (+, -, *, /, %)
		if(operator.getType() == TokenType.PLUS || operator.getType() == TokenType.MINUS ||
				operator.getType() == TokenType.STAR || operator.getType() == TokenType.SLASH ||
				operator.getType() == TokenType.MODULO)
		{
			// If compatible, use the wider numeric type for the result
			return Type.getWiderNumericType(leftType, rightType);
		}

		// Comparison operations (==, !=, <, >, <=, >=) always result in bool
		if(operator.getType() == TokenType.EQUAL_EQUAL || operator.getType() == TokenType.BANG_EQUAL ||
				operator.getType() == TokenType.LESS || operator.getType() == TokenType.GREATER ||
				operator.getType() == TokenType.LESS_EQUAL || operator.getType() == TokenType.GREATER_EQUAL)
		{
			// Ensure types are comparable
			if(!Type.isComparable(leftType, rightType))
			{
				error(operator, "Types '" + leftType.getName() + "' and '" + rightType.getName() + "' are not comparable with '" + operator.getLexeme() + "'.");
				return ErrorType.INSTANCE;
			}
			return PrimitiveType.BOOL;
		}

		// Logical operations (&&, ||)
		if(operator.getType() == TokenType.AMPERSAND_AMPERSAND || operator.getType() == TokenType.PIPE_PIPE)
		{
			if(!leftType.equals(PrimitiveType.BOOL) || !rightType.equals(PrimitiveType.BOOL))
			{
				error(operator, "Logical operators '&&' and '||' require boolean operands.");
				return ErrorType.INSTANCE;
			}
			return PrimitiveType.BOOL;
		}

		error(operator, "Unsupported binary operator '" + operator.getLexeme() + "' for given types.");
		return ErrorType.INSTANCE;
	}

	/**
	 * Determines the resulting type of a unary operation.
	 *
	 * @param operator    The operator token.
	 * @param operandType The type of the operand.
	 * @return The resulting Type, or ErrorType.INSTANCE if incompatible.
	 */
	private Type getUnaryExpressionResultType(Token operator, Type operandType)
	{
		if(operandType instanceof ErrorType)
		{
			return ErrorType.INSTANCE;
		}

		switch(operator.getType())
		{
			case BANG: // Logical NOT (!)
				if(!operandType.equals(PrimitiveType.BOOL))
				{
					error(operator, "Unary operator '!' can only be applied to a boolean expression.");
					return ErrorType.INSTANCE;
				}
				return PrimitiveType.BOOL;
			case MINUS: // Negation (-)
				if(!(operandType.equals(PrimitiveType.INT) || operandType.equals(PrimitiveType.DOUBLE) || operandType.equals(PrimitiveType.FLOAT) || operandType.equals(PrimitiveType.BYTE) || operandType.equals(PrimitiveType.CHAR)))
				{
					error(operator, "Unary operator '-' can only be applied to numeric expressions.");
					return ErrorType.INSTANCE;
				}
				return operandType; // Type remains the same (int -> int, double -> double)
			case PLUS_PLUS:   // Increment (++)
			case MINUS_MINUS: // Decrement (--)
				if(!(operandType.isNumeric())) // Use isNumeric helper
				{
					error(operator, "Increment/decrement operators '++' and '--' can only be applied to numeric expressions.");
					return ErrorType.INSTANCE;
				}
				return operandType;
			default:
				error(operator, "Unsupported unary operator: '" + operator.getLexeme() + "'.");
				return ErrorType.INSTANCE;
		}
	}


	// --- AST Visitor Implementations ---

	@Override
	public Type visitProgram(Program program)
	{
		// Set up initial currentConstructor to null before main traversal
		this.currentConstructor = null;

		for(NamespaceDeclaration namespaceDecl : program.getNamespaceDeclarations())
		{
			namespaceDecl.accept(this);
		}
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitImportDirective(ImportDirective directive)
	{
		String qualifiedName = getQualifiedNameFromExpression(directive.getQualifiedName());

		boolean isStatic = directive.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.STATIC);

		// Try to resolve as a class first by exact FQN
		ClassSymbol classSym = declaredClasses.get(qualifiedName);

		// If not found by exact FQN, try to resolve as a class within already imported namespaces
		if(classSym == null)
		{
			String resolvedFQN = resolveAgainstImportedNamespaces(qualifiedName);
			if(resolvedFQN != null)
			{
				classSym = declaredClasses.get(resolvedFQN);
			}
		}

		if(classSym != null)
		{
			// This is a direct class import or a class within an imported namespace
			String simpleName = getSimpleNameFromQualifiedName(qualifiedName); // Extract simple name
			importedClasses.put(simpleName, classSym);
			// If it's a static import, add the class to importedStaticClasses.
			// More granular static member import would need further resolution here.
			if(isStatic)
			{
				importedStaticClasses.add(classSym);
			}
		}
		else // Not a directly known class, might be a namespace or an undeclared class
		{
			// If it's a static import, and we didn't find a class, it's an error (static imports are always for classes/members)
			if(isStatic)
			{
				error(directive.getImportKeyword(), "Could not resolve static import '" + qualifiedName + "'. It must refer to a known class.");
			}
			else
			{
				// Assume it's a namespace import if it's not a static import and not a known class
				// This is a heuristic. A more robust solution might involve explicitly checking for namespaces.
				// For now, if it's not a static import and not a class, add it to importedNamespaces.
				importedNamespaces.add(qualifiedName);
			}
			// Only report an error if it was a class import attempt or static import
			// If it's a namespace, it might contain classes that are resolved later.
			// However, if the full qualified name itself isn't a class and isn't part of the namespace tree, it's an error.
			// The original error "It is not a known class or namespace." is correct if it's truly unresolvable.
			// The current logic adds it to importedNamespaces, so subsequent lookups (like getFullyQualifiedClassName) might resolve it.
			// If it's still an error, it means the namespace itself (e.g., "Nebula.System") is not recognized as a valid prefix
			// for any declared class, or "Console" isn't a class within "Nebula.System".
			// For direct import errors, the error is still valid.
			error(directive.getImportKeyword(), "Could not resolve import '" + qualifiedName + "'. It is not a known class or namespace.");
		}
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitNamespaceDeclaration(NamespaceDeclaration declaration)
	{
		String rawNamespaceName = getQualifiedNameFromExpression(declaration.getNameExpression());
		String oldNamespacePrefix = this.currentNamespacePrefix;

		String fqnForNamespace = "";
		if(oldNamespacePrefix.isEmpty())
		{
			fqnForNamespace = rawNamespaceName;
		}
		else
		{
			fqnForNamespace = oldNamespacePrefix + "." + rawNamespaceName;
		}

		this.currentNamespacePrefix = fqnForNamespace; // Update prefix for nested declarations

		SymbolTable namespaceScope = new SymbolTable(currentScope, "Namespace:" + fqnForNamespace);
		enterScope(namespaceScope);

		for(ClassDeclaration classDecl : declaration.getClassDeclarations())
		{
			classDecl.accept(this); // Recursive call for class declarations
		}

		exitScope();
		this.currentNamespacePrefix = oldNamespacePrefix; // Restore previous namespace
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitClassDeclaration(ClassDeclaration declaration)
	{
		String simpleClassName = declaration.getName().getLexeme();
		// Ensure fullClassName is correctly derived based on the current namespace prefix
		String fullClassName = currentNamespacePrefix.isEmpty() ? simpleClassName : currentNamespacePrefix + "." + simpleClassName;


		ClassSymbol classSymbol = declaredClasses.get(fullClassName);
		if(classSymbol == null)
		{
			error(declaration.getName(), "Internal error: Class symbol not found for " + fullClassName + ". This indicates an error in class discovery or lookup.");
			return ErrorType.INSTANCE; // Return ErrorType
		}
		System.out.println("DEBUG: visitClassDeclaration: Looking up class '" + fullClassName + "'");

		currentClass = classSymbol; // Set the current class context
		enterScope(classSymbol.getClassScope()); // Enter the class's scope (this scope holds fields)

		// Set the resolved symbol on the class declaration's name expression.
		// This should have already been done in the first pass, but good to ensure.
		// CRITICAL FIX: The ClassDeclaration's name expression MUST have its ClassSymbol set.
		declaration.getNameExpression().setResolvedSymbol(classSymbol);


		// Propagate the actual superclass type to the ClassType object
		if(declaration.getExtendsKeyword() != null)
		{
			String superClassName = declaration.getSuperClassName().getLexeme();
			String fullSuperClassName = getFullyQualifiedClassName(superClassName);
			ClassSymbol superSym = declaredClasses.get(fullSuperClassName);
			if(superSym != null)
			{
				((ClassType) currentClass.getType()).setSuperClassType(superSym.getType());
			}
			else
			{
				// Error already reported in first pass during ClassSymbol creation,
				// but ensure the ClassType reflects this, potentially using ErrorType for superclass.
				((ClassType) currentClass.getType()).setSuperClassType(ErrorType.INSTANCE);
			}
		}
		else
		{
			// Explicitly set Object as superclass if no 'extends' clause (unless it's Object itself)
			if(!fullClassName.equals("nebula.core.Object"))
			{
				((ClassType) currentClass.getType()).setSuperClassType(declaredClasses.get("nebula.core.Object").getType());
			}
		}


		// Phase 1: Analyze field initializers
		for(FieldDeclaration fieldDecl : declaration.getFields())
		{
			boolean oldStaticContext = inStaticContext;
			if(fieldDecl.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.STATIC))
			{
				inStaticContext = true; // Set static context for static field initializers
			}
			fieldDecl.accept(this); // Visit initializers (will perform type checking and mark initialized)
			inStaticContext = oldStaticContext; // Restore
		}

		// Phase 2: Visit the bodies of methods and constructors
		for(ConstructorDeclaration constructorDecl : declaration.getConstructors())
		{
			List<Type> constructorParamTypes = new ArrayList<>();
			for(int i = 0; i < constructorDecl.getParameters().size(); i += 2)
			{
				Type paramType = getTypeFromToken(constructorDecl.getParameters().get(i));
				if(paramType == null || paramType instanceof ErrorType)
				{
					paramType = ErrorType.INSTANCE;
				}
				constructorParamTypes.add(paramType);
			}

			// Resolve the specific constructor symbol using parameter types
			Symbol resolvedSymbol = classSymbol.resolveMember(constructorDecl.getName().getLexeme(), constructorParamTypes);
			MethodSymbol resolvedConstructor = null;

			if(resolvedSymbol instanceof MethodSymbol && ((MethodSymbol) resolvedSymbol).isConstructor())
			{
				resolvedConstructor = (MethodSymbol) resolvedSymbol;
			}

			if(resolvedConstructor != null)
			{
				MethodSymbol oldCurrentMethod = currentMethod;
				MethodSymbol oldCurrentConstructor = currentConstructor; // Save current constructor state
				this.currentMethod = resolvedConstructor;
				this.currentConstructor = resolvedConstructor; // Set current constructor
				this.inStaticContext = false; // Constructors are never static

				enterScope(currentMethod.getMethodScope());

				// Define parameters as local variables in the constructor's scope
				for(int i = 0; i < constructorDecl.getParameters().size(); i += 2)
				{
					Token paramTypeToken = constructorDecl.getParameters().get(i);
					Token paramNameToken = constructorDecl.getParameters().get(i + 1);
					Type paramType = getTypeFromToken(paramTypeToken);
					if(paramType == null || paramType instanceof ErrorType)
						continue;

					VariableSymbol paramSymbol = new VariableSymbol(paramNameToken.getLexeme(), paramType, paramNameToken, true, false, false, false);
					currentScope.define(paramSymbol);
				}

				constructorDecl.getBody().accept(this);
				exitScope();
				this.currentMethod = oldCurrentMethod;
				this.currentConstructor = oldCurrentConstructor; // Restore current constructor state
			}
			else
			{
				error(constructorDecl.getName(), "Internal error: Could not resolve constructor '" + constructorDecl.getName().getLexeme() + "' during second pass.");
			}
		}

		// Handle the synthetic default constructor's body if it was added and has a body
		List<Type> emptyParams = new ArrayList<>();
		// Check if a constructor with no arguments is already defined
		Symbol noArgCtorSymbol = classSymbol.resolveMember(classSymbol.getName(), emptyParams);
		MethodSymbol defaultConstructorSymbol = null;
		if(noArgCtorSymbol instanceof MethodSymbol && ((MethodSymbol) noArgCtorSymbol).isConstructor())
		{
			defaultConstructorSymbol = (MethodSymbol) noArgCtorSymbol;
		}

		// Only process if the default constructor exists and the class declared no explicit constructors
		if(declaration.getConstructors().isEmpty() && defaultConstructorSymbol != null)
		{
			MethodSymbol oldCurrentMethod = currentMethod;
			MethodSymbol oldCurrentConstructor = currentConstructor;
			this.currentMethod = defaultConstructorSymbol;
			this.currentConstructor = defaultConstructorSymbol;
			this.inStaticContext = false;

			enterScope(currentMethod.getMethodScope());
			// A synthetic default constructor has no explicit AST body to visit,
			// so this block is mainly for scope setup for potential implicit calls/initializers.
			exitScope();
			this.currentMethod = oldCurrentMethod;
			this.currentConstructor = oldCurrentConstructor;
		}


		for(MethodDeclaration methodDecl : declaration.getMethods())
		{
			List<Type> methodParamTypes = new ArrayList<>();
			for(int i = 0; i < methodDecl.getParameters().size(); i += 2)
			{
				Token paramTypeToken = methodDecl.getParameters().get(i);
				Type paramType = getTypeFromToken(paramTypeToken);
				if(paramType == null || paramType instanceof ErrorType)
				{
					paramType = ErrorType.INSTANCE;
				}
				methodParamTypes.add(paramType);
			}

			Symbol resolvedSymbol = classSymbol.resolveMember(methodDecl.getName().getLexeme(), methodParamTypes);
			MethodSymbol resolvedMethod = null;
			if(resolvedSymbol instanceof MethodSymbol && !((MethodSymbol) resolvedSymbol).isConstructor())
			{
				resolvedMethod = (MethodSymbol) resolvedSymbol;
			}

			if(resolvedMethod != null)
			{
				MethodSymbol oldCurrentMethod = currentMethod;
				this.currentMethod = resolvedMethod;
				this.inStaticContext = currentMethod.isStatic(); // Set static context based on method

				enterScope(currentMethod.getMethodScope());

				for(int i = 0; i < methodDecl.getParameters().size(); i += 2)
				{
					Token paramTypeToken = methodDecl.getParameters().get(i);
					Token paramNameToken = methodDecl.getParameters().get(i + 1);
					Type paramType = getTypeFromToken(paramTypeToken);
					if(paramType == null || paramType instanceof ErrorType)
						continue;

					VariableSymbol paramSymbol = new VariableSymbol(paramNameToken.getLexeme(), paramType, paramNameToken, true, false, false, false);
					currentScope.define(paramSymbol);
				}

				if(methodDecl.getBody() != null)
				{ // Only visit if a body exists (not a semicolon method)
					methodDecl.getBody().accept(this);
				}
				exitScope();
				this.currentMethod = oldCurrentMethod;
				this.inStaticContext = false; // Reset static context after method
			}
			else
			{
				error(methodDecl.getName(), "Internal error: Could not resolve method '" + methodDecl.getName().getLexeme() + "' during second pass.");
			}
		}

		exitScope(); // Exit the class scope
		currentClass = null;
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitConstructorDeclaration(ConstructorDeclaration declaration)
	{
		return PrimitiveType.VOID; // Handled in visitClassDeclaration's second pass
	}

	@Override
	public Type visitMethodDeclaration(MethodDeclaration declaration)
	{
		return PrimitiveType.VOID; // Handled in visitClassDeclaration's second pass
	}

	/**
	 * Visits a ConstructorChainingCallStatement (e.g., `this(...)` or `super(...)`).
	 *
	 * @param statement The ConstructorChainingCallStatement AST node to visit.
	 * @return PrimitiveType.VOID if successful, ErrorType.INSTANCE if semantic errors occur.
	 */
	@Override
	public Type visitConstructorChainingCallStatement(ConstructorChainingCallStatement statement)
	{
		// Validation: Must be inside a constructor
		if(currentClass == null || currentMethod == null || !currentMethod.isConstructor())
		{
			error(statement.getKeyword(), "Constructor call (this(...) or super(...)) can only be made from within a constructor.");
			return ErrorType.INSTANCE;
		}

		// Collect argument types
		List<Type> argTypes = new ArrayList<>();
		for(Expression arg : statement.getArguments())
		{
			Type argType = arg.accept(this);
			if(argType == null || argType instanceof ErrorType)
			{
				return ErrorType.INSTANCE; // Propagate error
			}
			argTypes.add(argType);
		}

		MethodSymbol targetConstructor = null;
		ClassSymbol classToCallConstructorOn = null;

		if(statement.getKeyword().getType() == TokenType.THIS)
		{
			classToCallConstructorOn = currentClass; // Calling a constructor of the current class
		}
		else if(statement.getKeyword().getType() == TokenType.SUPER)
		{
			if(currentClass.getType().getSuperClassType() instanceof ClassType)
			{
				classToCallConstructorOn = ((ClassType) currentClass.getType().getSuperClassType()).getClassSymbol();
				if(classToCallConstructorOn == null)
				{
					error(statement.getKeyword(), "Internal error: Superclass symbol not found for 'super()' call.");
					return ErrorType.INSTANCE;
				}
			}
			else
			{
				error(statement.getKeyword(), "Cannot call 'super()' constructor as there is no valid superclass.");
				return ErrorType.INSTANCE;
			}
		}
		else
		{
			error(statement.getKeyword(), "Unexpected keyword for constructor call: Expected 'this' or 'super'.");
			return ErrorType.INSTANCE;
		}

		if(classToCallConstructorOn == null)
		{
			error(statement.getKeyword(), "Internal error: Cannot determine class for constructor call (this/super).");
			return ErrorType.INSTANCE;
		}

		// FIX: Explicitly resolve the constructor and check if it's a constructor symbol
		Symbol resolvedSymbol = classToCallConstructorOn.resolveMember(classToCallConstructorOn.getName(), argTypes);

		if(resolvedSymbol instanceof MethodSymbol)
		{
			MethodSymbol candidate = (MethodSymbol) resolvedSymbol;
			if(candidate.isConstructor())
			{
				targetConstructor = candidate;
			}
		}

		if(targetConstructor == null)
		{
			StringBuilder argTypesBuilder = new StringBuilder();
			// FIX: Use 'argTypes' instead of 'actualArgumentTypes' here
			for(int i = 0; i < argTypes.size(); i++)
			{
				argTypesBuilder.append(argTypes.get(i).getName());
				if(i < argTypes.size() - 1)
				{
					argTypesBuilder.append(", ");
				}
			}
			String argTypesString = argTypesBuilder.toString();

			error(statement.getKeyword(), "No matching constructor found for class '" + classToCallConstructorOn.getName() + "' with arguments: (" + argTypesString + ").");
			return ErrorType.INSTANCE;
		}

		// Semantic check: Prevent recursive constructor calls (e.g., A() calls A())
		if(targetConstructor == currentConstructor)
		{
			error(statement.getKeyword(), "Recursive constructor call detected: A constructor cannot directly call itself.");
			return ErrorType.INSTANCE;
		}

		// Ensure 'super()' or 'this()' is the very first statement in the constructor.
		// This is a semantic check that often requires more context than just `visit`ing.
		// A common way is that the parser enforces this as a grammar rule, or the `BlockStatement`
		// visitor keeps track if the first statement is a constructor chaining call.
		// For now, we will add a warning/error here if it's not the first,
		// but note that robustly checking this requires more state in the visitor
		// or a different parsing strategy.
		// Assuming parser makes it the first for now.

		return PrimitiveType.VOID; // Constructor calls don't return a value
	}

	@Override
	public Type visitFieldDeclaration(FieldDeclaration declaration)
	{
		Type fieldType = getTypeFromToken(declaration.getType());
		if(fieldType instanceof ErrorType)
		{
			return PrimitiveType.VOID;
		}

		if(fieldType.equals(PrimitiveType.VOID))
		{
			error(declaration.getType(), "Cannot declare a field of type 'void'.");
			return PrimitiveType.VOID;
		}

		boolean isStaticField = declaration.getModifiers().stream()
				.anyMatch(m -> m.getType() == TokenType.STATIC);
		boolean isConstField = declaration.getModifiers().stream()
				.anyMatch(m -> m.getType() == TokenType.CONST);
		boolean isPublicField = declaration.getModifiers().stream()
				.anyMatch(m -> m.getType() == TokenType.PUBLIC);
		boolean isPrivateField = declaration.getModifiers().stream()
				.anyMatch(m -> m.getType() == TokenType.PRIVATE);

		if(!isPublicField && !isPrivateField)
		{
			isPrivateField = true;
		}

		// Fetch the field symbol that was defined in the first pass
		Symbol fieldSymInScope = currentScope.resolve(declaration.getName().getLexeme());
		VariableSymbol fieldSymbol = null;
		if(fieldSymInScope instanceof VariableSymbol)
		{
			fieldSymbol = (VariableSymbol) fieldSymInScope;
		}
		else
		{
			error(declaration.getName(), "Internal error: Field symbol '" + declaration.getName().getLexeme() + "' not found or is not a VariableSymbol.");
			return PrimitiveType.VOID;
		}


		if(declaration.getInitializer() != null)
		{
			boolean oldStaticContext = inStaticContext;
			inStaticContext = isStaticField;
			Type initializerType = declaration.getInitializer().accept(this);
			inStaticContext = oldStaticContext;

			if(initializerType instanceof ErrorType)
				return PrimitiveType.VOID; // Propagate error

			if(!fieldType.isAssignableTo(initializerType)) // Changed from isAssignableFrom to isAssignableTo
			{
				error(declaration.getName(), "Incompatible types in field initialization: cannot assign '" + initializerType.getName() + "' to '" + fieldType.getName() + "'.");
			}
			fieldSymbol.setInitialized(true); // Mark as initialized
		}
		else
		{
			if(isConstField)
			{
				error(declaration.getName(), "Constant field '" + declaration.getName().getLexeme() + "' must be initialized.");
			}
			fieldSymbol.setInitialized(false); // Explicitly mark as uninitialized
		}
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitBlockStatement(BlockStatement statement)
	{
		SymbolTable blockScope = new SymbolTable(currentScope, "Block");
		enterScope(blockScope);

		for(com.juanpa.nebula.transpiler.ast.statements.Statement stmt : statement.getStatements())
		{
			stmt.accept(this);
		}

		exitScope();
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitExpressionStatement(ExpressionStatement statement)
	{
		return statement.getExpression().accept(this);
	}

	@Override
	public Type visitIfStatement(IfStatement statement)
	{
		Type conditionType = statement.getCondition().accept(this);
		if(conditionType instanceof ErrorType)
			return ErrorType.INSTANCE;

		if(!conditionType.equals(PrimitiveType.BOOL))
		{
			error(statement.getCondition().getFirstToken(), "If condition must be a boolean expression.");
		}

		statement.getThenBranch().accept(this);
		if(statement.getElseBranch() != null)
		{
			statement.getElseBranch().accept(this);
		}
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitWhileStatement(WhileStatement statement)
	{
		Type conditionType = statement.getCondition().accept(this);
		if(conditionType instanceof ErrorType)
			return ErrorType.INSTANCE;

		if(!conditionType.equals(PrimitiveType.BOOL))
		{
			error(statement.getCondition().getFirstToken(), "While condition must be a boolean expression.");
		}
		statement.getBody().accept(this);
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitForStatement(ForStatement statement)
	{
		SymbolTable forScope = new SymbolTable(currentScope, "ForLoop");
		enterScope(forScope);

		if(statement.getInitializer() != null)
		{
			statement.getInitializer().accept(this);
		}

		if(statement.getCondition() != null)
		{
			Type conditionType = statement.getCondition().accept(this);
			if(conditionType instanceof ErrorType)
				return ErrorType.INSTANCE;

			if(!conditionType.equals(PrimitiveType.BOOL))
			{
				error(statement.getCondition().getFirstToken(), "For loop condition must be a boolean expression.");
			}
		}

		if(statement.getIncrement() != null)
		{
			statement.getIncrement().accept(this);
		}

		statement.getBody().accept(this);

		exitScope();
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitReturnStatement(ReturnStatement statement)
	{
		if(currentMethod == null)
		{
			error(statement.getKeyword(), "Return statement must be inside a method or constructor.");
			return PrimitiveType.VOID;
		}

		Type expectedReturnType = currentMethod.getType();

		if(statement.getValue() == null)
		{
			if(!expectedReturnType.equals(PrimitiveType.VOID))
			{
				error(statement.getKeyword(), "Method '" + currentMethod.getName() + "' expects a return value of type '" + expectedReturnType.getName() + "'.");
			}
		}
		else
		{
			Type actualReturnType = statement.getValue().accept(this);
			if(actualReturnType instanceof ErrorType)
				return ErrorType.INSTANCE;

			if(!expectedReturnType.isAssignableTo(actualReturnType)) // Changed from isAssignableFrom to isAssignableTo
			{
				error(statement.getKeyword(), "Cannot return value of type '" + actualReturnType.getName() + "'. Expected '" + expectedReturnType.getName() + "'.");
			}
		}
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitVariableDeclarationStatement(VariableDeclarationStatement statement)
	{
		Type declaredType = null;
		if(statement.getTypeToken().getType() == TokenType.VAR)
		{
			declaredType = null;
		}
		else
		{
			declaredType = getTypeFromToken(statement.getTypeToken());
			if(declaredType instanceof ErrorType)
			{
				return PrimitiveType.VOID;
			}
			if(declaredType.equals(PrimitiveType.VOID))
			{
				error(statement.getTypeToken(), "Cannot declare a variable of type 'void'.");
				return PrimitiveType.VOID;
			}
		}

		boolean isConstVar = statement.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.CONST);

		Type initializerType = null;
		if(statement.getInitializer() != null)
		{
			initializerType = statement.getInitializer().accept(this);
			if(initializerType instanceof ErrorType)
				return PrimitiveType.VOID;

			if(declaredType == null)
			{ // Infer type for 'var'
				declaredType = initializerType;
			}
		}
		else
		{
			if(isConstVar)
			{
				error(statement.getName(), "Constant variable '" + statement.getName().getLexeme() + "' must be initialized.");
			}
		}

		if(statement.getTypeToken().getType() == TokenType.VAR && statement.getInitializer() == null)
		{
			error(statement.getTypeToken(), "'var' cannot be used without an initializer.");
			return PrimitiveType.VOID;
		}

		if(declaredType != null && initializerType != null && !declaredType.isAssignableTo(initializerType)) // Changed from isAssignableFrom to isAssignableTo
		{
			error(statement.getName(), "Incompatible types in variable declaration: cannot assign '" + initializerType.getName() + "' to '" + declaredType.getName() + "'.");
		}

		// Define the variable symbol in the current scope
		VariableSymbol variableSymbol = new VariableSymbol(
				statement.getName().getLexeme(),
				declaredType,
				statement.getName(),
				statement.getInitializer() != null,
				false, // Local variables and parameters are never static
				isConstVar,
				false // Local variables are always implicitly private
		);
		try
		{
			currentScope.define(variableSymbol);
		}
		catch(IllegalArgumentException e)
		{
			error(statement.getName(), "Variable '" + statement.getName().getLexeme() + "' is already defined in this scope.");
		}
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitBinaryExpression(BinaryExpression expression)
	{
		Type leftType = expression.getLeft().accept(this);
		Type rightType = expression.getRight().accept(this);

		Type resultType = getBinaryExpressionResultType(expression.getOperator(), leftType, rightType);
		if(resultType instanceof ErrorType)
		{
			// Error reported by getBinaryExpressionResultType
		}
		return resultType;
	}

	@Override
	public Type visitUnaryExpression(UnaryExpression expression)
	{
		Type operandType = expression.getRight().accept(this); // Corrected from .apply(this)
		Type resultType = getUnaryExpressionResultType(expression.getOperator(), operandType);

		if((expression.getOperator().getType() == TokenType.PLUS_PLUS || expression.getOperator().getType() == TokenType.MINUS_MINUS) &&
				!(expression.getRight() instanceof IdentifierExpression || expression.getRight() instanceof DotExpression || expression.getRight() instanceof ArrayAccessExpression))
		{
			error(expression.getOperator(), "Increment/decrement operators '++' and '--' must be applied to a variable, property, or array element.");
			return ErrorType.INSTANCE;
		}

		return resultType;
	}

	@Override
	public Type visitLiteralExpression(LiteralExpression expression)
	{
		if(expression.getLiteralToken().getType() == TokenType.INTEGER_LITERAL)
			return PrimitiveType.INT;
		if(expression.getLiteralToken().getType() == TokenType.DOUBLE_LITERAL)
			return PrimitiveType.DOUBLE;
		if(expression.getLiteralToken().getType() == TokenType.FLOAT_LITERAL) // NEW: Handle float literal
			return PrimitiveType.FLOAT;
		if(expression.getLiteralToken().getType() == TokenType.STRING_LITERAL)
			return declaredClasses.get("nebula.core.String").getType();
		if(expression.getLiteralToken().getType() == TokenType.CHAR_LITERAL)
			return PrimitiveType.CHAR;
		if(expression.getLiteralToken().getType() == TokenType.BOOLEAN_LITERAL)
			return PrimitiveType.BOOL;
		if(expression.getLiteralToken().getLiteral() == null && expression.getLiteralToken().getLexeme().equals("null"))
		{
			return NullType.INSTANCE; // Handle 'null' literal
		}

		error(expression.getLiteralToken(), "Unknown literal type.");
		return ErrorType.INSTANCE;
	}

	@Override
	public Type visitIdentifierExpression(IdentifierExpression expression)
	{
		String name = expression.getName().getLexeme();

		// 1. Look in the current scope chain (local variables, parameters, class members).
		Symbol symbol = currentScope.resolve(name);
		if(symbol != null)
		{
			expression.setResolvedSymbol(symbol); // Set resolved symbol
			if(symbol instanceof VariableSymbol)
			{
				VariableSymbol var = (VariableSymbol) symbol;
				// Check for illegal forward reference to instance field in its own initializer
				if(!var.isStatic() && inStaticContext && currentMethod == null)
				{
					error(expression.getName(), "Cannot reference an instance member '" + var.getName() + "' from a static field initializer.");
				}
				// Simplified initialization check:
				// Parameters are initialized upon definition, so `!var.isInitialized()` will be false for them.
				// This condition correctly flags only truly uninitialized local variables/fields.
				if(!var.isInitialized())
				{
					error(expression.getName(), "Variable '" + symbol.getName() + "' might not have been initialized.");
				}
			}
			return symbol.getType();
		}

		// 2. Look for a single imported static member (e.g., `import static System.Console.WriteLine;`).
		if(importedStaticMembers.containsKey(name))
		{
			Symbol staticMember = importedStaticMembers.get(name);
			expression.setResolvedSymbol(staticMember); // Set resolved symbol
			return staticMember.getType();
		}

		// 3. Search through wildcard imported classes for a static member (e.g., `import static System.Console.*`).
		for(ClassSymbol staticClass : importedStaticClasses)
		{
			Symbol memberCandidate = staticClass.resolveMember(name); // Try resolving without arguments
			if(memberCandidate instanceof MethodSymbol && memberCandidate.isStatic())
			{
				expression.setResolvedSymbol(staticClass); // For static method calls, set the class as the target for resolution
				return staticClass.getType(); // Return the type of the class itself
			}
			else if(memberCandidate instanceof VariableSymbol && memberCandidate.isStatic())
			{
				expression.setResolvedSymbol(memberCandidate); // Set the static field symbol
				return memberCandidate.getType();
			}
		}


		// 4. Look for an imported class simple name (e.g., `import System.Console;` allows using `Console`).
		if(importedClasses.containsKey(name))
		{
			ClassSymbol classSymbol = importedClasses.get(name);
			expression.setResolvedSymbol(classSymbol); // Set resolved symbol
			return classSymbol.getType();
		}

		// 5. Check for a class in the current namespace.
		String fqnInCurrentNs = currentNamespacePrefix.isEmpty() ? name : currentNamespacePrefix + "." + name;
		if(declaredClasses.containsKey(fqnInCurrentNs))
		{
			ClassSymbol classSymbol = declaredClasses.get(fqnInCurrentNs);
			expression.setResolvedSymbol(classSymbol); // Set resolved symbol
			return classSymbol.getType();
		}

		// 6. Check if it's a namespace prefix or a top-level declared class that is not fully qualified here.
		// If it's a potential namespace prefix that will be completed by a DotExpression, return null.
		// We can't set a NamespaceSymbol if the user doesn't have it.
		if(isStartOfAnyQualifiedClassName(name))
		{ // This helper checks if it's a prefix of any known FQN
			// If it's a prefix, it's not a complete type/variable/member itself, but part of a qualified name.
			// Return null to signal that the DotExpression should continue resolving.
			// No symbol is set on the IdentifierExpression itself in this case,
			// as it's just a segment of a larger qualified name.
			return null;
		}


		error(expression.getName(), "Undefined identifier: '" + name + "'. Ensure it's declared, imported, or a valid namespace prefix.");
		return ErrorType.INSTANCE;
	}

	/**
	 * Checks if the given name could be the start of any fully qualified class name or namespace.
	 * This helps differentiate between an undefined identifier and an incomplete qualified name.
	 *
	 * @param name The identifier name to check.
	 * @return True if it could be the start of a qualified name, false otherwise.
	 */
	private boolean isStartOfAnyQualifiedClassName(String name)
	{
		// Check if any declared class FQN starts with 'name' (followed by a dot)
		for(String fqn : declaredClasses.keySet())
		{
			if(fqn.startsWith(name + ".") || fqn.equals(name)) // Include equals(name) for top-level class names (e.g., "Object")
			{
				return true;
			}
		}

		// Check if any imported namespace starts with 'name' (followed by a dot)
		for(String importedNs : importedNamespaces)
		{
			if(importedNs.startsWith(name + ".") || importedNs.equals(name))
			{
				return true;
			}
		}
		// No NamespaceSymbol, so we can't check for NamespaceSymbols explicitly.
		// We rely on string prefix matching.

		return false;
	}


	@Override
	public Type visitAssignmentExpression(AssignmentExpression expression)
	{
		Type targetType = expression.getTarget().accept(this);
		Type valueType = expression.getValue().accept(this);

		if(targetType instanceof ErrorType || valueType instanceof ErrorType)
		{
			return ErrorType.INSTANCE;
		}

		if(!(expression.getTarget() instanceof IdentifierExpression ||
				expression.getTarget() instanceof DotExpression ||
				expression.getTarget() instanceof ArrayAccessExpression))
		{
			error(expression.getOperator(), "Invalid assignment target. Must be a variable, property, or array element.");
			return ErrorType.INSTANCE;
		}

		Symbol resolvedTargetSymbol = null;
		if(expression.getTarget() instanceof IdentifierExpression)
		{
			resolvedTargetSymbol = ((IdentifierExpression) expression.getTarget()).getResolvedSymbol();
		}
		else if(expression.getTarget() instanceof DotExpression)
		{
			resolvedTargetSymbol = ((DotExpression) expression.getTarget()).getResolvedSymbol();
		}
		else if(expression.getTarget() instanceof ArrayAccessExpression)
		{
			// For array access, the base array/string itself might be the target symbol
			Expression baseArray = ((ArrayAccessExpression) expression.getTarget()).getArray();
			if(baseArray instanceof IdentifierExpression)
			{
				resolvedTargetSymbol = ((IdentifierExpression) baseArray).getResolvedSymbol();
			}
			else if(baseArray instanceof DotExpression)
			{
				resolvedTargetSymbol = ((DotExpression) baseArray).getResolvedSymbol();
			}
			// resolvedTargetSymbol will be the array/string itself, not the element, which is correct for const check
		}


		if(resolvedTargetSymbol instanceof VariableSymbol)
		{
			VariableSymbol varSymbol = (VariableSymbol) resolvedTargetSymbol;
			if(varSymbol.isConst())
			{
				error(expression.getOperator(), "Cannot assign to constant variable/field '" + varSymbol.getName() + "'.");
				return ErrorType.INSTANCE;
			}
			// Mark as initialized if it's a variable or field
			varSymbol.setInitialized(true);
		}
		// The ArrayAccessExpression const check logic was redundant here as it's now handled by the general
		// `resolvedTargetSymbol instanceof VariableSymbol` check if the base of the array access is a variable/field.
		// If the base is a literal array (e.g., `new int[]{1,2,3}[0] = 5;`), that would be an AST structure
		// where the base is not a VariableSymbol, and generally array literals are not mutable in this way.


		if(!targetType.isAssignableTo(valueType)) // Changed from isAssignableFrom to isAssignableTo
		{
			error(expression.getOperator(), "Incompatible types in assignment: cannot assign '" + valueType.getName() + "' to '" + targetType.getName() + "'.");
			return ErrorType.INSTANCE;
		}

		// For compound assignments (+=, -=, etc.), check compatibility for the implicit binary operation
		if(expression.getOperator().getType() != TokenType.ASSIGN)
		{
			TokenType baseBinaryOpType = null;
			switch(expression.getOperator().getType())
			{
				case PLUS_ASSIGN:
					baseBinaryOpType = TokenType.PLUS;
					break;
				case MINUS_ASSIGN:
					baseBinaryOpType = TokenType.MINUS;
					break;
				case STAR_ASSIGN:
					baseBinaryOpType = TokenType.STAR;
					break;
				case SLASH_ASSIGN:
					baseBinaryOpType = TokenType.SLASH;
					break;
				case MODULO_ASSIGN:
					baseBinaryOpType = TokenType.MODULO;
					break;
				default:
					error(expression.getOperator(), "Internal error: Unhandled compound assignment operator type: " + expression.getOperator().getType() + ".");
					return ErrorType.INSTANCE;
			}
			Token baseBinaryOpToken = new Token(baseBinaryOpType, expression.getOperator().getLexeme().substring(0, 1), null,
					expression.getOperator().getLine(), expression.getOperator().getColumn());

			Type binaryOpResultType = getBinaryExpressionResultType(baseBinaryOpToken, targetType, valueType);
			if(binaryOpResultType instanceof ErrorType)
			{
				return ErrorType.INSTANCE;
			}
			if(!targetType.isAssignableTo(binaryOpResultType)) // Changed from isAssignableFrom to isAssignableTo
			{
				error(expression.getOperator(), "The result of the compound assignment operation ('" + expression.getOperator().getLexeme() + "') is not assignable back to the target type '" + targetType.getName() + "'.");
				return ErrorType.INSTANCE;
			}
		}

		return targetType;
	}

	@Override
	public Type visitCallExpression(CallExpression expression)
	{
		Expression callee = expression.getCallee();

		List<Type> actualArgumentTypes = new ArrayList<>();
		for(Expression arg : expression.getArguments())
		{
			Type argType = arg.accept(this);
			if(argType instanceof ErrorType)
				return ErrorType.INSTANCE;
			actualArgumentTypes.add(argType);
		}

		MethodSymbol resolvedMethod = null;
		String methodName = null;

		if(callee instanceof IdentifierExpression)
		{
			IdentifierExpression idCallee = (IdentifierExpression) callee;
			methodName = idCallee.getName().getLexeme();

			// Resolution order for simple method name:
			// 1. In current class (implicit 'this.')
			if(currentClass != null)
			{
				Symbol memberCandidate = currentClass.resolveMember(methodName, actualArgumentTypes);
				if(memberCandidate instanceof MethodSymbol)
				{
					resolvedMethod = (MethodSymbol) memberCandidate;
					// Check if calling an instance method from a static context
					if(!resolvedMethod.isStatic() && inStaticContext)
					{
						error(idCallee.getName(), "Cannot call non-static method '" + methodName + "' from a static context without an instance.");
						return ErrorType.INSTANCE;
					}
				}
			}

			// 2. From single static imports
			if(resolvedMethod == null && importedStaticMembers.containsKey(methodName))
			{
				Symbol member = importedStaticMembers.get(methodName);
				if(member instanceof MethodSymbol)
				{
					// We must still check if the arguments match this specific imported overload
					if(((MethodSymbol) member).matchesArguments(actualArgumentTypes))
					{
						resolvedMethod = (MethodSymbol) member;
					}
				}
			}

			// 3. From wildcard static imports
			if(resolvedMethod == null)
			{
				for(ClassSymbol staticClass : importedStaticClasses)
				{
					Symbol memberCandidate = staticClass.resolveMember(methodName, actualArgumentTypes);
					if(memberCandidate instanceof MethodSymbol && memberCandidate.isStatic())
					{
						resolvedMethod = (MethodSymbol) memberCandidate;
						break; // Found a match
					}
				}
			}

			if(resolvedMethod == null)
			{
				error(idCallee.getName(), "Identifier '" + methodName + "' is not an invocable method in this scope or via static import with matching arguments: (" + actualArgumentTypes.stream().map(Type::getName).collect(Collectors.joining(", ")) + ").");
				return ErrorType.INSTANCE;
			}

		}
		else if(callee instanceof DotExpression)
		{
			// ... The existing logic for DotExpression in visitCallExpression is largely correct ...
			// and will benefit from the improved visitDotExpression. No major changes needed here.
			// I'm including the original logic here for completeness.
			DotExpression dotCallee = (DotExpression) callee;
			methodName = dotCallee.getMemberName().getLexeme();

			Type leftType = dotCallee.getLeft().accept(this);
			if(leftType instanceof ErrorType)
				return ErrorType.INSTANCE;

			ClassSymbol ownerClass = null;
			// Retrieve the resolved symbol of the left part of the dot expression.
			Symbol leftResolvedSymbol = dotCallee.getLeft().getResolvedSymbol();


			if(leftType instanceof ClassType)
			{
				ownerClass = ((ClassType) leftType).getClassSymbol();
			}
			// If leftType is null, it might be a namespace segment (e.g. `System.Console.WriteLine`).
			// In this scenario, `leftResolvedSymbol` should ideally be a ClassSymbol (like for `System.Console`).
			else if(leftType == null && leftResolvedSymbol instanceof ClassSymbol)
			{
				ownerClass = (ClassSymbol) leftResolvedSymbol;
			}
			else
			{
				error(dotCallee.getLeft().getFirstToken(), "Cannot call methods on type '" + (leftType != null ? leftType.getName() : "null/unresolved") + "'. Expected a class or object instance, or a valid namespace/class prefix.");
				return ErrorType.INSTANCE;
			}

			if(ownerClass == null)
			{
				error(dotCallee.getLeft().getFirstToken(), "Internal error: Could not determine containing class for method call.");
				return ErrorType.INSTANCE;
			}

			Symbol memberCandidate = ownerClass.resolveMember(methodName, actualArgumentTypes);
			if(memberCandidate instanceof MethodSymbol)
			{
				resolvedMethod = (MethodSymbol) memberCandidate;
			}

			if(resolvedMethod == null)
			{
				error(dotCallee.getMemberName(), "Method '" + methodName + "' with arguments (" + actualArgumentTypes.stream().map(Type::getName).collect(Collectors.joining(", ")) + ") not found in class '" + ownerClass.getName() + "'.");
				return ErrorType.INSTANCE;
			}

			// Check for static vs. instance access consistency.
			// `isStaticAccess` means the left-hand side of the dot expression resolved to a ClassSymbol (e.g., `ClassName.staticMethod`).
			boolean isStaticAccess = leftResolvedSymbol instanceof ClassSymbol;

			if(!resolvedMethod.isStatic() && isStaticAccess)
			{
				error(dotCallee.getMemberName(), "Cannot call non-static method '" + methodName + "' from a static context (via class name).");
				return ErrorType.INSTANCE;
			}

		}
		else
		{
			error(expression.getParen(), "Invalid expression for method call. Expected identifier or member access.");
			return ErrorType.INSTANCE;
		}

		if(resolvedMethod == null)
		{
			return ErrorType.INSTANCE; // Should have been caught earlier, but as a safeguard.
		}

		// Set the resolved symbol on the callee expression for later stages (e.g. code generation)
		if(callee instanceof IdentifierExpression)
			((IdentifierExpression) callee).setResolvedSymbol(resolvedMethod);
		if(callee instanceof DotExpression)
			((DotExpression) callee).setResolvedSymbol(resolvedMethod);

		return resolvedMethod.getType();
	}

	@Override
	public Type visitDotExpression(DotExpression expression)
	{
		// First, resolve the left-hand side of the dot. This could be an object instance, a class name, or a namespace prefix.
		Type leftType = expression.getLeft().accept(this);
		Symbol leftResolvedSymbol = expression.getLeft().getResolvedSymbol(); // Crucial: get the resolved symbol here

		if(leftType instanceof ErrorType)
		{
			return ErrorType.INSTANCE;
		}

		ClassSymbol containerClassSymbol = null;
		Symbol resolvedMemberSymbol = null; // The symbol representing the member (field, method, nested class)

		if(leftType instanceof ClassType)
		{
			// Left side is a class instance or a class type itself (e.g., `myObject.field` or `MyClass.staticField`)
			containerClassSymbol = ((ClassType) leftType).getClassSymbol();
		}
		else if(leftType == null && leftResolvedSymbol instanceof ClassSymbol)
		{
			// This path is for cases like `Namespace.Class.member` where `Namespace.Class`
			// was resolved as a ClassSymbol in `visitIdentifierExpression` or a previous `visitDotExpression`.
			containerClassSymbol = (ClassSymbol) leftResolvedSymbol;
		}
		// Removed `leftResolvedSymbol instanceof NamespaceSymbol` path as we're not using NamespaceSymbol.
		// If leftType is null and leftResolvedSymbol is also null (and not a ClassSymbol), it implies
		// the left side is an unresolvable part of an FQN or an invalid expression.
		else if(leftType instanceof ArrayType)
		{
			String memberName = expression.getMemberName().getLexeme();
			if(memberName.equals("length"))
			{
				VariableSymbol lengthSymbol = new VariableSymbol("length", PrimitiveType.INT, expression.getMemberName(), true, true, true, true);
				expression.setResolvedSymbol(lengthSymbol); // Set resolved symbol for 'length'
				return PrimitiveType.INT;
			}
			else
			{
				error(expression.getMemberName(), "Member '" + memberName + "' not found for array type. Only 'length' is supported.");
				return ErrorType.INSTANCE;
			}
		}
		else
		{
			error(expression.getLeft().getFirstToken(), "Cannot access members on type '" + (leftType != null ? leftType.getName() : "null/unresolved") + "'. Expected a class, object instance, or valid namespace/class prefix.");
			return ErrorType.INSTANCE;
		}

		if(containerClassSymbol == null)
		{
			// This path is reached if `leftType` was ClassType but `getClassSymbol()` was null, or other unhandled cases.
			error(expression.getLeft().getFirstToken(), "Internal error: Could not determine containing class for member access on '" + expression.getLeft().toString() + "'.");
			return ErrorType.INSTANCE;
		}

		// Resolve the member within the containerClassSymbol's scope
		String memberName = expression.getMemberName().getLexeme();
		resolvedMemberSymbol = containerClassSymbol.resolveMember(memberName);

		if(resolvedMemberSymbol == null)
		{
			error(expression.getMemberName(), "Member '" + memberName + "' not found in class '" + containerClassSymbol.getName() + "'.");
			expression.setResolvedSymbol(null); // Explicitly set null if not found
			return ErrorType.INSTANCE;
		}

		// Check for static vs. instance access consistency
		// If the left part resolved to a ClassSymbol itself, it's a static access.
		boolean isStaticAccess = leftResolvedSymbol instanceof ClassSymbol;

		if(!resolvedMemberSymbol.isStatic() && isStaticAccess)
		{
			error(expression.getMemberName(), "Cannot access non-static member '" + memberName + "' from a static context (via class name).");
			return ErrorType.INSTANCE;
		}
		// Special check: accessing static member via instance (e.g., `myObject.staticField`) is often allowed but warned.
		// For now, we'll allow it if the member is indeed static.
		if(resolvedMemberSymbol.isStatic() && !(leftResolvedSymbol instanceof ClassSymbol))
		{
			// This means a static member is being accessed via an instance.
			// You might want to add a warning here: error(expression.getMemberName(), "Warning: Static member '" + memberName + "' accessed via an instance.");
		}


		expression.setResolvedSymbol(resolvedMemberSymbol); // Set the resolved symbol for the DotExpression
		return resolvedMemberSymbol.getType();
	}


	@Override
	public Type visitThisExpression(ThisExpression expression)
	{
		if(currentClass == null || inStaticContext)
		{
			error(expression.getKeyword(), "The 'this' keyword cannot be used in a static context or outside a class.");
			return ErrorType.INSTANCE;
		}
		expression.setResolvedSymbol(currentClass); // Set resolved symbol to current ClassSymbol
		return currentClass.getType();
	}

	@Override
	public Type visitNewExpression(NewExpression expression)
	{
		String classNameString = getQualifiedNameFromExpression(expression.getClassName());
		if(classNameString == null)
		{
			error(expression.getNewKeyword(), "Invalid class name in 'new' expression.");
			return ErrorType.INSTANCE;
		}

		ClassSymbol classSymbol = null;
		String fqn = getFullyQualifiedClassName(classNameString);
		if(declaredClasses.containsKey(fqn))
		{
			classSymbol = declaredClasses.get(fqn);
		}

		if(classSymbol == null)
		{
			error(expression.getNewKeyword(), "Undefined class: '" + classNameString + "'. Ensure the namespace is imported or the type is fully qualified.");
			return ErrorType.INSTANCE;
		}

		List<Type> actualArgumentTypes = new ArrayList<>();
		for(Expression arg : expression.getArguments())
		{
			Type argType = arg.accept(this);
			if(argType instanceof ErrorType)
				return ErrorType.INSTANCE;
			actualArgumentTypes.add(argType);
		}

		// Use ClassSymbol.resolveMember for constructor lookup with argument types
		Symbol resolvedSymbol = classSymbol.resolveMember(classSymbol.getName(), actualArgumentTypes);
		MethodSymbol matchingConstructor = null;

		if(resolvedSymbol instanceof MethodSymbol && ((MethodSymbol) resolvedSymbol).isConstructor())
		{
			matchingConstructor = (MethodSymbol) resolvedSymbol;
		}

		if(matchingConstructor == null)
		{
			StringBuilder argTypesBuilder = new StringBuilder();
			for(int i = 0; i < actualArgumentTypes.size(); i++)
			{
				argTypesBuilder.append(actualArgumentTypes.get(i).getName());
				if(i < actualArgumentTypes.size() - 1)
				{
					argTypesBuilder.append(", ");
				}
			}
			String argTypesString = argTypesBuilder.toString();

			error(expression.getNewKeyword(), "No matching constructor found for class '" + classNameString + "' with arguments (" + argTypesString + ").");
			return ErrorType.INSTANCE;
		}

		expression.setResolvedConstructor(matchingConstructor);
		expression.setResolvedType(classSymbol.getType());
		return classSymbol.getType();
	}

	@Override
	public Type visitPostfixUnaryExpression(PostfixUnaryExpression expression)
	{
		Type operandType = expression.getOperand().accept(this);
		Type resultType = getUnaryExpressionResultType(expression.getOperator(), operandType);

		if((operandType instanceof ErrorType))
			return ErrorType.INSTANCE;

		if(!(expression.getOperand() instanceof IdentifierExpression || expression.getOperand() instanceof DotExpression || expression.getOperand() instanceof ArrayAccessExpression))
		{
			error(expression.getOperator(), "Postfix increment/decrement operators '++' and '--' must be applied to a variable, property, or array element.");
			return ErrorType.INSTANCE;
		}

		// Check for const and mark as initialized
		Symbol operandSymbol = null;
		if(expression.getOperand() instanceof IdentifierExpression)
		{
			operandSymbol = ((IdentifierExpression) expression.getOperand()).getResolvedSymbol();
		}
		else if(expression.getOperand() instanceof DotExpression)
		{
			operandSymbol = ((DotExpression) expression.getOperand()).getResolvedSymbol();
		}
		else if(expression.getOperand() instanceof ArrayAccessExpression)
		{
			// If array element, check if the base array/string is const.
			Expression baseArray = ((ArrayAccessExpression) expression.getOperand()).getArray();
			if(baseArray instanceof IdentifierExpression)
			{
				operandSymbol = ((IdentifierExpression) baseArray).getResolvedSymbol();
			}
			else if(baseArray instanceof DotExpression)
			{
				operandSymbol = ((DotExpression) baseArray).getResolvedSymbol();
			}
			// operandSymbol now refers to the array/string variable, not the element, which is correct for const check
		}


		if(operandSymbol instanceof VariableSymbol)
		{
			VariableSymbol varSym = (VariableSymbol) operandSymbol;
			if(varSym.isConst())
			{
				error(expression.getOperator(), "Cannot increment/decrement constant variable/field '" + varSym.getName() + "'.");
				return ErrorType.INSTANCE;
			}
			varSym.setInitialized(true); // Mark as initialized after modification
		}
		// The ArrayAccessExpression const check logic was redundant here as it's now handled by the general
		// `operandSymbol instanceof VariableSymbol` check if the base of the array access is a variable/field.


		return resultType;
	}

	@Override
	public Type visitSwitchStatement(SwitchStatement statement)
	{
		Type switchExprType = statement.getSwitchExpression().accept(this);
		if(switchExprType instanceof ErrorType)
			return ErrorType.INSTANCE;

		if(switchExprType != null &&
				!(switchExprType.equals(PrimitiveType.INT) ||
						switchExprType.equals(PrimitiveType.CHAR) ||
						switchExprType.equals(PrimitiveType.BYTE) ||
						switchExprType.equals(PrimitiveType.BOOL)))
		{
			error(statement.getSwitchKeyword(),
					"Switch expression must be of an integral type (int, char, byte, bool). Found '" + switchExprType.getName() + "'.");
			return PrimitiveType.VOID;
		}

		for(SwitchCase switchCase : statement.getCases())
		{
			switchCase.accept(this);
		}

		if(statement.getDefaultBlock() != null)
		{
			statement.getDefaultBlock().accept(this);
		}
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitSwitchCase(SwitchCase switchCase)
	{
		Type caseValueType = switchCase.getValue().accept(this);
		if(caseValueType instanceof ErrorType)
			return ErrorType.INSTANCE;

		if(!(switchCase.getValue() instanceof LiteralExpression)) // Or a const variable/expression
		{
			error(switchCase.getCaseKeyword(), "Switch case value must be a constant expression.");
		}
		// Further checks for type compatibility with the switch expression happen in visitSwitchStatement

		for(com.juanpa.nebula.transpiler.ast.statements.Statement stmt : switchCase.getBody())
		{
			stmt.accept(this);
		}
		return PrimitiveType.VOID;
	}

	@Override
	public Type visitArrayAccessExpression(ArrayAccessExpression expression)
	{
		Type arrayType = expression.getArray().accept(this);
		Type indexType = expression.getIndex().accept(this);

		if(arrayType instanceof ErrorType || indexType instanceof ErrorType)
		{
			return ErrorType.INSTANCE;
		}

		Type elementType = null;
		if(arrayType instanceof ArrayType)
		{
			elementType = ((ArrayType) arrayType).getElementType();
		}
		else
		{
			ClassSymbol stringClassSymbol = declaredClasses.get("nebula.core.String");
			if(stringClassSymbol != null && arrayType.equals(stringClassSymbol.getType()))
			{
				elementType = PrimitiveType.CHAR;
			}
			else
			{
				error(expression.getArray().getFirstToken(), "Cannot apply array access (operator '[]') to non-array or non-string type '" + arrayType.getName() + "'.");
				return ErrorType.INSTANCE;
			}
		}

		if(!(indexType.equals(PrimitiveType.INT) || indexType.equals(PrimitiveType.BYTE) || indexType.equals(PrimitiveType.CHAR)))
		{
			error(expression.getIndex().getFirstToken(), "Array index must be an integral type (int, byte, or char). Found '" + indexType.getName() + "'.");
			return ErrorType.INSTANCE;
		}

		// Check initialization status of the array variable/field (if applicable)
		// This is now done by checking the `resolvedSymbol` of the base expression.
		Expression baseArray = expression.getArray();
		Symbol baseArraySymbol = null;
		if(baseArray instanceof IdentifierExpression)
		{
			baseArraySymbol = ((IdentifierExpression) baseArray).getResolvedSymbol();
		}
		else if(baseArray instanceof DotExpression)
		{
			baseArraySymbol = ((DotExpression) baseArray).getResolvedSymbol();
		}
		else if(baseArray instanceof ThisExpression)
		{ // `this[idx]` should resolve `this`
			baseArraySymbol = ((ThisExpression) baseArray).getResolvedSymbol();
		}

		if(baseArraySymbol instanceof VariableSymbol)
		{
			VariableSymbol varSymbol = (VariableSymbol) baseArraySymbol;
			if(!varSymbol.isInitialized())
			{
				error(baseArray.getFirstToken(), "Array/String variable '" + varSymbol.getName() + "' might not have been initialized.");
			}
		}

		return elementType;
	}

	@Override
	public Type visitGroupingExpression(GroupingExpression expression)
	{
		return expression.getExpression().accept(this);
	}

	@Override
	public Type visitIsExpression(IsExpression expression)
	{
		Type leftType = expression.getLeft().accept(this);
		if(leftType instanceof ErrorType)
		{
			return ErrorType.INSTANCE;
		}

		Type targetType = getTypeFromToken(expression.getTypeToken());
		if(targetType instanceof ErrorType)
		{
			error(expression.getTypeToken(), "Expected a valid type name on the right side of 'is' operator. Found '" + expression.getTypeToken().getLexeme() + "'.");
			return ErrorType.INSTANCE;
		}

		if(!(leftType instanceof ClassType || leftType instanceof ArrayType))
		{
			error(expression.getLeft().getFirstToken(), "Left side of 'is' operator must be an object or array type (found '" + leftType.getName() + "'). Primitive types cannot be checked with 'is'.");
			return ErrorType.INSTANCE;
		}

		if(!(targetType instanceof ClassType || targetType instanceof ArrayType))
		{
			error(expression.getTypeToken(), "Right side of 'is' operator must be a class or array type (found '" + targetType.getName() + "').");
			return ErrorType.INSTANCE;
		}

		return PrimitiveType.BOOL;
	}

	/**
	 * Helper to resolve a simple or partial name against imported namespaces and classes
	 */
	private String resolveAgainstImportedNamespaces(String simpleOrPartialName)
	{
		// 1. Check if the simple name directly resolves to an imported class
		if(importedClasses.containsKey(simpleOrPartialName))
		{
			return importedClasses.get(simpleOrPartialName).getType().getName();
		}

		// 2. Iterate through imported namespaces and try to resolve
		for(String ns : importedNamespaces)
		{
			String potentialFQN = ns + "." + simpleOrPartialName;
			if(declaredClasses.containsKey(potentialFQN))
			{
				return potentialFQN;
			}
		}

		// 3. Check against static imported classes (e.g., if simpleOrPartialName is a static method name)
		// This part needs more specific logic depending on how you handle static imports.
		// For now, if simpleOrPartialName is "Console", and "Nebula.System.Console" was imported statically:
		for(ClassSymbol staticClass : importedStaticClasses)
		{
			String potentialFQN = staticClass.getType().getName();
			if(potentialFQN.endsWith("." + simpleOrPartialName))
			{
				return potentialFQN; // This implies importing the class itself via static import
			}
		}

		// 4. Try to resolve as a direct sub-namespace of an imported namespace (e.g., if "System" is imported,
		// and we're looking for "Text.Console" where "Text" is a sub-namespace).
		// This requires more complex namespace symbol table management. For now, we assume direct class resolution.

		return null; // Not resolved
	}

	/**
	 * Extracts the simple name (the part after the last dot) from a fully qualified name.
	 * For "Nebula.System.Console", returns "Console". For "String", returns "String".
	 *
	 * @param fqn The fully qualified name.
	 * @return The simple name.
	 */
	private String getSimpleNameFromQualifiedName(String fqn)
	{
		if(fqn == null || fqn.isEmpty())
		{
			return fqn;
		}
		int lastDotIndex = fqn.lastIndexOf('.');
		if(lastDotIndex == -1)
		{
			return fqn; // No dot, it's already a simple name
		}
		return fqn.substring(lastDotIndex + 1);
	}

	// Inside SemanticAnalyzer.java
	public Map<String, ClassSymbol> getDeclaredClasses()
	{
		return declaredClasses; // Or Collections.unmodifiableMap(declaredClasses) for immutability
	}

	// You might also need these, depending on CppGenerator's needs:
	public Map<String, ClassSymbol> getImportedClasses()
	{
		return importedClasses;
	}

	public List<String> getImportedNamespaces()
	{
		return importedNamespaces;
	}

	public Map<String, Symbol> getImportedStaticMembers()
	{
		return importedStaticMembers;
	}

	public List<ClassSymbol> getImportedStaticClasses()
	{
		return importedStaticClasses;
	}
}
