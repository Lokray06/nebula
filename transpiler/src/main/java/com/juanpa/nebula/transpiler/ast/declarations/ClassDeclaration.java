// File: src/main/java/com/juanpa/nebula/transpiler/ast/declarations/ClassDeclaration.java

package com.juanpa.nebula.transpiler.ast.declarations;

import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.expressions.IdentifierExpression;
import com.juanpa.nebula.transpiler.lexer.Token;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * AST node representing a class declaration.
 * Holds the class's name, its modifiers, optional superclass, and lists of its members (fields, constructors, methods).
 */
public class ClassDeclaration implements ASTNode
{
	private final Token name;
	private final List<Token> modifiers; // e.g., PUBLIC, PRIVATE, STATIC, etc.
	private final Token classKeyword; // The 'class' keyword token itself
	private final Token extendsKeyword; // The 'extends' keyword token, null if no extends clause
	private final Token superClassName; // The superclass name token, null if no extends clause
	private final Token leftBrace; // The '{' token opening the class body
	private final Token rightBrace; // The '}' token closing the class body

	private final List<FieldDeclaration> fields;
	private final List<ConstructorDeclaration> constructors;
	private final List<MethodDeclaration> methods;

	// NEW: Field to store the fully qualified name of the containing namespace
	private String containingNamespace;

	// Original constructor (keep for compatibility if needed elsewhere, but typically won't be used by parser)
	public ClassDeclaration(Token name)
	{
		this.name = name;
		this.modifiers = new ArrayList<>();
		this.classKeyword = null; // Default to null for this constructor
		this.extendsKeyword = null;
		this.superClassName = null;
		this.leftBrace = null;
		this.rightBrace = null;
		this.fields = new ArrayList<>();
		this.constructors = new ArrayList<>();
		this.methods = new ArrayList<>();
		this.containingNamespace = ""; // Initialize to empty string by default
	}

	/**
	 * Comprehensive constructor for ClassDeclaration used by the parser.
	 *
	 * @param modifiers           The list of modifiers (public, private, static, etc.) for the class.
	 * @param classKeyword        The 'class' keyword token.
	 * @param name                The identifier token for the class name.
	 * @param extendsKeyword      The 'extends' keyword token (can be null if not present).
	 * @param superClassName      The identifier token for the superclass name (can be null if not present).
	 * @param leftBrace           The '{' token opening the class body.
	 * @param fields              List of field declarations within the class.
	 * @param methods             List of method declarations within the class.
	 * @param constructors        List of constructor declarations within the class.
	 * @param rightBrace          The '}' token closing the class body.
	 * @param containingNamespace The fully qualified name of the namespace this class belongs to.
	 */
	public ClassDeclaration(List<Token> modifiers, Token classKeyword, Token name,
							Token extendsKeyword, Token superClassName,
							Token leftBrace, List<FieldDeclaration> fields,
							List<MethodDeclaration> methods, List<ConstructorDeclaration> constructors,
							Token rightBrace, String containingNamespace) // NEW: Added containingNamespace parameter
	{
		this.modifiers = modifiers;
		this.classKeyword = classKeyword;
		this.name = name;
		this.extendsKeyword = extendsKeyword;
		this.superClassName = superClassName;
		this.leftBrace = leftBrace;
		this.rightBrace = rightBrace;
		this.fields = fields;
		this.constructors = constructors;
		this.methods = methods;
		this.containingNamespace = containingNamespace; // NEW: Set the field
	}

	public Token getName()
	{
		return name;
	}

	public Token getExtendsKeyword()
	{
		return extendsKeyword;
	}

	public Token getSuperClassName()
	{
		return superClassName;
	}

	/**
	 * Returns the class name as an IdentifierExpression.
	 * Useful for semantic analysis which expects Expressions for name resolution.
	 */
	public IdentifierExpression getNameExpression()
	{
		return new IdentifierExpression(name);
	}

	public List<Token> getModifiers()
	{
		return Collections.unmodifiableList(modifiers);
	}

	public List<FieldDeclaration> getFields()
	{
		return Collections.unmodifiableList(fields);
	}

	public List<ConstructorDeclaration> getConstructors()
	{
		return Collections.unmodifiableList(constructors);
	}

	public List<MethodDeclaration> getMethods()
	{
		return Collections.unmodifiableList(methods);
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitClassDeclaration(this);
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		modifiers.forEach(mod -> sb.append(mod.getLexeme()).append(" "));
		sb.append("class ").append(name.getLexeme());
		if(superClassName != null)
		{
			sb.append(" extends ").append(superClassName.getLexeme());
		}
		sb.append(" {\n");
		// Append fields
		for(FieldDeclaration field : fields)
		{
			sb.append("    ").append(field.toString().replace("\n", "\n    ")).append("\n");
		}
		// Append constructors
		for(ConstructorDeclaration constructor : constructors)
		{
			sb.append("    ").append(constructor.toString().replace("\n", "\n    ")).append("\n");
		}
		// Append methods
		for(MethodDeclaration method : methods)
		{
			sb.append("    ").append(method.toString().replace("\n", "\n    ")).append("\n");
		}
		sb.append("  }");
		return sb.toString();
	}

	/**
	 * Returns the first token of this ClassDeclaration, which is typically the 'class' keyword.
	 * Useful for error reporting.
	 *
	 * @return The first Token of the class declaration.
	 */
	public Token getFirstToken()
	{
		return classKeyword;
	}

	// NEW: Getter for the containing namespace
	public String getContainingNamespace()
	{
		return containingNamespace;
	}

	// NEW: Setter for the containing namespace (useful for AST building or semantic enrichment)
	public void setContainingNamespace(String containingNamespace)
	{
		this.containingNamespace = containingNamespace;
	}
}
