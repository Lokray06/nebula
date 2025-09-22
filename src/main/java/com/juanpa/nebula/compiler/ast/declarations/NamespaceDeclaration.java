// File: src/main/java/com/juanpa.nebula.transpiler/ast/declarations/NamespaceDeclaration.java

package com.juanpa.nebula.compiler.ast.declarations;

import com.juanpa.nebula.compiler.ast.ASTNode;
import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.ast.expressions.Expression;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * AST node representing a namespace declaration.
 * Holds the qualified name of the namespace and a list of classes declared within it.
 */
public class NamespaceDeclaration implements ASTNode
{
	private final Expression nameExpression; // Represents the qualified name (e.g., Com.MyCompany.App)
	private final List<ClassDeclaration> classes; // Stores class declarations within this namespace

	/**
	 * Constructor for NamespaceDeclaration.
	 * @param nameExpression The expression representing the qualified name of the namespace.
	 */
	public NamespaceDeclaration(Expression nameExpression)
	{
		this.nameExpression = nameExpression;
		this.classes = new ArrayList<>();
	}

	/**
	 * Returns the expression representing the qualified name of the namespace.
	 * This is used by the SemanticAnalyzer to resolve the full namespace path.
	 * @return The Expression node for the namespace's name.
	 */
	public Expression getNameExpression()
	{
		return nameExpression;
	}

	/**
	 * Returns an unmodifiable list of class declarations within this namespace.
	 * This method name is chosen to be consistent with SemanticAnalyzer's usage.
	 * @return A list of ClassDeclaration AST nodes.
	 */
	public List<ClassDeclaration> getClassDeclarations() // Corrected method name for consistency
	{
		return Collections.unmodifiableList(classes);
	}

	/**
	 * Adds a ClassDeclaration to this namespace.
	 * @param classDecl The ClassDeclaration to add.
	 */
	public void addClass(ClassDeclaration classDecl)
	{
		this.classes.add(classDecl);
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitNamespaceDeclaration(this);
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("  Namespace '").append(nameExpression).append("' {\n");
		for (ClassDeclaration classDecl : classes)
		{
			// Indent the class declarations for better readability in AST printout
			sb.append(classDecl.toString().replace("\n", "\n  ")).append("\n");
		}
		sb.append("  }");
		return sb.toString();
	}
}
