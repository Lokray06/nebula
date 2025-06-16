// File: src/main/java/com/juanpa/nebula/transpiler/ast/Program.java

package com.juanpa.nebula.transpiler.ast;

import com.juanpa.nebula.transpiler.ast.declarations.NamespaceDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.ImportDirective; // Changed: Import ImportDirective

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * The root AST node representing an entire Nebula program.
 * Contains a list of namespace declarations.
 */
public class Program implements ASTNode
{
	private final List<ImportDirective> importDirectives; // Changed: List of import directives
	private final List<NamespaceDeclaration> namespaceDeclarations; // List of namespace declarations

	public Program()
	{
		this.importDirectives = new ArrayList<>(); // Initialize the list
		this.namespaceDeclarations = new ArrayList<>();
	}

	public List<ImportDirective> getImportDirectives()
	{ // Changed: Getter for import directives
		return Collections.unmodifiableList(importDirectives);
	}

	public void addImportDirective(ImportDirective directive)
	{ // Changed: Add import directive
		this.importDirectives.add(directive);
	}

	public List<NamespaceDeclaration> getNamespaceDeclarations()
	{
		return Collections.unmodifiableList(namespaceDeclarations);
	}

	public void addNamespace(NamespaceDeclaration namespaceDecl)
	{
		this.namespaceDeclarations.add(namespaceDecl);
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitProgram(this);
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		for(ImportDirective directive : importDirectives)
		{
			sb.append(directive.toString()).append("\n");
		}
		if(!importDirectives.isEmpty() && !namespaceDeclarations.isEmpty())
		{
			sb.append("\n"); // Add a newline to separate imports from namespaces
		}
		for(NamespaceDeclaration namespaceDecl : namespaceDeclarations)
		{
			sb.append(namespaceDecl.toString()).append("\n");
		}
		return sb.toString();
	}
}
