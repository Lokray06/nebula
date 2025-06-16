// File: src/main/java/com/juanpa.nebula.transpiler/ast/statements/BlockStatement.java
package com.juanpa.nebula.transpiler.ast.statements;

import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.ast.ASTVisitor;

import java.util.ArrayList;
import java.util.List;

/**
 * AST node representing a block of statements enclosed in curly braces {}.
 * This is used for method bodies, if/else blocks, loop bodies, etc.
 */
public class BlockStatement implements Statement
{
	private final List<Statement> statements;

	public BlockStatement()
	{
		this.statements = new ArrayList<>();
	}

	/**
	 * Constructor to create a BlockStatement from a pre-existing list of statements.
	 *
	 * @param statements The list of statements to include in this block.
	 */
	public BlockStatement(List<Statement> statements)
	{
		this.statements = new ArrayList<>(statements); // Defensive copy
	}


	public void addStatement(Statement statement)
	{
		this.statements.add(statement);
	}

	public List<Statement> getStatements()
	{
		return statements;
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder("{\n");
		for(Statement stmt : statements)
		{
			// Indent statements within the block
			sb.append(indent(stmt.toString(), 1));
		}
		sb.append("}");
		return sb.toString();
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitBlockStatement(this);
	}

	// Helper for indentation
	private String indent(String text, int level)
	{
		StringBuilder indentedText = new StringBuilder();
		String prefix = "  ".repeat(level);
		for(String line : text.split("\n"))
		{
			indentedText.append(prefix).append(line).append("\n");
		}
		return indentedText.toString();
	}
}
