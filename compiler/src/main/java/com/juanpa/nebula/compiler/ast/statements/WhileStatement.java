// File: src/main/java/com/juanpa.nebula.transpiler/ast/statements/WhileStatement.java
package com.juanpa.nebula.compiler.ast.statements;

import com.juanpa.nebula.compiler.ast.ASTVisitor; // Import ASTVisitor
import com.juanpa.nebula.compiler.ast.expressions.Expression;

/**
 * AST node representing a 'while' loop statement.
 * Includes a condition expression and a loop body.
 */
public class WhileStatement implements Statement
{
	private final Expression condition;
	private final BlockStatement body;

	public WhileStatement(Expression condition, BlockStatement body)
	{
		this.condition = condition;
		this.body = body;
	}

	public Expression getCondition()
	{
		return condition;
	}

	public BlockStatement getBody()
	{
		return body;
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder("While (").append(condition).append(") {\n");
		sb.append(indent(body.toString(), 1)); // Indent the body
		sb.append("}\n");
		return sb.toString();
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitWhileStatement(this);
	}

	// Helper for indentation
	private String indent(String text, int level) {
		StringBuilder indentedText = new StringBuilder();
		String prefix = "  ".repeat(level);
		for (String line : text.split("\n")) {
			indentedText.append(prefix).append(line).append("\n");
		}
		return indentedText.toString();
	}
}

