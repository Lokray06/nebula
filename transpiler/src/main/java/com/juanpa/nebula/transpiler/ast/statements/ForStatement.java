// File: src/main/java/com/juanpa.nebula.transpiler/ast/statements/ForStatement.java
package com.juanpa.nebula.transpiler.ast.statements;

import com.juanpa.nebula.transpiler.ast.ASTVisitor; // Import ASTVisitor
import com.juanpa.nebula.transpiler.ast.expressions.Expression;

/**
 * AST node representing a 'for' loop statement.
 * Includes an optional initializer, condition, increment expression, and a loop body.
 */
public class ForStatement implements Statement
{
	private final Statement initializer; // Can be a VariableDeclarationStatement or ExpressionStatement (or null)
	private final Expression condition;  // The loop continuation condition (can be null for infinite loop)
	private final Expression increment;  // The expression executed after each iteration (can be null)
	private final BlockStatement body;   // The loop body

	public ForStatement(Statement initializer, Expression condition, Expression increment, BlockStatement body)
	{
		this.initializer = initializer;
		this.condition = condition;
		this.increment = increment;
		this.body = body;
	}

	public Statement getInitializer()
	{
		return initializer;
	}

	public Expression getCondition()
	{
		return condition;
	}

	public Expression getIncrement()
	{
		return increment;
	}

	public BlockStatement getBody()
	{
		return body;
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder("For (");
		sb.append(initializer != null ? initializer.toString().trim() : "");
		sb.append("; ").append(condition != null ? condition.toString() : "");
		sb.append("; ").append(increment != null ? increment.toString() : "");
		sb.append(") {\n");
		sb.append(indent(body.toString(), 1)); // Indent the body
		sb.append("}\n");
		return sb.toString();
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitForStatement(this);
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

