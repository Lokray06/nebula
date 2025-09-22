// File: src/main/java/com/juanpa.nebula.transpiler/ast/statements/ExpressionStatement.java
package org.lokray.nebula.ast.statements;

import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.ast.expressions.Expression;

/**
 * AST node representing a statement that consists solely of an expression,
 * typically followed by a semicolon (e.g., `someMethodCall();`).
 */
public class ExpressionStatement implements Statement
{
	private final Expression expression;

	public ExpressionStatement(Expression expression)
	{
		this.expression = expression;
	}

	public Expression getExpression()
	{
		return expression;
	}

	@Override
	public String toString()
	{
		return "ExprStmt: " + expression + ";\n";
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitExpressionStatement(this);
	}
}

