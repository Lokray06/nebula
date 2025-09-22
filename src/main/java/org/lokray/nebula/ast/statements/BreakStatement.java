package org.lokray.nebula.ast.statements;

import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.lexer.Token;

/**
 * AST node representing a 'break' statement.
 */
public class BreakStatement implements Statement
{
	private final Token breakKeyword;

	public BreakStatement(Token breakKeyword)
	{
		this.breakKeyword = breakKeyword;
	}

	public Token getBreakKeyword()
	{
		return breakKeyword;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitBreakStatement(this);
	}
}