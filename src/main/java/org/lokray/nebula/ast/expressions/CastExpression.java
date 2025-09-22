// File: src/main/java/com/juanpa/nebula/transpiler/ast/expressions/CastExpression.java
package org.lokray.nebula.ast.expressions;

import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.semantics.Symbol;
import org.lokray.nebula.semantics.Type;

public class CastExpression implements Expression
{
	private final Token typeToken;
	private final int rank;
	private final Expression expression;
	private Type resolvedType;

	public CastExpression(Token typeToken, int rank, Expression expression)
	{
		this.typeToken = typeToken;
		this.rank = rank;
		this.expression = expression;
	}

	public Token getTypeToken()
	{
		return typeToken;
	}

	public int getRank()
	{
		return rank;
	}

	public Expression getExpression()
	{
		return expression;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitCastExpression(this);
	}

	@Override
	public Token getFirstToken()
	{
		return typeToken;
	}

	@Override
	public Type getResolvedType()
	{
		return resolvedType;
	}

	@Override
	public void setResolvedType(Type type)
	{
		this.resolvedType = type;
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return null;
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not applicable
	}
}