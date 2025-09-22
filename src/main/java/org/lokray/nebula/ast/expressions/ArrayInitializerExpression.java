// File: src/main/java/com/juanpa/nebula/transpiler/ast/expressions/ArrayInitializerExpression.java
package org.lokray.nebula.ast.expressions;

import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.semantics.Symbol;
import org.lokray.nebula.semantics.Type;

import java.util.List;

/**
 * Represents an array initializer expression, e.g., "{1, 2, 3}".
 */
public class ArrayInitializerExpression implements Expression
{
	private final Token leftBrace;
	private final List<Expression> elements;
	private Type resolvedType;

	public ArrayInitializerExpression(Token leftBrace, List<Expression> elements)
	{
		this.leftBrace = leftBrace;
		this.elements = elements;
	}

	public List<Expression> getElements()
	{
		return elements;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitArrayInitializerExpression(this);
	}

	@Override
	public Token getFirstToken()
	{
		return leftBrace;
	}

	@Override
	public Type getResolvedType()
	{
		return resolvedType;
	}

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