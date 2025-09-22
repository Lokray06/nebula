// File: src/main/java/com/juanpa/nebula/transpiler/ast/expressions/ArrayInitializerExpression.java
package com.juanpa.nebula.compiler.ast.expressions;

import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.lexer.Token;
import com.juanpa.nebula.compiler.semantics.Symbol;
import com.juanpa.nebula.compiler.semantics.Type;

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