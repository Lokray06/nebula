// File: src/main/java/com/juanpa/nebula/transpiler/ast/expressions/ArrayCreationExpression.java
package org.lokray.nebula.ast.expressions;

import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.semantics.Symbol;
import org.lokray.nebula.semantics.Type;

/**
 * Represents an array creation expression, e.g., "new int[10]".
 */
public class ArrayCreationExpression implements Expression
{
	private final Token newKeyword;
	private final Token typeToken; // The base type of the array elements (e.g., 'int')
	private final int rank; // The number of dimensions, e.g., 1 for int[], 2 for int[][]
	private final Expression sizeExpression; // The expression inside the brackets
	private Type resolvedType;

	public ArrayCreationExpression(Token newKeyword, Token typeToken, int rank, Expression sizeExpression)
	{
		this.newKeyword = newKeyword;
		this.typeToken = typeToken;
		this.rank = rank;
		this.sizeExpression = sizeExpression;
	}

	public Token getTypeToken()
	{
		return typeToken;
	}

	public int getRank()
	{
		return rank;
	}

	public Expression getSizeExpression()
	{
		return sizeExpression;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitArrayCreationExpression(this);
	}

	@Override
	public Token getFirstToken()
	{
		return newKeyword;
	}

	@Override
	public Type getResolvedType()
	{
		return resolvedType;
	}

	/**
	 * Sets the resolved type of this expression. Called by the SemanticAnalyzer.
	 *
	 * @param type The resolved type.
	 */
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