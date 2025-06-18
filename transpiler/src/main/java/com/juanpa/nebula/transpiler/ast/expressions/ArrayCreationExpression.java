// File: src/main/java/com/juanpa/nebula/transpiler/ast/expressions/ArrayCreationExpression.java
package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type;

/**
 * Represents an array creation expression, e.g., "new int[10]".
 */
public class ArrayCreationExpression implements Expression
{
	private final Token newKeyword;
	private final Token typeToken; // The base type of the array elements (e.g., 'int')
	private final Expression sizeExpression; // The expression inside the brackets
	private Type resolvedType;

	public ArrayCreationExpression(Token newKeyword, Token typeToken, Expression sizeExpression)
	{
		this.newKeyword = newKeyword;
		this.typeToken = typeToken;
		this.sizeExpression = sizeExpression;
	}

	public Token getTypeToken()
	{
		return typeToken;
	}

	public Expression getSizeExpression()
	{
		return sizeExpression;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		// Assumes a new visitor method is added for this node type
		// return visitor.visitArrayCreationExpression(this);
		// As a fallback, we can use a generic method if the visitor interface is not updated yet.
		// For this implementation, we will assume the visitor has the method.
		// The SemanticAnalyzer will need to be updated to have visitArrayCreationExpression.
		return null;
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

	public void setResolvedType(Type type)
	{
		this.resolvedType = type;
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return null; // This expression doesn't resolve to a single symbol
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not applicable
	}
}