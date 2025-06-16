// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/ArrayAccessExpression.java
package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type; // Import Type

// Corrected to implement the Expression interface
public class ArrayAccessExpression implements Expression
{
	private final Expression array; // The expression representing the array/string
	private final Expression index; // The expression representing the index
	private final Token rightBracket; // The ']' token
	private Symbol resolvedSymbol;
	private Type resolvedType; // NEW: Field for resolved type

	public ArrayAccessExpression(Expression array, Expression index, Token rightBracket)
	{
		// No super() call needed as Expression is an interface
		this.array = array;
		this.index = index;
		this.rightBracket = rightBracket;
	}

	public Expression getArray()
	{
		return array;
	}

	public Expression getIndex()
	{
		return index;
	}

	public Token getRightBracket()
	{
		return rightBracket;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitArrayAccessExpression(this);
	}

	@Override
	public Token getFirstToken()
	{
		// The first token of an ArrayAccessExpression is the first token of the array expression
		return array.getFirstToken();
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return resolvedSymbol;
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		resolvedSymbol = symbol;
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType() {
		return resolvedType;
	}

	// Setter for resolved type
	public void setResolvedType(Type resolvedType) {
		this.resolvedType = resolvedType;
	}
}
