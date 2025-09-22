// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/ThisExpression.java

package com.juanpa.nebula.compiler.ast.expressions;

import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.lexer.Token;
import com.juanpa.nebula.compiler.lexer.TokenType;
import com.juanpa.nebula.compiler.semantics.Symbol;
import com.juanpa.nebula.compiler.semantics.Type;

/**
 * AST node representing the 'this' keyword.
 * Represents a reference to the current instance of the class.
 */
public class ThisExpression implements Expression
{
	private final Token keyword; // The 'this' keyword token
	private Symbol resolvedSymbol; // Added field for resolved symbol
	private Type resolvedType; // NEW: Field for resolved type

	public ThisExpression(Token keyword)
	{
		if(keyword.getType() != TokenType.THIS)
		{
			throw new IllegalArgumentException("Keyword for ThisExpression must be TokenType.THIS.");
		}
		this.keyword = keyword;
	}

	public Token getKeyword()
	{
		return keyword;
	}

	// NEW: Getter for resolved symbol
	@Override
	public Symbol getResolvedSymbol()
	{
		return resolvedSymbol;
	}

	// NEW: Setter for resolved symbol
	@Override
	public void setResolvedSymbol(Symbol resolvedSymbol)
	{
		this.resolvedSymbol = resolvedSymbol;
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

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitThisExpression(this);
	}

	@Override
	public String toString()
	{
		return "this";
	}

	@Override
	public Token getFirstToken()
	{
		return keyword; // The 'this' keyword itself is the first token
	}
}
