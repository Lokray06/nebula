// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/IdentifierExpression.java

package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type; // Import Type

/**
 * AST node representing an identifier (e.g., a variable name, class name, method name).
 */
public class IdentifierExpression implements Expression
{
	private final Token name; // The IDENTIFIER token
	private Symbol resolvedSymbol; // Added field for resolved symbol
	private Type resolvedType;

	public IdentifierExpression(Token name)
	{
		if(name.getType() != TokenType.IDENTIFIER)
		{
			throw new IllegalArgumentException("Token for IdentifierExpression must be an IDENTIFIER.");
		}
		this.name = name;
	}

	public Token getName()
	{
		return name;
	}

	// Getter for resolved symbol
	public Symbol getResolvedSymbol()
	{
		return resolvedSymbol;
	}

	// Setter for resolved symbol
	public void setResolvedSymbol(Symbol resolvedSymbol)
	{
		this.resolvedSymbol = resolvedSymbol;
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType()
	{
		return resolvedSymbol != null ? resolvedSymbol.getType() : null;
	}

	@Override
	public void setResolvedType(Type resolvedType)
	{
		this.resolvedType = resolvedType;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitIdentifierExpression(this);
	}

	@Override
	public String toString()
	{
		return name.getLexeme();
	}

	@Override
	public Token getFirstToken()
	{
		return name;
	}
}
