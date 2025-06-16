// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/IsExpression.java

package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type; // Import Type
import com.juanpa.nebula.transpiler.semantics.PrimitiveType; // Import PrimitiveType (for boolean return type)

/**
 * AST node for the 'is' type-checking operator (e.g., `expr is Type`).
 */
public class IsExpression implements Expression
{
	private final Expression left;
	private final Token isKeyword; // The 'is' keyword token
	private final Token typeToken; // The token representing the type (e.g., IDENTIFIER for class, or primitive keyword)
	private Type resolvedType; // NEW: Field for resolved type (will be boolean)


	public IsExpression(Expression left, Token isKeyword, Token typeToken)
	{
		this.left = left;
		this.isKeyword = isKeyword;
		this.typeToken = typeToken;
		this.resolvedType = PrimitiveType.BOOL; // 'is' expression always results in a boolean
	}

	public Expression getLeft()
	{
		return left;
	}

	public Token getIsKeyword()
	{
		return isKeyword;
	}

	public Token getTypeToken()
	{
		return typeToken;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		// This will call visitor.visitIsExpression(this)
		return visitor.visitIsExpression(this);
	}

	@Override
	public Token getFirstToken()
	{
		return left.getFirstToken();
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return null; // 'is' expression itself doesn't resolve to a symbol
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not applicable
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType() {
		return resolvedType;
	}

	// Setter for resolved type (SemanticAnalyzer might refine, but default to BOOL here)
	public void setResolvedType(Type resolvedType) {
		this.resolvedType = resolvedType;
	}


	@Override
	public String toString()
	{
		return "(" + left.toString() + " is " + typeToken.getLexeme() + ")";
	}
}
