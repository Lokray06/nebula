// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/UnaryExpression.java

package com.juanpa.nebula.compiler.ast.expressions;

import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.lexer.Token;
import com.juanpa.nebula.compiler.semantics.Symbol;
import com.juanpa.nebula.compiler.semantics.Type; // Import Type

/**
 * AST node representing a unary operation (e.g., !a, -b).
 * It has an operator token and a single operand expression.
 */
public class UnaryExpression implements Expression
{
	private final Token operator; // The unary operator token (e.g., BANG, MINUS)
	private final Expression right; // The operand expression
	private Type resolvedType; // NEW: Field for resolved type

	public UnaryExpression(Token operator, Expression right)
	{
		this.operator = operator;
		this.right = right;
	}

	public Token getOperator()
	{
		return operator;
	}

	public Expression getRight()
	{
		return right;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitUnaryExpression(this);
	}

	@Override
	public String toString()
	{
		return "(" + operator.getLexeme() + right + ")";
	}

	@Override
	public Token getFirstToken()
	{
		return operator; // The first token of a unary expression is its operator
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return null; // Unary expressions don't directly resolve to a symbol.
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not directly applicable for this expression type itself.
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType() {
		// The type of a unary expression is typically the type of its operand.
		return resolvedType;
	}

	// Setter for resolved type
	public void setResolvedType(Type resolvedType) {
		this.resolvedType = resolvedType;
	}
}
