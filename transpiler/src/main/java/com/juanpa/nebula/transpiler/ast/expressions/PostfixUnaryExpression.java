// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/PostfixUnaryExpression.java

package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type; // Import Type

/**
 * AST node representing a postfix unary operation (e.g., x++, y--).
 * It has an operand expression and a postfix operator token.
 */
public class PostfixUnaryExpression implements Expression
{
	private final Expression operand; // The expression being operated on (must be assignable)
	private final Token operator;    // The postfix operator token (e.g., PLUS_PLUS, MINUS_MINUS)
	private Type resolvedType; // NEW: Field for resolved type

	public PostfixUnaryExpression(Expression operand, Token operator)
	{
		this.operand = operand;
		this.operator = operator;
	}

	public Expression getOperand()
	{
		return operand;
	}

	public Token getOperator()
	{
		return operator;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitPostfixUnaryExpression(this);
	}

	@Override
	public String toString()
	{
		return "(" + operand + operator.getLexeme() + ")";
	}

	@Override
	public Token getFirstToken()
	{
		return operand.getFirstToken(); // The first token is the first token of the operand
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return null; // Postfix expressions don't directly resolve to a symbol, but modify one.
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not directly applicable for this expression type itself.
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType()
	{
		// The type of a postfix unary expression is the type of its operand.
		return operand.getResolvedType();
	}

	@Override
	public void setResolvedType(Type resolvedType)
	{
		this.resolvedType = resolvedType;
	}
}
