// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/BinaryExpression.java

package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type; // Import Type

/**
 * AST node representing a binary operation (e.g., a + b, x == y, c && d).
 * It has a left operand, an operator token, and a right operand.
 */
public class BinaryExpression implements Expression {
	private final Expression left;
	private final Token operator; // The binary operator token (e.g., PLUS, MINUS, EQUAL_EQUAL)
	private final Expression right;
	private Type resolvedType; // NEW: Field for resolved type

	public BinaryExpression(Expression left, Token operator, Expression right) {
		this.left = left;
		this.operator = operator;
		this.right = right;
	}

	public Expression getLeft() {
		return left;
	}

	public Token getOperator() {
		return operator;
	}

	public Expression getRight() {
		return right;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor) {
		return visitor.visitBinaryExpression(this);
	}

	@Override
	public String toString() {
		return "(" + left + " " + operator.getLexeme() + " " + right + ")";
	}

	@Override
	public Token getFirstToken() {
		return left.getFirstToken(); // The first token of a binary expression is its left operand's first token
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return null; // Binary expressions don't directly resolve to a symbol.
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not directly applicable for this expression type itself.
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
