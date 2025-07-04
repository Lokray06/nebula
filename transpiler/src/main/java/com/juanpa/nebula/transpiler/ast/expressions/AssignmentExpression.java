// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/AssignmentExpression.java

package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type; // Import Type

/**
 * AST node representing an assignment operation (e.g., x = 10, obj.property = value, x += 5).
 * It holds the target expression (what's being assigned to), the operator, and the value expression.
 */
public class AssignmentExpression implements Expression
{
	private final Expression target; // The left-hand side of the assignment (e.g., IdentifierExpression, DotExpression)
	private final Token operator;    // The assignment operator token (e.g., ASSIGN, PLUS_ASSIGN, MINUS_ASSIGN)
	private final Expression value;  // The right-hand side expression
	private Type resolvedType; // NEW: Field for resolved type

	/**
	 * Constructor for AssignmentExpression.
	 *
	 * @param target   The expression representing the target of the assignment.
	 * @param operator The assignment operator token.
	 * @param value    The expression whose value is being assigned.
	 */
	public AssignmentExpression(Expression target, Token operator, Expression value)
	{
		// Basic validation for the operator type (can be expanded)
		if(!isAssignmentOperator(operator.getType()))
		{
			throw new IllegalArgumentException("Invalid token type for assignment operator: " + operator.getType());
		}
		this.target = target;
		this.operator = operator;
		this.value = value;
	}

	private boolean isAssignmentOperator(TokenType type)
	{
		return type == TokenType.ASSIGN || type == TokenType.PLUS_ASSIGN ||
				type == TokenType.MINUS_ASSIGN || type == TokenType.STAR_ASSIGN ||
				type == TokenType.SLASH_ASSIGN || type == TokenType.MODULO_ASSIGN;
	}

	public Expression getTarget()
	{
		return target;
	}

	public Token getOperator()
	{
		return operator;
	}

	public Expression getValue()
	{
		return value;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitAssignmentExpression(this);
	}

	@Override
	public String toString()
	{
		return "(" + target + " " + operator.getLexeme() + " " + value + ")";
	}

	@Override
	public Token getFirstToken()
	{
		return target.getFirstToken(); // The first token is the first token of the target expression
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return target.getResolvedSymbol(); // The symbol being assigned to
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// The symbol is set on the target, not directly on the assignment expression itself.
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType() {
		// The type of an assignment expression is the type of its target (left-hand side).
		return resolvedType;
	}

	// Setter for resolved type
	public void setResolvedType(Type resolvedType) {
		this.resolvedType = resolvedType;
	}
}
