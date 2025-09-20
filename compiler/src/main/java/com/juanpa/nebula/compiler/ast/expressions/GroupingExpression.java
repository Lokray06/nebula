// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/GroupingExpression.java

package com.juanpa.nebula.compiler.ast.expressions;

import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.lexer.Token;
import com.juanpa.nebula.compiler.lexer.TokenType;
import com.juanpa.nebula.compiler.semantics.Symbol;
import com.juanpa.nebula.compiler.semantics.Type; // Import Type

/**
 * AST node representing a grouped expression, enclosed in parentheses (e.g., `(a + b)`).
 * This node is primarily used to enforce operator precedence.
 */
public class GroupingExpression implements Expression
{
	private final Expression expression; // The inner expression being grouped
	private final Token leftParen;      // The opening parenthesis token

	/**
	 * Constructs a new GroupingExpression.
	 *
	 * @param leftParen  The opening parenthesis token.
	 * @param expression The inner expression enclosed by the parentheses.
	 */
	public GroupingExpression(Token leftParen, Expression expression)
	{
		if(leftParen.getType() != TokenType.LEFT_PAREN)
		{
			throw new IllegalArgumentException("Token for GroupingExpression must be TokenType.LEFT_PAREN.");
		}
		this.leftParen = leftParen;
		this.expression = expression;
	}

	public Expression getExpression()
	{
		return expression;
	}

	@SuppressWarnings("unused")
	public Token getLeftParen()
	{
		return leftParen;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitGroupingExpression(this);
	}

	@Override
	public String toString()
	{
		return "(" + expression.toString() + ")";
	}

	@Override
	public Token getFirstToken()
	{
		return leftParen; // The opening parenthesis is the first token of the grouping expression
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return null; // Grouping itself doesn't resolve to a symbol, its inner expression does.
	}

	@Override
	public void setResolvedType(Type resolvedType)
	{
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
		// The type of a grouping expression is the type of its inner expression.
		return expression.getResolvedType();
	}
}
