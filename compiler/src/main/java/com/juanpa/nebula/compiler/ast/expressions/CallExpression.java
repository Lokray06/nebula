// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/CallExpression.java

package com.juanpa.nebula.compiler.ast.expressions;

import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.lexer.Token;
import com.juanpa.nebula.compiler.semantics.Symbol;
import com.juanpa.nebula.compiler.semantics.Type; // Import Type

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * AST node representing a function or method call.
 * Includes the expression representing the callable entity (e.g., identifier, dot expression)
 * and a list of argument expressions.
 */
public class CallExpression implements Expression
{
	private final Expression callee; // The expression being called (e.g., IdentifierExpression, DotExpression)
	private final Token paren;       // The opening parenthesis token (for error reporting)
	private final List<Expression> arguments;
	private Type resolvedType; // NEW: Field for resolved type


	public CallExpression(Expression callee, Token paren, List<Expression> arguments)
	{
		this.callee = callee;
		this.paren = paren;
		this.arguments = new ArrayList<>(arguments); // Defensive copy
	}

	public Expression getCallee()
	{
		return callee;
	}

	public Token getParen()
	{
		return paren;
	}

	public List<Expression> getArguments()
	{
		return Collections.unmodifiableList(arguments);
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitCallExpression(this);
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("(").append(callee);
		sb.append(")"); // Closing parenthesis from parser
		sb.append("("); // Opening parenthesis for arguments
		for(int i = 0; i < arguments.size(); i++)
		{
			sb.append(arguments.get(i));
			if(i < arguments.size() - 1)
			{
				sb.append(", ");
			}
		}
		sb.append(")"); // Closing parenthesis for arguments
		return sb.toString();
	}

	@Override
	public Token getFirstToken()
	{
		return callee.getFirstToken(); // The first token is the first token of the callee expression
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		// The resolved symbol of a CallExpression is the symbol of its callee (e.g., a MethodSymbol)
		return callee.getResolvedSymbol();
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not directly setting symbol on CallExpression, it's on the callee.
		// However, for consistency, can set it here if needed.
		// Typically, the SemanticAnalyzer would set the resolved symbol on the `callee` Expression.
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
