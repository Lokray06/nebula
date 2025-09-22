// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/DotExpression.java

package org.lokray.nebula.ast.expressions;

import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.semantics.Symbol;
import org.lokray.nebula.semantics.Type;

/**
 * AST node representing a member access expression (e.g., object.property, Namespace.Class.staticMethod).
 * It has a left-hand side expression (the object/namespace) and an identifier for the member.
 */
public class DotExpression implements Expression
{
	private final Expression left; // The left-hand side (e.g., IdentifierExpression for object, DotExpression for qualified name)
	private final Token dotToken; // The dot operator token
	private final Token memberName; // The identifier token for the member name (field, method, nested class)

	// Semantic information to be filled during semantic analysis
	private Symbol resolvedSymbol;
	private Type resolvedType;

	public DotExpression(Expression left, Token dotToken, Token memberName)
	{
		this.left = left;
		this.dotToken = dotToken;
		this.memberName = memberName;
	}

	public Expression getLeft()
	{
		return left;
	}

	@SuppressWarnings("unused")
	public Token getDotToken()
	{
		return dotToken;
	}

	public Token getMemberName()
	{
		return memberName;
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return resolvedSymbol;
	}

	@Override
	public void setResolvedSymbol(Symbol resolvedSymbol)
	{
		this.resolvedSymbol = resolvedSymbol;
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType()
	{
		return resolvedType;
	}

	@Override
	public void setResolvedType(Type resolvedType)
	{
		this.resolvedType = resolvedType;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitDotExpression(this);
	}

	@Override
	public String toString()
	{
		// Example: ((Com.MyCompany).App) or (obj.field)
		return "(" + left + "." + memberName.getLexeme() + ")";
	}

	@Override
	public Token getFirstToken()
	{
		return left.getFirstToken(); // The first token is the first token of the left expression
	}
}
