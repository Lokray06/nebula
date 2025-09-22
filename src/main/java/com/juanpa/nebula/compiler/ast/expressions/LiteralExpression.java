// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/LiteralExpression.java

package com.juanpa.nebula.compiler.ast.expressions;

import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.lexer.Token;
import com.juanpa.nebula.compiler.lexer.TokenType;
import com.juanpa.nebula.compiler.semantics.NullType;
import com.juanpa.nebula.compiler.semantics.PrimitiveType;
import com.juanpa.nebula.compiler.semantics.Symbol;
import com.juanpa.nebula.compiler.semantics.Type;

public class LiteralExpression implements Expression
{
	private final Object value;
	private final Token literalToken;
	private Type resolvedType;

	// Existing constructor for literals with a value (numbers, strings)
	public LiteralExpression(Object value, Token literalToken)
	{
		this.value = value;
		this.literalToken = literalToken;
		this.resolvedType = determineLiteralType(literalToken);
	}

	// NEW overloaded constructor for literals without a value (null, true, false)
	public LiteralExpression(Token literalToken)
	{
		this.value = literalToken.getLiteral(); // This will be null for 'null', 'true', 'false'
		this.literalToken = literalToken;
		this.resolvedType = determineLiteralType(literalToken);
	}

	private Type determineLiteralType(Token literalToken)
	{
		return switch (literalToken.getType())
		{
			case INTEGER_LITERAL -> PrimitiveType.INT;
			case STRING_LITERAL ->  null; // Correctly resolve String type later in semantic analysis
			case CHAR_LITERAL -> PrimitiveType.CHAR;
			case BOOLEAN_LITERAL -> PrimitiveType.BOOL;
			case FLOAT_LITERAL -> PrimitiveType.FLOAT;
			case DOUBLE_LITERAL -> PrimitiveType.DOUBLE;
			case NULL -> NullType.INSTANCE;
			default -> null;
		};
	}

	public Object getValue()
	{
		return value;
	}

	public Token getLiteralToken()
	{
		return literalToken;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitLiteralExpression(this);
	}

	@Override
	public String toString()
	{
		if (literalToken.getType() == TokenType.STRING_LITERAL)
		{
			return "\"" + value + "\"";
		}
		return literalToken.getLexeme();
	}

	@Override
	public Token getFirstToken()
	{
		return literalToken;
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		return null;
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not applicable for literals
	}

	@Override
	public void setResolvedType(Type resolvedType)
	{
		this.resolvedType = resolvedType;
	}

	@Override
	public Type getResolvedType()
	{
		return resolvedType;
	}
}