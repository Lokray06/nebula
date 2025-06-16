// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/LiteralExpression.java

package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type; // Import Type
import com.juanpa.nebula.transpiler.semantics.PrimitiveType; // Import PrimitiveType
import com.juanpa.nebula.transpiler.semantics.ClassType; // Import ClassType

/**
 * AST node representing a literal value (e.g., 123, "hello", true, 3.14).
 * Holds the literal value and its corresponding token.
 */
public class LiteralExpression implements Expression
{
	private final Object value; // The actual literal value (e.g., Integer, String, Boolean, Double)
	private final Token literalToken; // The token representing the literal
	private Type resolvedType; // NEW: Field for resolved type (for literals)

	public LiteralExpression(Object value, Token literalToken)
	{
		this.value = value;
		this.literalToken = literalToken;
		// Automatically determine the resolved type for literals
		this.resolvedType = determineLiteralType(literalToken);
	}

	private Type determineLiteralType(Token literalToken) {
		switch (literalToken.getType()) {
			case INTEGER_LITERAL: return PrimitiveType.INT;
			case STRING_LITERAL:
				// Assuming 'string' keyword implies 'nebula.core.String'
				// This needs access to declaredClasses, which this AST node doesn't have.
				// For now, it will be null, and SemanticAnalyzer will properly set it.
				// A temporary ClassType can be created here if needed for initial type,
				// but it must be resolved against actual ClassSymbols later.
				return null; // Will be set by SemanticAnalyzer
			case CHAR_LITERAL: return PrimitiveType.CHAR;
			case BOOLEAN_LITERAL: return PrimitiveType.BOOL;
			case FLOAT_LITERAL: return PrimitiveType.FLOAT;
			case DOUBLE_LITERAL: return PrimitiveType.DOUBLE;
			// For 'null' literal, assign NullType.INSTANCE
			case IDENTIFIER: // Could be 'null' identifier token
				if ("null".equals(literalToken.getLexeme())) {
					return com.juanpa.nebula.transpiler.semantics.NullType.INSTANCE;
				}
				// fall through for other identifiers if this is a generic literal expression
			default: return null; // Resolved by SemanticAnalyzer
		}
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
		// For string literals, include quotes in the output for clarity
		if(literalToken.getType() == TokenType.STRING_LITERAL)
		{
			return "\"" + value + "\"";
		}
		return value.toString();
	}

	@Override
	public Token getFirstToken()
	{
		return literalToken;
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		// Literals typically don't resolve to a Symbol in the symbol table directly,
		// but rather have an inherent Type.
		return null;
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// Not applicable for literals
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType() {
		// For literals, the type is inherent. SemanticAnalyzer might refine for String.
		return resolvedType;
	}
}
