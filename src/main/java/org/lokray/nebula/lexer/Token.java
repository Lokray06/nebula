package org.lokray.nebula.lexer;

import java.util.Objects;

/**
 * Represents a single token produced by the Nebula Lexer.
 * Each token encapsulates its type, the actual text (lexeme),
 * and its position in the source file for error reporting.
 */
public class Token
{
	private final TokenType type;    // The classification of the token (e.g., IDENTIFIER, INT, PLUS)
	private final String lexeme;     // The actual text of the token (e.g., "myVariable", "123", "+")
	private final Object literal;    // The parsed value of the literal (e.g., Integer 123, String "hello")
	private final int line;          // The line number in the source file where the token starts
	private final int column;        // The column number in the source file where the token starts

	/**
	 * Constructs a new Token instance.
	 *
	 * @param type    The TokenType of this token.
	 * @param lexeme  The raw string value of the token from the source code.
	 * @param literal The parsed literal value for literal tokens (e.g., for "123" it's the Integer object 123,
	 *                for "true" it's the Boolean object true). Null for non-literal tokens.
	 * @param line    The line number where this token begins.
	 * @param column  The column number where this token begins.
	 */
	public Token(TokenType type, String lexeme, Object literal, int line, int column)
	{
		this.type = type;
		this.lexeme = lexeme;
		this.literal = literal;
		this.line = line;
		this.column = column;
	}

	// --- Getters for Token properties ---

	public TokenType getType()
	{
		return type;
	}

	public String getLexeme()
	{
		return lexeme;
	}

	public Object getLiteral()
	{
		return literal;
	}

	public int getLine()
	{
		return line;
	}

	public int getColumn()
	{
		return column;
	}

	/**
	 * Provides a string representation of the Token, useful for debugging.
	 * Format: "TokenType Lexeme [Literal] (Line:Column)"
	 */
	@Override
	public String toString()
	{
		String literalStr = (literal != null) ? " [" + literal + "]" : "";
		return type + " '" + lexeme + "'" + literalStr + " (Line:" + line + ", Col:" + column + ")";
	}

	/**
	 * Basic equality check for tokens, primarily for testing purposes.
	 * Compares type, lexeme, and literal. Line/column are not included as
	 * tokens from different positions could still be considered "the same"
	 * for semantic comparison.
	 */
	@Override
	public boolean equals(Object o)
	{
		if(this == o)
			return true;
		if(o == null || getClass() != o.getClass())
			return false;

		Token token = (Token) o;

		if(type != token.type)
			return false;
		if(!lexeme.equals(token.lexeme))
			return false;
		return Objects.equals(literal, token.literal);
	}

	/**
	 * Generates a hash code for the token.
	 */
	@Override
	public int hashCode()
	{
		int result = type.hashCode();
		result = 31 * result + lexeme.hashCode();
		result = 31 * result + (literal != null ? literal.hashCode() : 0);
		return result;
	}
}