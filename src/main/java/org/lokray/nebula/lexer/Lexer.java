// File: src/main/java/com/juanpa/nebula/transpiler/lexer/Lexer.java

package org.lokray.nebula.lexer;

import org.lokray.nebula.util.ErrorReporter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * The Lexer is responsible for performing lexical analysis (scanning).
 * It reads the raw Nebula source code and converts it into a stream of meaningful Tokens.
 * This class identifies keywords, identifiers, literals, operators, and punctuation.
 */
public class Lexer
{
	private final String source; // The raw source code string
	private final List<Token> tokens = new ArrayList<>(); // List to store generated tokens
	private final ErrorReporter errorReporter; // For reporting lexical errors

	private int start = 0; // Current token's starting position in the source
	private int current = 0; // Current position in the source
	private int line = 1; // Current line number
	private int column = 1; // Current column number

	private int startLine = 1;
	private int startColumn = 1;

	// Static map to store reserved keywords for quick lookup
	private static final Map<String, TokenType> keywords;

	static
	{
		keywords = new HashMap<>();
		keywords.put("class", TokenType.CLASS);
		keywords.put("public", TokenType.PUBLIC);
		keywords.put("private", TokenType.PRIVATE);
		keywords.put("static", TokenType.STATIC);
		keywords.put("void", TokenType.VOID);
		keywords.put("string", TokenType.STRING_KEYWORD);
		keywords.put("int", TokenType.INT);
		keywords.put("bool", TokenType.BOOL);
		keywords.put("float", TokenType.FLOAT);
		keywords.put("double", TokenType.DOUBLE);
		keywords.put("byte", TokenType.BYTE);
		keywords.put("var", TokenType.VAR);
		keywords.put("new", TokenType.NEW);
		keywords.put("this", TokenType.THIS);
		keywords.put("if", TokenType.IF);
		keywords.put("else", TokenType.ELSE);
		keywords.put("for", TokenType.FOR);
		keywords.put("foreach", TokenType.FOREACH);
		keywords.put("in", TokenType.IN);
		keywords.put("while", TokenType.WHILE);
		keywords.put("do", TokenType.DO);
		keywords.put("extends", TokenType.EXTENDS);
		keywords.put("return", TokenType.RETURN);
		keywords.put("namespace", TokenType.NAMESPACE);
		keywords.put("get", TokenType.GET);
		keywords.put("set", TokenType.SET);
		keywords.put("throw", TokenType.THROW);
		keywords.put("try", TokenType.TRY);
		keywords.put("catch", TokenType.CATCH);
		keywords.put("finally", TokenType.FINALLY);
		keywords.put("result", TokenType.RESULT);
		keywords.put("operator", TokenType.OPERATOR);
		keywords.put("char", TokenType.CHAR);
		keywords.put("true", TokenType.BOOLEAN_LITERAL);
		keywords.put("false", TokenType.BOOLEAN_LITERAL);
		keywords.put("const", TokenType.CONST);
		keywords.put("import", TokenType.IMPORT);
		keywords.put("global", TokenType.GLOBAL);
		keywords.put("switch", TokenType.SWITCH);
		keywords.put("case", TokenType.CASE);
		keywords.put("break", TokenType.BREAK);
		keywords.put("default", TokenType.DEFAULT);
		keywords.put("is", TokenType.IS);
		keywords.put("alias", TokenType.ALIAS);
		keywords.put("native", TokenType.NATIVE);
		keywords.put("wrapper", TokenType.WRAPPER);
		keywords.put("char16", TokenType.CHAR16);
		keywords.put("char32", TokenType.CHAR32);
		keywords.put("int8", TokenType.INT8);
		keywords.put("int16", TokenType.INT16);
		keywords.put("int32", TokenType.INT32);
		keywords.put("int64", TokenType.INT64);
		keywords.put("uint8", TokenType.UINT8);
		keywords.put("uint16", TokenType.UINT16);
		keywords.put("uint32", TokenType.UINT32);
		keywords.put("uint64", TokenType.UINT64);
		keywords.put("ubyte", TokenType.UINT8);
		keywords.put("ushort", TokenType.UINT16);
		keywords.put("uint", TokenType.UINT32);
		keywords.put("ulong", TokenType.UINT64);
		keywords.put("short", TokenType.INT16);
		keywords.put("long", TokenType.INT64);
		keywords.put("null", TokenType.NULL);
	}

	/**
	 * Constructs a Lexer.
	 *
	 * @param source        The source code string to tokenize.
	 * @param errorReporter An instance of ErrorReporter for logging errors.
	 */
	public Lexer(String source, ErrorReporter errorReporter)
	{
		this.source = source;
		this.errorReporter = errorReporter;
	}

	/**
	 * Scans the entire source code and returns a list of tokens.
	 */
	public List<Token> scanTokens()
	{
		while (!isAtEnd())
		{
			start = current; // Mark the beginning of the current token

			// --- THIS IS A NEW AND IMPORTANT STEP ---
			// Save the starting position of the token before scanning it
			startLine = line;
			startColumn = column;

			scanToken(); // Scan and add the next token
		}

		// Add the End Of File token
		// The position for EOF can be the end of the last token
		tokens.add(new Token(TokenType.EOF, "", null, line, column));
		return tokens;
	}

	/**
	 * Scans a single token from the source code.
	 */
	private void scanToken()
	{
		char c = advance(); // Get and consume the current character

		switch (c)
		{
			// Punctuation
			case '(':
				addToken(TokenType.LEFT_PAREN);
				break;
			case ')':
				addToken(TokenType.RIGHT_PAREN);
				break;
			case '{':
				addToken(TokenType.LEFT_BRACE);
				break;
			case '}':
				addToken(TokenType.RIGHT_BRACE);
				break;
			case '[':
				addToken(TokenType.LEFT_BRACKET);
				break;
			case ']':
				addToken(TokenType.RIGHT_BRACKET);
				break;
			case ',':
				addToken(TokenType.COMMA);
				break;
			case ';':
				addToken(TokenType.SEMICOLON);
				break;
			case ':':
				addToken(TokenType.COLON);
				break;
			case '?':
				addToken(match('?') ? TokenType.NULL_COALESCING : TokenType.QUESTION);
				break;

			// Operators
			case '+':
				addToken(match('=') ? TokenType.PLUS_ASSIGN : (match('+') ? TokenType.PLUS_PLUS : TokenType.PLUS));
				break;
			case '*':
				if (match('*'))
				{
					addToken(match('=') ? TokenType.POWER_ASSIGN : TokenType.POWER);
				}
				else
				{
					addToken(match('=') ? TokenType.STAR_ASSIGN : TokenType.STAR);
				}
				break;
			case '-':
				addToken(match('=') ? TokenType.MINUS_ASSIGN : (match('-') ? TokenType.MINUS_MINUS : (match('>') ? TokenType.ARROW : TokenType.MINUS)));
				break;
			case '%':
				addToken(match('=') ? TokenType.MODULO_ASSIGN : TokenType.MODULO);
				break;
			case '=':
				addToken(match('=') ? TokenType.EQUAL_EQUAL : TokenType.ASSIGN);
				break;
			case '!':
				addToken(match('=') ? TokenType.BANG_EQUAL : TokenType.BANG);
				break;
			case '^':
				addToken(match('=') ? TokenType.XOR_ASSIGN : TokenType.XOR);
				break;
			case '~':
				addToken(TokenType.BITWISE_NOT);
				break; // Renamed NOT to TILDE for clarity

			case '&':
				addToken(match('=') ? TokenType.AMPERSAND_ASSIGN : (match('&') ? TokenType.AMPERSAND_AMPERSAND : TokenType.AMPERSAND));
				break;
			case '|':
				addToken(match('=') ? TokenType.PIPE_ASSIGN : (match('|') ? TokenType.PIPE_PIPE : TokenType.PIPE));
				break;

			case '<':
				addToken(match('=') ? TokenType.LESS_EQUAL : (match('<') ? (match('=') ? TokenType.LEFT_SHIFT_ASSIGN : TokenType.LEFT_SHIFT) : TokenType.LESS));
				break;
			case '>':
				addToken(match('=') ? TokenType.GREATER_EQUAL : (match('>') ? (match('=') ? TokenType.RIGHT_SHIFT_ASSIGN : TokenType.RIGHT_SHIFT) : TokenType.GREATER));
				break;

			case '/':
				if (match('/'))
				{ // Single-line comment
					while (peek() != '\n' && !isAtEnd())
					{
						advance();
					}
				}
				else if (match('*'))
				{ // Multi-line comment
					multilineComment();
				}
				else
				{
					addToken(match('=') ? TokenType.SLASH_ASSIGN : TokenType.SLASH);
				}
				break;

			// Whitespace
			case ' ':
			case '\r':
			case '\t':
				// Ignore.
				break;
			case '\n':
				line++;
				column = 0;
				break;
			// Literals
			case '$':
				if (peek() == '"')
				{
					// It's the start of an interpolated string.
					// Consume the opening quote and scan the content.
					advance();
					scanStringLiteral(true); // Pass true to indicate interpolation
				}
				else
				{
					// Handle other uses of '$' or report an error
					error("'$' is only valid for interpolated strings and must be followed by '\"'.");
				}
				break;
			case '"':
				scanStringLiteral(false);
				break;
			case '\'':
				scanCharacterLiteral();
				break;
			case '.':
				if (Character.isDigit(peek()))
				{
					error("Floating point numbers cannot start with a decimal point. Expected a digit before '.'.");
				}
				addToken(TokenType.DOT);
				break;

			default:
				if (Character.isDigit(c))
				{
					// Check for '0x' prefix to handle hexadecimal literals
					if (c == '0' && (peek() == 'x' || peek() == 'X'))
					{
						advance(); // consume the 'x'
						scanHexNumber();
					}
					else
					{
						scanNumber();
					}
				}
				else if (Character.isLetter(c) || c == '_')
				{
					scanIdentifier();
				}
				else
				{
					error("Unexpected character '" + c + "'.");
				}
				break;
		}
	}

	/**
	 * Consumes the current character and returns it, also updates line/column.
	 *
	 * @return The consumed character.
	 */
	private char advance()
	{
		char c = source.charAt(current++);
		column++; // Increment column for the consumed character
		return c;
	}

	/**
	 * Adds a token to the list of tokens.
	 *
	 * @param type    The TokenType of the token.
	 * @param literal The literal value of the token (for numbers, strings, booleans).
	 */
	private void addToken(TokenType type, Object literal)
	{
		String text = source.substring(start, current);

		// --- THIS IS THE MODIFIED LOGIC ---
		// Use the saved startLine and startColumn instead of the current lexer position.
		// This ensures multi-line tokens get the correct starting position.
		tokens.add(new Token(type, text, literal, startLine, startColumn));
	}


	/**
	 * Overloaded method to add a token without a literal value.
	 *
	 * @param type The TokenType of the token.
	 */
	private void addToken(TokenType type)
	{
		addToken(type, null);
	}

	/**
	 * Checks if the current character matches the expected character.
	 * If it matches, consumes it (advances `current`).
	 *
	 * @param expected The expected character.
	 * @return True if the character matched and was consumed, false otherwise.
	 */
	private boolean match(char expected)
	{
		if (isAtEnd())
		{
			return false;
		}
		if (source.charAt(current) != expected)
		{
			return false;
		}

		current++;
		column++; // Increment column for the matched character
		return true;
	}

	/**
	 * Looks at the current character without consuming it.
	 *
	 * @return The current character, or '\0' if at the end of the source.
	 */
	private char peek()
	{
		if (isAtEnd())
		{
			return '\0';
		}
		return source.charAt(current);
	}

	/**
	 * Looks at the next character (one position ahead) without consuming it.
	 *
	 * @return The next character, or '\0' if at or beyond the end of the source.
	 */
	private char peekNext()
	{
		if (current + 1 >= source.length())
		{
			return '\0';
		}
		return source.charAt(current + 1);
	}

	/**
	 * Looks at the character at a given offset from the current position without consuming it.
	 *
	 * @param offset The number of characters to look ahead.
	 * @return The character at the specified offset, or '\0' if at the end of the source.
	 */
	private char peek(int offset)
	{
		if (current + offset >= source.length())
		{
			return '\0';
		}
		return source.charAt(current + offset);
	}

	/**
	 * Checks if the lexer has reached the end of the code.
	 *
	 * @return True if at the end, false otherwise.
	 */
	private boolean isAtEnd()
	{
		return current >= source.length();
	}

	/**
	 * Reports a lexical error.
	 *
	 * @param message The error message.
	 */
	private void error(String message)
	{
		errorReporter.report(line, column - (current - start), "[Lexical Error] " + message);
	}

	private void multilineComment()
	{
		while (!(peek() == '*' && peekNext() == '/') && !isAtEnd())
		{
			if (peek() == '\n')
			{
				line++;
				column = 0;
			}
			advance();
		}
		if (isAtEnd())
		{
			error("Unterminated multi-line comment.");
			return;
		}
		// Consume the closing */
		advance();
		advance();
	}

	/**
	 * Scans a number literal (integer, float, or double).
	 */
	// In Lexer.java, replace the old scanNumber() method with this new one
	private void scanNumber()
	{
		// Consume the integer part, allowing for separators
		while (Character.isDigit(peek()) || peek() == '_')
		{
			advance();
		}

		boolean isFloatingPoint = false;
		// Check for a fractional part
		if (peek() == '.' && Character.isDigit(peekNext()))
		{
			isFloatingPoint = true;
			advance(); // consume the '.'
			// Consume the fractional part
			while (Character.isDigit(peek()) || peek() == '_')
			{
				advance();
			}
		}

		String numberText = source.substring(start, current);
		String cleanedText = numberText.replace("_", "");

		if (isFloatingPoint)
		{
			if (peek() == 'f' || peek() == 'F')
			{
				advance(); // Consume 'f'
				try
				{
					addToken(TokenType.FLOAT_LITERAL, Float.parseFloat(cleanedText));
				}
				catch (NumberFormatException e)
				{
					error("Invalid float literal: " + numberText);
				}
			}
			else
			{
				if (peek() == 'd' || peek() == 'D')
				{
					advance(); // Consume 'd'
				}
				try
				{
					addToken(TokenType.DOUBLE_LITERAL, Double.parseDouble(cleanedText));
				}
				catch (NumberFormatException e)
				{
					error("Invalid double literal: " + numberText);
				}
			}
		}
		else
		{ // It's an integer type
			boolean isLong = false;
			if (peek() == 'L' || peek() == 'l')
			{
				isLong = true;
				advance();
			}

			try
			{
				if (isLong)
				{
					addToken(TokenType.INTEGER_LITERAL, Long.parseLong(cleanedText));
				}
				else
				{
					// Try to parse as long first to check the range.
					long value = Long.parseLong(cleanedText);
					if (value >= Integer.MIN_VALUE && value <= Integer.MAX_VALUE)
					{
						addToken(TokenType.INTEGER_LITERAL, (int) value);
					}
					else
					{
						// Value is too big for int, so it's a long.
						addToken(TokenType.INTEGER_LITERAL, value);
					}
				}
			}
			catch (NumberFormatException e)
			{
				error("Invalid integer literal: " + numberText + ". Value is out of range for a 64-bit integer.");
			}
		}
	}

	/**
	 * Scans a string literal enclosed in double quotes.
	 */
	private void scanStringLiteral(boolean isInterpolated)
	{
		StringBuilder value = new StringBuilder();
		int braceDepth = 0;

		while (!isAtEnd())
		{
			char c = advance();

			if (c == '\\')
			{
				// handle escapes as you already do...
				if (isAtEnd())
				{
					break;
				}
				char escapeChar = advance();
				switch (escapeChar)
				{
					case 'n':
						value.append('\n');
						break;
					case 't':
						value.append('\t');
						break;
					case 'r':
						value.append('\r');
						break;
					case 'b':
						value.append('\b');
						break;
					case 'f':
						value.append('\f');
						break;
					case '"':
						value.append('"');
						break;
					case '\\':
						value.append('\\');
						break;
					case '0':
						value.append('\0');
						break;
					default:
						error("Invalid escape sequence '\\" + escapeChar + "' in string literal.");
						value.append('\\').append(escapeChar);
				}
				continue;
			}

			if (c == '{' && isInterpolated)
			{
				braceDepth++;
				value.append(c);
				continue;
			}

			if (c == '}' && isInterpolated)
			{
				if (braceDepth > 0)
				{
					braceDepth--;
				}
				value.append(c);
				continue;
			}

			if (c == '"' && (!isInterpolated || braceDepth == 0))
			{
				// End of string
				break;
			}

			if (c == '\n')
			{
				line++;
				column = 0;
				value.append(c);
			}
			else
			{
				value.append(c);
			}
		}

		if (isAtEnd())
		{
			error("Unterminated string literal.");
			addToken(TokenType.ERROR, null);
			return;
		}

		String strVal = value.toString();
		if (isInterpolated)
		{
			addToken(TokenType.INTERPOLATED_STRING_LITERAL, strVal);
		}
		else
		{
			addToken(TokenType.STRING_LITERAL, strVal);
		}
	}

	/**
	 * Scans a character literal enclosed in single quotes.
	 */
	private void scanCharacterLiteral()
	{
		char charValue;
		if (isAtEnd())
		{
			error("Unterminated character literal.");
			addToken(TokenType.ERROR, null);
			return;
		}
		char c = peek();
		if (c == '\\')
		{
			advance();
			if (isAtEnd())
			{
				error("Unterminated escape sequence in character literal.");
				addToken(TokenType.ERROR, null);
				return;
			}
			char escapeChar = advance();
			switch (escapeChar)
			{
				case 'n':
					charValue = '\n';
					break;
				case 't':
					charValue = '\t';
					break;
				case 'r':
					charValue = '\r';
					break;
				case 'b':
					charValue = '\b';
					break;
				case 'f':
					charValue = '\f';
					break;
				case '\'':
					charValue = '\'';
					break;
				case '\\':
					charValue = '\\';
					break;
				case '0':
					charValue = '\0';
					break;
				default:
					error("Invalid escape sequence '\\" + escapeChar + "' in character literal.");
					charValue = '\0';
			}
		}
		else if (c == '\n' || c == '\r')
		{
			error("Newline in character literal.");
			charValue = '\0';
		}
		else
		{
			charValue = advance();
		}
		if (isAtEnd() || peek() != '\'')
		{
			error("Unterminated character literal. Expected '.");
			addToken(TokenType.ERROR, null);
			while (!isAtEnd() && peek() != '\n' && peek() != '\'')
			{
				advance();
			}
			if (!isAtEnd() && peek() == '\'')
			{
				advance();
			}
			return;
		}
		advance();
		addToken(TokenType.CHAR_LITERAL, charValue);
	}


	/**
	 * Scans an identifier (variable name, class name, method name, keyword).
	 */
	private void scanIdentifier()
	{
		while (Character.isLetterOrDigit(peek()) || peek() == '_')
		{
			advance();
		}
		String text = source.substring(start, current);
		TokenType type = keywords.get(text);
		if (type == null)
		{
			type = TokenType.IDENTIFIER;
			addToken(type);
		}
		else
		{
			if (type == TokenType.BOOLEAN_LITERAL)
			{
				addToken(type, Boolean.parseBoolean(text));
			}
			else
			{
				addToken(type);
			}
		}
	}

	/**
	 * Scans a raw string literal, which starts and ends with three double quotes (""").
	 * It handles multi-line content and removes common leading indentation based on the
	 * indentation of the closing delimiter.
	 */
	private void rawString()
	{
		int contentStartLine = line;
		int contentStartCol = column;
		while (!(peek() == '"' && peek(1) == '"' && peek(2) == '"'))
		{
			if (isAtEnd())
			{
				errorReporter.report(contentStartLine, contentStartCol, "[Lexical Error] Unterminated raw string literal.");
				return;
			}
			if (peek() == '\n')
			{
				line++;
				column = 0;
			}
			advance();
		}
		String rawContent = source.substring(start + 3, current);
		advance();
		advance();
		advance();
		int lastNewline = rawContent.lastIndexOf('\n');
		if (lastNewline == -1)
		{
			addToken(TokenType.STRING_LITERAL, rawContent);
			return;
		}
		StringBuilder finalValue = getStringBuilder(rawContent, lastNewline);
		addToken(TokenType.STRING_LITERAL, finalValue.toString());
	}

	private static StringBuilder getStringBuilder(String rawContent, int lastNewline)
	{
		String closingIndent = rawContent.substring(lastNewline + 1);
		if (!closingIndent.trim().isEmpty())
		{
			closingIndent = "";
		}
		String contentToProcess = rawContent.substring(0, lastNewline);
		String[] lines = contentToProcess.split("\n");
		StringBuilder finalValue = new StringBuilder();
		if (lines.length > 0)
		{
			finalValue.append(lines[0].stripLeading());
		}
		for (int i = 1; i < lines.length; i++)
		{
			finalValue.append('\n');
			String currentLine = lines[i];
			if (!currentLine.isBlank() && currentLine.startsWith(closingIndent))
			{
				finalValue.append(currentLine.substring(closingIndent.length()));
			}
			else
			{
				finalValue.append(currentLine);
			}
		}
		return finalValue;
	}

	// Add this new helper method to Lexer.java
	private boolean isHexDigit(char c)
	{
		return (c >= '0' && c <= '9') ||
				(c >= 'a' && c <= 'f') ||
				(c >= 'A' && c <= 'F');
	}

	// Add this new method to Lexer.java to parse hex literals
	private void scanHexNumber()
	{
		int hexStart = current; // Position after '0x'

		while (isHexDigit(peek()) || peek() == '_')
		{
			advance();
		}

		String cleanedHexStr = source.substring(hexStart, current).replace("_", "");

		if (cleanedHexStr.isEmpty())
		{
			error("Invalid hexadecimal literal: no digits found after '0x'.");
			addToken(TokenType.ERROR, null);
			return;
		}

		boolean isLong = false;
		if (peek() == 'L' || peek() == 'l')
		{
			isLong = true;
			advance();
		}

		try
		{
			long value = Long.parseLong(cleanedHexStr, 16);
			if (isLong || value > Integer.MAX_VALUE || value < Integer.MIN_VALUE)
			{
				addToken(TokenType.INTEGER_LITERAL, value);
			}
			else
			{
				addToken(TokenType.INTEGER_LITERAL, (int) value);
			}
		}
		catch (NumberFormatException e)
		{
			error("Invalid hexadecimal literal: '" + cleanedHexStr + "' is out of range.");
			addToken(TokenType.ERROR, null);
		}
	}
}