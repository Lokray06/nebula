// File: src/main/java/com/juanpa/nebula/transpiler/lexer/Lexer.java

package com.juanpa.nebula.transpiler.lexer;

import com.juanpa.nebula.transpiler.util.ErrorReporter;

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
		keywords.put("string", TokenType.STRING_KEYWORD); // 'string' is a keyword, not just an identifier
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
		keywords.put("default", TokenType.DEFAULT);
		keywords.put("is", TokenType.IS);
		keywords.put("alias", TokenType.ALIAS); // NEW: Added alias keyword
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
		keywords.put("ubyte", TokenType.UINT8); // Alias
		keywords.put("ushort", TokenType.UINT16); // Alias
		keywords.put("uint", TokenType.UINT32); // Alias
		keywords.put("ulong", TokenType.UINT64); // Alias
		keywords.put("short", TokenType.INT16); // Alias
		keywords.put("long", TokenType.INT64); // Alias
		keywords.put("null", TokenType.NULL); // Alias
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
			// --- Single-character tokens ---
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
			case '?':
				addToken(TokenType.QUESTION);
				break;
			case ':': // Added COLON
				addToken(TokenType.COLON);
				break;

			// --- Operators that can be single or double characters ---
			case '+':
				if (match('+'))
				{
					addToken(TokenType.PLUS_PLUS);
				}
				else if (match('='))
				{
					addToken(TokenType.PLUS_ASSIGN);
				}
				else
				{
					addToken(TokenType.PLUS);
				}
				break;
			case '-':
				if (match('>'))
				{
					addToken(TokenType.ARROW);
				}
				else if (match('-'))
				{
					addToken(TokenType.MINUS_MINUS);
				}
				else if (match('='))
				{
					addToken(TokenType.MINUS_ASSIGN);
				}
				else
				{
					addToken(TokenType.MINUS);
				}
				break;
			case '*':
				if (match('='))
				{
					addToken(TokenType.STAR_ASSIGN);
				}
				else
				{
					addToken(TokenType.STAR);
				}
				break;
			case '/':
				if (match('='))
				{
					addToken(TokenType.SLASH_ASSIGN);
				}
				else if (match('/'))
				{
					// It's a single-line comment, consume until newline
					while (peek() != '\n' && !isAtEnd())
					{
						advance();
					}
				}
				else if (match('*'))
				{
					// Multi-line comment, consume until '*/'
					while (!(peek() == '*' && peekNext() == '/') && !isAtEnd())
					{
						if (peek() == '\n')
						{
							line++;
							column = 0; // Reset column for new line
						}
						advance();
					}
					if (!isAtEnd())
					{ // Consume '*/'
						advance();
						advance();
					}
					else
					{
						error("Unterminated multi-line comment.");
					}
				}
				else
				{
					addToken(TokenType.SLASH);
				}
				break;
			case '%':
				if (match('='))
				{
					addToken(TokenType.MODULO_ASSIGN);
				}
				else
				{
					addToken(TokenType.MODULO);
				}
				break;
			case '=':
				addToken(match('=') ? TokenType.EQUAL_EQUAL : TokenType.ASSIGN);
				break;
			case '!':
				addToken(match('=') ? TokenType.BANG_EQUAL : TokenType.BANG);
				break;
			case '<':
				addToken(match('=') ? TokenType.LESS_EQUAL : TokenType.LESS);
				break;
			case '>':
				addToken(match('=') ? TokenType.GREATER_EQUAL : TokenType.GREATER);
				break;
			case '&':
				if (match('&'))
				{
					addToken(TokenType.AMPERSAND_AMPERSAND);
				}
				else
				{
					addToken(TokenType.AMPERSAND);
				}
				break;
			case '|':
				if (match('|'))
				{
					addToken(TokenType.PIPE_PIPE);
				}
				else
				{
					addToken(TokenType.PIPE);
				}
				break;
			case '.':
				if (Character.isDigit(peek()))
				{
					error("Floating point number cannot start with a decimal point. Expected a digit before '.'.");
					addToken(TokenType.DOT);
				}
				else
				{
					addToken(TokenType.DOT);
				}
				break;


			// --- Literals and Identifiers ---
			case '"':
				// Check if this is the start of a raw string (""")
				if (peek() == '"' && peek(1) == '"')
				{
					// Consume the next two quotes to move past the """ delimiter
					advance();
					advance();
					rawString(); // Call the existing rawString method
				}
				else
				{
					// Otherwise, it's a regular string literal
					scanStringLiteral();
				}
				break;
			case '\'':
				scanCharacterLiteral();
				break;

			// --- Whitespace ---
			case ' ':
			case '\r':
			case '\t':
				// Ignore whitespace.
				break;
			case '\n':
				line++;
				column = 0; // Reset column for new line
				break;

			default:
				if (Character.isDigit(c))
				{ // Corrected: Use Character.isDigit()
					scanNumber();
				}
				else if (Character.isLetter(c) || c == '_')
				{ // Corrected: Use Character.isLetter()
					scanIdentifier();
				}
				else
				{
					error("Unexpected character '" + c + "'.");
					addToken(TokenType.ERROR, null); // Add an error token to continue parsing
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

	/**
	 * Scans a number literal (integer, float, or double).
	 */
	private void scanNumber()
	{
		// Consume all digits before the decimal point
		while (Character.isDigit(peek()))
		{
			advance();
		}

		// Check for a decimal point and subsequent digits (for float/double)
		boolean isFloatingPoint = false;
		if (peek() == '.' && Character.isDigit(peekNext()))
		{
			isFloatingPoint = true;
			advance(); // Consume the '.'
			while (Character.isDigit(peek()))
			{
				advance();
			}
		}

		String numberStr = source.substring(start, current);

		// Check for float/double literal suffixes ('f', 'F', 'd', 'D')
		if (isFloatingPoint)
		{
			if (peek() == 'f' || peek() == 'F')
			{
				advance(); // Consume 'f'
				try
				{
					addToken(TokenType.FLOAT_LITERAL, Float.parseFloat(numberStr));
				}
				catch (NumberFormatException e)
				{
					error("Invalid float literal: " + numberStr);
					addToken(TokenType.ERROR, null);
				}
			}
			else if (peek() == 'd' || peek() == 'D')
			{
				advance(); // Consume 'd'
				try
				{
					addToken(TokenType.DOUBLE_LITERAL, Double.parseDouble(numberStr));
				}
				catch (NumberFormatException e)
				{
					error("Invalid double literal: " + numberStr);
					addToken(TokenType.ERROR, null);
				}
			}
			else
			{
				// Default to DOUBLE_LITERAL if no suffix or unknown suffix
				try
				{
					addToken(TokenType.DOUBLE_LITERAL, Double.parseDouble(numberStr));
				}
				catch (NumberFormatException e)
				{
					error("Invalid double literal: " + numberStr);
					addToken(TokenType.ERROR, null);
				}
			}
		}
		else
		{
			// It's an integer literal
			try
			{
				addToken(TokenType.INTEGER_LITERAL, Integer.parseInt(numberStr));
			}
			catch (NumberFormatException e)
			{
				error("Invalid integer literal: " + numberStr);
				addToken(TokenType.ERROR, null);
			}
		}
	}


	// In Lexer.java

	/**
	 * Scans a string literal enclosed in double quotes.
	 */
	private void scanStringLiteral()
	{
		StringBuilder value = new StringBuilder();
		while (peek() != '"' && !isAtEnd())
		{
			char c = advance(); // Consume character
			if (c == '\\')
			{ // Handle escape sequences inside strings
				if (isAtEnd())
				{
					error("Unterminated escape sequence in string literal.");
					break;
				}
				char escapeChar = advance(); // Consume the escaped character
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
					case '"': // This case correctly handles \"
						value.append('"');
						break;
					case '\\':
						value.append('\\');
						break;
					case '0':
						value.append('\0'); // Null character
						break;
					default:
						error("Invalid escape sequence '\\" + escapeChar + "' in string literal.");
						value.append('\\').append(escapeChar); // Append as-is if invalid escape
						break;
				}
			}
			else if (c == '\n')
			{ // This correctly handles newlines
				value.append('\n'); // Append the newline character to the string value
				line++;             // Increment line count
				column = 0;         // Reset column for the new line
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

		advance(); // Consume the closing '"'
		addToken(TokenType.STRING_LITERAL, value.toString());
	}

	/**
	 * Scans a character literal enclosed in single quotes.
	 */
	private void scanCharacterLiteral()
	{
		// Consume the opening single quote
		// `start` already points to the beginning of the literal (before the first '\'')
		// `current` is on the character after the first '\''
		char charValue;

		if (isAtEnd())
		{
			error("Unterminated character literal.");
			addToken(TokenType.ERROR, null);
			return;
		}

		char c = peek();
		if (c == '\\')
		{ // Handle escape sequences
			advance(); // Consume '\'
			if (isAtEnd())
			{
				error("Unterminated escape sequence in character literal.");
				addToken(TokenType.ERROR, null);
				return;
			}
			char escapeChar = advance(); // Consume the escaped character
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
					charValue = '\0'; // Specifically handle null character
					break;
				default:
					error("Invalid escape sequence '\\" + escapeChar + "' in character literal.");
					charValue = '\0'; // Assign dummy value for error recovery
			}
		}
		else if (c == '\n' || c == '\r')
		{
			error("Newline in character literal.");
			charValue = '\0'; // Assign dummy value for error recovery
		}
		else
		{
			charValue = advance(); // Consume the character itself
		}

		// Check for closing single quote
		if (isAtEnd() || peek() != '\'')
		{
			error("Unterminated character literal. Expected '.");
			addToken(TokenType.ERROR, null);
			// Try to recover by advancing until end of line or next quote
			while (!isAtEnd() && peek() != '\n' && peek() != '\'')
			{
				advance();
			}
			if (!isAtEnd() && peek() == '\'')
			{ // If we found the closing quote
				advance(); // Consume it
			}
			return;
		}

		advance(); // Consume the closing single quote
		addToken(TokenType.CHAR_LITERAL, charValue);
	}


	/**
	 * Scans an identifier (variable name, class name, method name, keyword).
	 */
	private void scanIdentifier()
	{
		while (Character.isLetterOrDigit(peek()) || peek() == '_')
		{ // Corrected: Use Character.isLetterOrDigit()
			advance();
		}

		String text = source.substring(start, current);
		TokenType type = keywords.get(text); // Check if it's a reserved keyword

		if (type == null)
		{
			type = TokenType.IDENTIFIER; // If not a keyword, it's an identifier
			addToken(type); // Identifiers don't have literal values
		}
		else
		{
			// Check if it's a boolean literal keyword ('true' or 'false')
			if (type == TokenType.BOOLEAN_LITERAL)
			{
				// Parse the boolean string and pass the Boolean object as the literal value
				addToken(type, Boolean.parseBoolean(text));
			}
			else
			{
				// For other keywords (like 'class', 'void', 'if', etc.), they don't have a literal value.
				addToken(type); // This calls addToken(type, null) implicitly.
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

		// 1. Find the closing delimiter """
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
				column = 0; // Reset column for the new line
			}
			advance();
		}

		// 2. We found the end. Extract the raw content.
		// The content is from the character after the opening """ to the one before the closing """.
		String rawContent = source.substring(start + 3, current);

		// Consume the closing delimiter
		advance();
		advance();
		advance();

		// 3. Calculate the indentation of the closing line.
		int lastNewline = rawContent.lastIndexOf('\n');
		if (lastNewline == -1)
		{
			// Single-line raw string: """some content"""
			addToken(TokenType.STRING_LITERAL, rawContent);
			return;
		}

		// The indentation is the whitespace after the last newline.
		String closingIndent = rawContent.substring(lastNewline + 1);

		// The C# spec requires the closing line to contain only whitespace.
		if (!closingIndent.trim().isEmpty())
		{
			// We can be lenient and just assume no indentation in this malformed case.
			closingIndent = "";
		}

		// The actual content to process is everything before this last newline.
		String contentToProcess = rawContent.substring(0, lastNewline);

		// 4. Split into lines and strip the common indentation.
		String[] lines = contentToProcess.split("\n");
		StringBuilder finalValue = new StringBuilder();

		// Handle the first line separately to ignore whitespace after opening quotes.
		if (lines.length > 0)
		{
			finalValue.append(lines[0].stripLeading());
		}

		// Process the rest of the lines
		for (int i = 1; i < lines.length; i++)
		{
			finalValue.append('\n');
			String currentLine = lines[i];

			// Only strip indentation if the line isn't blank and has the prefix
			if (!currentLine.isBlank() && currentLine.startsWith(closingIndent))
			{
				finalValue.append(currentLine.substring(closingIndent.length()));
			}
			else
			{
				finalValue.append(currentLine);
			}
		}

		// 5. Add the final processed string token.
		addToken(TokenType.STRING_LITERAL, finalValue.toString());
	}
}