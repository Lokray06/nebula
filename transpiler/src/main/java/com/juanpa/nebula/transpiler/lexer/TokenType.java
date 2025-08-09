// File: src/main/java/com/juanpa/nebula/transpiler/lexer/TokenType.java

package com.juanpa.nebula.transpiler.lexer;

/**
 * Defines the types of tokens recognized by the Nebula Lexer.
 * This enum covers keywords, operators, literals, punctuation, and special tokens.
 */
public enum TokenType
{
	// --- Keywords ---
	// Access Modifiers & Class-related
	CLASS, PUBLIC, PRIVATE, STATIC, EXTENDS, NAMESPACE, NATIVE, WRAPPER, SUPER, ALIAS, IMPORT,

	// Control Flow
	IF, ELSE, FOR, WHILE, DO, SWITCH, CASE, DEFAULT, RETURN, THROW, TRY, CATCH, FINALLY,

	// Primitives & Types
	VOID, BOOL, CHAR, CHAR16, CHAR32, BYTE, SHORT, INT, LONG, FLOAT, DOUBLE,
	INT8, INT16, INT32, INT64,
	UINT8, UINT16, UINT32, UINT64,
	UBYTE, USHORT, UINT, ULONG,
	STRING_KEYWORD, VAR, CONST, IS,

	// Other
	NEW, THIS, GLOBAL, OPERATOR, GET, SET, RESULT,

	// --- Identifiers ---
	IDENTIFIER,

	// --- Literals ---
	INTEGER_LITERAL,
	STRING_LITERAL,
	CHAR_LITERAL,
	BOOLEAN_LITERAL,
	FLOAT_LITERAL,
	DOUBLE_LITERAL,
	NULL,

	// --- Operators ---
	// Arithmetic
	PLUS, MINUS, STAR, SLASH, MODULO,
	PLUS_PLUS, MINUS_MINUS,

	// Assignment
	ASSIGN, PLUS_ASSIGN, MINUS_ASSIGN, STAR_ASSIGN, SLASH_ASSIGN, MODULO_ASSIGN,

	// Comparison & Logical
	EQUAL_EQUAL, BANG_EQUAL,
	LESS, GREATER, LESS_EQUAL, GREATER_EQUAL,
	AMPERSAND, AMPERSAND_AMPERSAND, PIPE, PIPE_PIPE, BANG,

	// Other Operators
	NULL_COALESCING, ARROW,

	// --- Punctuation & Delimiters ---
	// Grouping
	LEFT_PAREN, RIGHT_PAREN,
	LEFT_BRACE, RIGHT_BRACE,
	LEFT_BRACKET, RIGHT_BRACKET,

	// Separators
	DOT, COMMA, SEMICOLON, COLON, QUESTION,

	// --- Special Tokens ---
	EOF, // End Of File
	ERROR // For lexical errors
}