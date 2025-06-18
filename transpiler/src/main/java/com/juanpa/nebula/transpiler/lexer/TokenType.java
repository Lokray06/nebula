// File: src/main/java/com/juanpa.nebula.transpiler/lexer/TokenType.java

package com.juanpa.nebula.transpiler.lexer;

/**
 * Defines the types of tokens recognized by the Nebula Lexer.
 * This enum covers keywords, operators, literals, punctuation, and special tokens.
 */
public enum TokenType
{
	// --- Keywords ---
	CLASS, PUBLIC, PRIVATE, STATIC, VOID, STRING_KEYWORD, INT, BOOL, FLOAT, DOUBLE, BYTE, VAR,
	NEW, THIS, IF, ELSE, FOR, WHILE, DO, EXTENDS, RETURN,
	NAMESPACE, GET, SET, THROW, TRY, CATCH, FINALLY,
	RESULT, OPERATOR, CHAR,
	CONST,
	IMPORT,
	GLOBAL,
	SWITCH, // Added for switch statement
	CASE,   // Added for switch statement
	DEFAULT, // Added for switch statement
	IS, //For type checking
	SUPER, // NEW: Added for super constructor calls
	ALIAS, // NEW: Added for alias declarations

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
	PLUS, MINUS, STAR, SLASH, MODULO,
	EQUAL_EQUAL, BANG_EQUAL,
	LESS, GREATER, LESS_EQUAL, GREATER_EQUAL,
	NULL_COALESCING,

	ASSIGN,
	PLUS_ASSIGN,
	MINUS_ASSIGN,
	STAR_ASSIGN,
	SLASH_ASSIGN,
	MODULO_ASSIGN,

	PLUS_PLUS,
	MINUS_MINUS,

	DOT,
	COMMA,
	SEMICOLON,
	QUESTION,
	AMPERSAND,
	AMPERSAND_AMPERSAND,
	PIPE,
	PIPE_PIPE,
	BANG,

	COLON, // Added for switch statement case labels


	// --- Punctuation & Delimiters ---
	LEFT_PAREN, RIGHT_PAREN,
	LEFT_BRACE, RIGHT_BRACE,
	LEFT_BRACKET, RIGHT_BRACKET,

	// --- Special Tokens ---
	EOF, // End Of File
	ERROR // For lexical errors
}