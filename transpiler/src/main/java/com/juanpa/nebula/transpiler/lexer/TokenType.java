// File: src/main/java/com/juanpa/nebula/transpiler/lexer/TokenType.java
package com.juanpa.nebula.transpiler.lexer;

/**
 * Defines the types of tokens recognized by the Nebula Lexer.
 * This enum covers keywords, operators, literals, punctuation, and special tokens.
 */
public enum TokenType
{
	// --- Keywords ---
	// Access Modifiers & Declarations
	CLASS, PUBLIC, PRIVATE, STATIC, EXTENDS, NAMESPACE, NATIVE, WRAPPER, SUPER, ALIAS, IMPORT,

	// Control Flow
	IF, ELSE, FOR, FOREACH, IN, WHILE, DO, SWITCH, CASE, DEFAULT, RETURN, THROW, TRY, CATCH, FINALLY,

	// Primitives & Types
	VOID, BOOL, CHAR, CHAR16, CHAR32, BYTE, SHORT, INT, LONG, FLOAT, DOUBLE,
	INT8, INT16, INT32, INT64,
	UINT8, UINT16, UINT32, UINT64,
	UBYTE, USHORT, UINT, ULONG,
	STRING_KEYWORD, VAR, CONST, IS,

	// Other
	NEW, THIS, GLOBAL, OPERATOR, GET, SET, RESULT,

	// --- Literals ---
	IDENTIFIER,
	INTEGER_LITERAL,
	STRING_LITERAL,
	INTERPOLATED_STRING_LITERAL, // <-- ADD THIS LINE
	CHAR_LITERAL,
	BOOLEAN_LITERAL,
	FLOAT_LITERAL,
	DOUBLE_LITERAL,
	NULL,

	// --- Punctuation & Delimiters ---
	LEFT_PAREN, RIGHT_PAREN,       // ( )
	LEFT_BRACE, RIGHT_BRACE,       // { }
	LEFT_BRACKET, RIGHT_BRACKET,   // [ ]
	DOT, COMMA, SEMICOLON, COLON, QUESTION,

	// --- Operators ---
	// Unary
	PLUS_PLUS, MINUS_MINUS,         // ++ --
	BANG,                     // !

	// Multiplicative
	STAR, SLASH, MODULO,             // * / %

	// Additive
	PLUS, MINUS,                     // + -

	// Shift
	LEFT_SHIFT, RIGHT_SHIFT,         // << >>

	// Relational & Equality
	LESS, LESS_EQUAL,                // < <=
	GREATER, GREATER_EQUAL,          // > >=
	EQUAL_EQUAL, BANG_EQUAL,        // == !=

	// Bitwise
	AMPERSAND,                      // &
	PIPE,                           // |
	XOR,                            // ^
	BITWISE_NOT,                    // ~ (TILDE is new for bitwise NOT)

	// Logical
	AMPERSAND_AMPERSAND,             // &&
	PIPE_PIPE,                       // ||

	// Assignment & Compound Assignment
	ASSIGN,                          // =
	PLUS_ASSIGN, MINUS_ASSIGN,       // += -=
	STAR_ASSIGN, SLASH_ASSIGN,       // *= /=
	MODULO_ASSIGN,                   // %=
	AMPERSAND_ASSIGN, PIPE_ASSIGN,   // &= |=
	XOR_ASSIGN,                      // ^=
	LEFT_SHIFT_ASSIGN, RIGHT_SHIFT_ASSIGN, // <<= >>=
	POWER_ASSIGN,                    // **= (NEW)

	// Other
	ARROW,                           // ->
	POWER,                           // **
	NULL_COALESCING,                 // ?? (Reserved for future use)

	// --- Special Tokens ---
	EOF, // End Of File
	ERROR // For lexical errors
}