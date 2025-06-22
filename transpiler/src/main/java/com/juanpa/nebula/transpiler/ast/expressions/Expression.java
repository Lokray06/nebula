// File: src/main/java/com/juanpa/nebula/transpiler/ast/expressions/Expression.java

package com.juanpa.nebula.transpiler.ast.expressions;

import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.semantics.Symbol;
import com.juanpa.nebula.transpiler.semantics.Type; // <-- ADD THIS IMPORT

/**
 * Base interface for all expression nodes in the Abstract Syntax Tree (AST).
 * Expressions are parts of the program that produce a value.
 */
public interface Expression extends ASTNode
{
	/**
	 * Returns the first token that constitutes this expression.
	 * Useful for error reporting to pinpoint the exact location of a semantic error.
	 *
	 * @return The first Token of this expression.
	 */
	Token getFirstToken();

	/**
	 * Retrieves the symbol that this expression was resolved to during semantic analysis.
	 *
	 * @return The resolved Symbol, or null if not yet resolved or not applicable.
	 */
	Symbol getResolvedSymbol();

	/**
	 * Sets the resolved symbol for this expression. This is called by the SemanticAnalyzer.
	 *
	 * @param symbol The symbol that this expression resolves to.
	 */
	void setResolvedSymbol(Symbol symbol);

	/**
	 * Retrieves the resolved type of this expression after semantic analysis.
	 * This is a convenience method that usually delegates to `getResolvedSymbol().getType()`.
	 *
	 * @return The resolved Type, or null if not yet resolved or not applicable.
	 */
	Type getResolvedType(); // NEW METHOD

	void setResolvedType(Type resolvedType);
}
