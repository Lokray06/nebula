package com.juanpa.nebula.transpiler.ast;

/**
 * Base interface for all nodes in the Abstract Syntax Tree (AST).
 * All elements that form the structured representation of the program
 * will implement this interface.
 */
public interface ASTNode
{
	/**
	 * Accepts an ASTVisitor to traverse this node.
	 * This is part of the Visitor design pattern, allowing operations to be
	 * performed on the AST nodes without modifying the node classes themselves.
	 *
	 * @param visitor The ASTVisitor instance.
	 * @param <R>     The return type of the visitor's visit methods.
	 * @return The result of the visitor's operation.
	 */
	<R> R accept(ASTVisitor<R> visitor);
}
