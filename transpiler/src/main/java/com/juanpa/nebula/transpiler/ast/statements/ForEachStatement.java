// File: src/main/java/com/juanpa/nebula/transpiler/ast/statements/ForEachStatement.java
package com.juanpa.nebula.transpiler.ast.statements;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.expressions.Expression;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.semantics.VariableSymbol;

/**
 * AST node for a 'foreach' loop statement.
 * Syntax: foreach (Type variableName in collection) { body }
 */
public class ForEachStatement implements Statement
{

	private final Token foreachKeyword;
	private final Token typeToken;
	private final int arrayRank;
	private final Token variableName;
	private final Expression collection;
	private final BlockStatement body;
	private VariableSymbol resolvedLoopVariable; // <-- ADD THIS FIELD

	public ForEachStatement(Token foreachKeyword, Token typeToken, int arrayRank, Token variableName, Expression collection, BlockStatement body)
	{
		this.foreachKeyword = foreachKeyword;
		this.typeToken = typeToken;
		this.arrayRank = arrayRank;
		this.variableName = variableName;
		this.collection = collection;
		this.body = body;
	}

	public Token getForeachKeyword()
	{
		return foreachKeyword;
	}

	public Token getTypeToken()
	{
		return typeToken;
	}

	public int getArrayRank()
	{
		return arrayRank;
	}

	public Token getVariableName()
	{
		return variableName;
	}

	public Expression getCollection()
	{
		return collection;
	}

	public BlockStatement getBody()
	{
		return body;
	}

	public VariableSymbol getResolvedLoopVariable()
	{
		return resolvedLoopVariable;
	}

	public void setResolvedLoopVariable(VariableSymbol resolvedLoopVariable)
	{
		this.resolvedLoopVariable = resolvedLoopVariable;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitForEachStatement(this);
	}
}