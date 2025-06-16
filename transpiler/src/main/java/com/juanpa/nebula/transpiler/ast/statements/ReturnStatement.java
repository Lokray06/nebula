// File: src/main/java/com/juanpa.nebula.transpiler/ast/statements/ReturnStatement.java
package com.juanpa.nebula.transpiler.ast.statements;

import com.juanpa.nebula.transpiler.ast.ASTVisitor; // Import ASTVisitor
import com.juanpa.nebula.transpiler.ast.expressions.Expression;
import com.juanpa.nebula.transpiler.lexer.Token;

/**
 * AST node representing a return statement.
 * Can optionally include an expression to be returned.
 */
public class ReturnStatement implements Statement
{
	private final Token keyword; // The 'return' keyword token
	private final Expression value; // Optional expression to return

	public ReturnStatement(Token keyword, Expression value)
	{
		this.keyword = keyword;
		this.value = value;
	}

	public Token getKeyword()
	{
		return keyword;
	}

	public Expression getValue()
	{
		return value;
	}

	@Override
	public String toString()
	{
		String val = value != null ? " " + value.toString() : "";
		return "Return" + val + ";\n";
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitReturnStatement(this);
	}
}

