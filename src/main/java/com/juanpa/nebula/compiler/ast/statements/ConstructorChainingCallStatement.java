// File: src/main/java/com/juanpa/nebula/transpiler/ast/statements/ConstructorChainingCallStatement.java

package com.juanpa.nebula.compiler.ast.statements;

import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.ast.expressions.Expression;
import com.juanpa.nebula.compiler.lexer.Token;

import java.util.Collections;
import java.util.List;

/**
 * AST node representing a constructor chaining call within a constructor (e.g., `this(...)` or `super(...)`).
 * This is a statement, not an expression.
 */
public class ConstructorChainingCallStatement implements Statement
{
	private final Token keyword; // The 'this' or 'super' keyword
	private final Token leftParen; // The opening parenthesis token
	private final List<Expression> arguments;
	private final Token rightParen; // The closing parenthesis token

	public ConstructorChainingCallStatement(Token keyword, Token leftParen, List<Expression> arguments, Token rightParen)
	{
		this.keyword = keyword;
		this.leftParen = leftParen;
		this.arguments = arguments;
		this.rightParen = rightParen;
	}

	public Token getKeyword()
	{
		return keyword;
	}

	public List<Expression> getArguments()
	{
		return Collections.unmodifiableList(arguments);
	}

	public Token getFirstToken()
	{
		return keyword;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitConstructorChainingCallStatement(this);
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append(keyword.getLexeme()).append("(");
		for(int i = 0; i < arguments.size(); i++)
		{
			sb.append(arguments.get(i).toString());
			if(i < arguments.size() - 1)
			{
				sb.append(", ");
			}
		}
		sb.append(");");
		return sb.toString();
	}
}