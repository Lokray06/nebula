// File: src/main/java/com/juanpa.nebula.transpiler/ast/statements/IfStatement.java
package com.juanpa.nebula.transpiler.ast.statements;

import com.juanpa.nebula.transpiler.ast.ASTVisitor; // Import ASTVisitor
import com.juanpa.nebula.transpiler.ast.expressions.Expression;
import com.juanpa.nebula.transpiler.lexer.Token; // Import Token

/**
 * AST node representing an 'if-else' statement.
 * Includes a condition, a 'then' branch (which can be a single statement or a block),
 * and an optional 'else' branch (which can also be a single statement or a block).
 */
public class IfStatement implements Statement
{
	private final Token ifKeyword;       // The 'if' keyword token
	private final Expression condition;
	private final Statement thenBranch;  // CHANGED: From BlockStatement to Statement
	private final Statement elseBranch;  // Already Statement

	/**
	 * Constructs an IfStatement.
	 *
	 * @param ifKeyword  The 'if' keyword token.
	 * @param condition  The expression for the condition.
	 * @param thenBranch The statement or block to execute if the condition is true.
	 * @param elseBranch The optional statement or block to execute if the condition is false.
	 */
	public IfStatement(Token ifKeyword, Expression condition, Statement thenBranch, Statement elseBranch)
	{
		this.ifKeyword = ifKeyword;
		this.condition = condition;
		this.thenBranch = thenBranch;
		this.elseBranch = elseBranch;
	}

	public Token getIfKeyword()
	{
		return ifKeyword;
	}

	public Expression getCondition()
	{
		return condition;
	}

	public Statement getThenBranch() // CHANGED: Return type is Statement
	{
		return thenBranch;
	}

	public Statement getElseBranch()
	{
		return elseBranch;
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder("If (").append(condition).append(") ");

		// For the 'then' branch:
		// If it's a BlockStatement, print it with its curly braces.
		// If it's a single statement, print it directly (its own toString will handle newline).
		// We'll indent the inner content in either case.
		if(thenBranch instanceof BlockStatement)
		{
			sb.append("{\n");
			sb.append(indent(thenBranch.toString(), 1));
			sb.append("}");
		}
		else
		{
			sb.append(indent(thenBranch.toString(), 0).trim()); // Trim to avoid extra newlines if `statement().toString()` adds one
		}


		if(elseBranch != null)
		{
			sb.append(" Else ");
			// For the 'else' branch:
			if(elseBranch instanceof BlockStatement)
			{
				sb.append("{\n");
				sb.append(indent(elseBranch.toString(), 1));
				sb.append("}");
			}
			else
			{
				sb.append(indent(elseBranch.toString(), 0).trim());
			}
		}
		sb.append("\n"); // Ensure a consistent newline at the end of the entire if statement
		return sb.toString();
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitIfStatement(this);
	}

	// Helper for indentation
	private String indent(String text, int level)
	{
		StringBuilder indentedText = new StringBuilder();
		String prefix = "  ".repeat(level);
		// Split by any Unicode newline sequence to handle different OS line endings
		String[] lines = text.split("\\R");
		for(int i = 0; i < lines.length; i++)
		{
			if(!lines[i].trim().isEmpty())
			{ // Only indent non-empty lines
				indentedText.append(prefix).append(lines[i]);
			}
			else
			{
				indentedText.append(lines[i]); // Keep empty lines as is
			}
			if(i < lines.length - 1)
			{
				indentedText.append("\n"); // Add newline back for all but the last line
			}
		}
		return indentedText.toString();
	}

	public Token getFirstToken()
	{
		return ifKeyword; // The 'if' keyword is the first token of the IfStatement
	}
}