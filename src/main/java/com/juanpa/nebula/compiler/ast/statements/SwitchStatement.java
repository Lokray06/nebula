// File: src/main/java/com/juanpa/nebula/transpiler/ast/statements/SwitchStatement.java

package com.juanpa.nebula.compiler.ast.statements;

import com.juanpa.nebula.compiler.ast.ASTVisitor;
import com.juanpa.nebula.compiler.ast.expressions.Expression;
import com.juanpa.nebula.compiler.lexer.Token;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SwitchStatement implements Statement
{
	private final Expression switchExpression; // The expression to switch on
	private final List<SwitchCase> cases;    // List of 'case' clauses
	private final BlockStatement defaultBlock; // Optional 'default' block
	private final Token switchKeyword; // The 'switch' keyword token for error reporting

	public SwitchStatement(Token switchKeyword, Expression switchExpression, List<SwitchCase> cases, BlockStatement defaultBlock)
	{
		this.switchKeyword = switchKeyword;
		this.switchExpression = switchExpression;
		this.cases = new ArrayList<>(cases); // Defensive copy
		this.defaultBlock = defaultBlock;
	}

	public Token getSwitchKeyword()
	{
		return switchKeyword;
	}

	public Expression getSwitchExpression()
	{
		return switchExpression;
	}

	public List<SwitchCase> getCases()
	{
		return Collections.unmodifiableList(cases);
	}

	public BlockStatement getDefaultBlock()
	{
		return defaultBlock;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitSwitchStatement(this);
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder("switch (").append(switchExpression.toString()).append(") {\n");
		for(SwitchCase sc : cases)
		{
			sb.append(sc.toString());
		}
		if(defaultBlock != null)
		{
			sb.append("  default:\n").append(defaultBlock.toString()).append("\n");
		}
		sb.append("}\n");
		return sb.toString();
	}
}
