// File: src/main/java/com/juanpa/nebula/transpiler/ast/statements/SwitchCase.java

package com.juanpa.nebula.transpiler.ast.statements;

import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.expressions.Expression; // The constant value of the case
import com.juanpa.nebula.transpiler.lexer.Token; // Import Token

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors; // Import Collectors

public class SwitchCase implements ASTNode
{
	private final Expression value; // The constant expression for the case (e.g., 10, 'A')
	private final List<Statement> body; // The statements within this case
	private final Token caseKeyword; // The 'case' keyword token for error reporting

	public SwitchCase(Token caseKeyword, Expression value, List<Statement> body)
	{
		this.caseKeyword = caseKeyword;
		this.value = value;
		this.body = new ArrayList<>(body); // Defensive copy
	}

	public Token getCaseKeyword()
	{
		return caseKeyword;
	}

	public Expression getValue()
	{
		return value;
	}

	public List<Statement> getBody()
	{
		return Collections.unmodifiableList(body);
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitSwitchCase(this);
	}

	@Override
	public String toString()
	{
		return "  case " + value.toString() + ":\n" +
				body.stream().map(s -> "    " + s.toString()).collect(Collectors.joining("\n")) + "\n";
	}
}
