// File: src/main/java/com/juanpa.nebula.transpiler/ast/statements/VariableDeclarationStatement.java

package com.juanpa.nebula.transpiler.ast.statements;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.expressions.Expression;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType; // Import TokenType

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * AST node representing a variable declaration statement.
 * Includes the type token (e.g., INT, STRING, VAR, or IDENTIFIER for custom types),
 * the variable name, and an optional initializer expression.
 */
public class VariableDeclarationStatement implements Statement
{
	private final List<Token> modifiers; // Add modifiers
	private final Token typeToken;
	private final Token name;
	private final Expression initializer;

	/**
	 * Constructs a VariableDeclarationStatement.
	 *
	 * @param modifiers   List of modifier tokens (e.g., CONST)
	 * @param typeToken   The token representing the declared type.
	 * @param name        The identifier token for the variable name.
	 * @param initializer Optional initializer expression.
	 */
	public VariableDeclarationStatement(List<Token> modifiers, Token typeToken, Token name, Expression initializer)
	{
		this.modifiers = new ArrayList<>(modifiers); // Defensive copy
		this.typeToken = typeToken;
		this.name = name;
		this.initializer = initializer;
	}

	// Existing constructor for backward compatibility (assume no modifiers)
	public VariableDeclarationStatement(Token typeToken, Token name, Expression initializer)
	{
		this(new ArrayList<>(), typeToken, name, initializer);
	}

	public List<Token> getModifiers()
	{ // Getter for modifiers
		return Collections.unmodifiableList(modifiers);
	}

	public Token getTypeToken()
	{
		return typeToken;
	}

	public Token getName()
	{
		return name;
	}

	public Expression getInitializer()
	{
		return initializer;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitVariableDeclarationStatement(this);
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		String mods = modifiers.isEmpty() ? "" : modifiers.stream()
				.map(Token::getLexeme)
				.collect(Collectors.joining(" ")) + " ";
		sb.append("VarDecl: ").append(mods).append(typeToken.getLexeme()).append(" ").append(name.getLexeme());
		if(initializer != null)
		{
			sb.append(" = ").append(initializer);
		}
		return sb.toString();
	}
}
