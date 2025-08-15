// File: src/main/java/com/juanpa.nebula.transpiler/ast/statements/VariableDeclarationStatement.java

package com.juanpa.nebula.transpiler.ast.statements;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.expressions.Expression;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType; // Import TokenType
import com.juanpa.nebula.transpiler.semantics.VariableSymbol;

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
	private final int arrayRank; // NEW: To store array dimensionality (0 for non-array)
	private final Token name;
	private final Expression initializer;
	private VariableSymbol resolvedSymbol; // <-- ADD THIS FIELD

	/**
	 * Constructs a VariableDeclarationStatement.
	 *
	 * @param modifiers   List of modifier tokens (e.g., CONST)
	 * @param typeToken   The token representing the base declared type.
	 * @param arrayRank   The rank of the array (e.g., 1 for int[], 2 for int[][]).
	 * @param name        The identifier token for the variable name.
	 * @param initializer Optional initializer expression.
	 */
	public VariableDeclarationStatement(List<Token> modifiers, Token typeToken, int arrayRank, Token name, Expression initializer)
	{
		this.modifiers = new ArrayList<>(modifiers); // Defensive copy
		this.typeToken = typeToken;
		this.arrayRank = arrayRank;
		this.name = name;
		this.initializer = initializer;
	}

	// Overloaded constructor for non-array types for convenience
	public VariableDeclarationStatement(List<Token> modifiers, Token typeToken, Token name, Expression initializer)
	{
		this(modifiers, typeToken, 0, name, initializer);
	}


	public List<Token> getModifiers()
	{ // Getter for modifiers
		return Collections.unmodifiableList(modifiers);
	}

	public Token getTypeToken()
	{
		return typeToken;
	}

	public int getArrayRank()
	{
		return arrayRank;
	}

	public Token getName()
	{
		return name;
	}

	public Expression getInitializer()
	{
		return initializer;
	}

	public VariableSymbol getResolvedSymbol()
	{
		return resolvedSymbol;
	}

	public void setResolvedSymbol(VariableSymbol resolvedSymbol)
	{
		this.resolvedSymbol = resolvedSymbol;
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
		sb.append("VarDecl: ").append(mods).append(typeToken.getLexeme());
		sb.append("[]".repeat(arrayRank)); // Append brackets for array type
		sb.append(" ").append(name.getLexeme());
		if (initializer != null)
		{
			sb.append(" = ").append(initializer);
		}
		return sb.toString();
	}
}