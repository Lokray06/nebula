// File: src/main/java/com/juanpa/nebula/transpiler/ast/declarations/ConstructorDeclaration.java

package org.lokray.nebula.ast.declarations;

import org.lokray.nebula.ast.ASTNode;
import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.ast.statements.BlockStatement;
import org.lokray.nebula.ast.statements.ConstructorChainingCallStatement;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.semantics.MethodSymbol;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * AST node representing a constructor declaration.
 * Constructors do not have a return type and their name matches the class name.
 */
public class ConstructorDeclaration implements ASTNode
{
	private final List<Token> modifiers;
	private final Token name;
	private final List<Token> parameters;
	private final BlockStatement body;
	private MethodSymbol resolvedSymbol; // <-- ADD THIS FIELD

	// --- NEW FIELD ---
	// This will hold the 'this(...)' or 'super(...)' call if it exists.
	private ConstructorChainingCallStatement chainingCall;

	public ConstructorDeclaration(List<Token> modifiers, Token name, List<Token> parameters, BlockStatement body)
	{
		this.modifiers = new ArrayList<>(modifiers);
		this.name = name;
		this.parameters = new ArrayList<>(parameters);
		this.body = body;
		this.chainingCall = null; // Initialize to null
		this.resolvedSymbol = null; // <-- ADD THIS LINE
	}

	public List<Token> getModifiers()
	{
		return modifiers;
	}

	public Token getName()
	{
		return name;
	}

	public List<Token> getParameters()
	{
		return parameters;
	}

	public BlockStatement getBody()
	{
		return body;
	}

	public MethodSymbol getResolvedSymbol()
	{
		return resolvedSymbol;
	}

	public void setResolvedSymbol(MethodSymbol resolvedSymbol)
	{
		this.resolvedSymbol = resolvedSymbol;
	}

	// --- NEW GETTER ---
	public ConstructorChainingCallStatement getChainingCall()
	{
		return chainingCall;
	}

	// --- NEW SETTER ---
	public void setChainingCall(ConstructorChainingCallStatement chainingCall)
	{
		this.chainingCall = chainingCall;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitConstructorDeclaration(this);
	}

	// ... (toString and other helper methods remain the same) ...
	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		String mods = modifiers.isEmpty() ? "" : modifiers.stream()
				.map(Token::getLexeme)
				.collect(Collectors.joining(" ")) + " ";
		sb.append(mods).append(name.getLexeme());
		sb.append("(").append(formatParameters(parameters)).append(") {\n");
		sb.append(indent(body.toString(), 1)); // Indent body
		sb.append("}\n");
		return sb.toString();
	}

	private String formatParameters(List<Token> params)
	{
		if (params.isEmpty())
		{
			return "";
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < params.size(); i += 2)
		{ // Assuming pairs of (type, name)
			sb.append(params.get(i).getLexeme()).append(" ").append(params.get(i + 1).getLexeme());
			if (i + 2 < params.size())
			{
				sb.append(", ");
			}
		}
		return sb.toString();
	}

	private String indent(String text, int level)
	{
		StringBuilder indentedText = new StringBuilder();
		String prefix = "  ".repeat(level);
		for (String line : text.split("\n"))
		{
			indentedText.append(prefix).append(line).append("\n");
		}
		return indentedText.toString();
	}
}