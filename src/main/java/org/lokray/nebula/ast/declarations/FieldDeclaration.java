package org.lokray.nebula.ast.declarations;

import org.lokray.nebula.ast.ASTNode;
import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.ast.expressions.Expression;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.semantics.Type;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Represents a field (member variable) declaration in a class.
 * Example: `public int myField = 10;`
 */
public class FieldDeclaration implements ASTNode
{
	private final List<Token> modifiers; // e.g., public, private, static, const
	private final Token type;            // The token representing the type of the field (e.g., int, String, MyCustomClass)
	private final Token name;            // The name of the field
	private final Expression initializer; // The optional initializer expression

	// NEW: Field to store the resolved Type object after semantic analysis
	private Type resolvedType;

	private final boolean isWrapper;   // ADD THIS
	private final Token cppTarget;     // ADD THIS

	/**
	 * Constructs a new FieldDeclaration.
	 *
	 * @param modifiers   List of modifier tokens for the field.
	 * @param type        The token representing the field's type.
	 * @param name        The token representing the field's name.
	 * @param initializer The optional expression for the field's initial value, can be null.
	 * @param isWrapper   Used for native classes
	 * @param cppTarget   The token representing the cpp code it wraps
	 */
	public FieldDeclaration(List<Token> modifiers, Token type, Token name, Expression initializer, boolean isWrapper, Token cppTarget)
	{
		this.modifiers = new ArrayList<>(modifiers);
		this.type = type;
		this.name = name;
		this.initializer = initializer;
		this.isWrapper = isWrapper;     // ADD THIS
		this.cppTarget = cppTarget;     // ADD THIS
	}

	public List<Token> getModifiers()
	{
		return modifiers;
	}

	public Token getType()
	{
		return type;
	}

	public Token getName()
	{
		return name;
	}

	public Expression getInitializer()
	{
		return initializer;
	}

	// NEW: Getter for the resolved Type
	public Type getResolvedType()
	{
		return resolvedType;
	}

	// NEW: Setter for the resolved Type
	public void setResolvedType(Type resolvedType)
	{
		this.resolvedType = resolvedType;
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		String mods = modifiers.isEmpty() ? "" : modifiers.stream()
				.map(Token::getLexeme)
				.collect(Collectors.joining(" ")) + " ";
		sb.append(mods).append("Field ").append(type.getLexeme()).append(" ").append(name.getLexeme());
		if (initializer != null)
		{
			sb.append(" = ").append(initializer.toString()); // Recursively print initializer
		}
		sb.append(";\n");
		return sb.toString();
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitFieldDeclaration(this);
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

	public boolean isWrapper()
	{
		return isWrapper;
	}

	public Token getCppTarget()
	{
		return cppTarget;
	}
}