// File: src/main/java/com/juanpa.nebula.transpiler/ast/declarations/FieldDeclaration.java
package com.juanpa.nebula.transpiler.ast.declarations;

import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.expressions.Expression;
import com.juanpa.nebula.transpiler.lexer.Token;

import java.util.ArrayList; // Import ArrayList
import java.util.List;
import java.util.stream.Collectors; // Import Collectors

/**
 * Represents a field (member variable) declaration in a class.
 * Example: `public int myField = 10;`
 */
public class FieldDeclaration implements ASTNode
{
	private final List<Token> modifiers; // e.g., public, private, static, const
	private final Token type;            // The type of the field (e.g., int, String, MyCustomClass)
	private final Token name;            // The name of the field
	private final Expression initializer; // The optional initializer expression

	/**
	 * Constructs a new FieldDeclaration.
	 *
	 * @param modifiers   List of modifier tokens for the field.
	 * @param type        The token representing the field's type.
	 * @param name        The token representing the field's name.
	 * @param initializer The optional expression for the field's initial value, can be null.
	 */
	public FieldDeclaration(List<Token> modifiers, Token type, Token name, Expression initializer)
	{
		this.modifiers = new ArrayList<>(modifiers); // Make a copy
		this.type = type;
		this.name = name;
		this.initializer = initializer;
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

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		String mods = modifiers.isEmpty() ? "" : modifiers.stream()
				.map(Token::getLexeme)
				.collect(Collectors.joining(" ")) + " ";
		sb.append(mods).append("Field ").append(type.getLexeme()).append(" ").append(name.getLexeme());
		if(initializer != null)
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
		for(String line : text.split("\n"))
		{
			indentedText.append(prefix).append(line).append("\n");
		}
		return indentedText.toString();
	}
}
