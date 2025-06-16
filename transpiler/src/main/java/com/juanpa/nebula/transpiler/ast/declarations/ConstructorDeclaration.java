// File: src/main/java/com/juanpa.nebula.transpiler/ast/declarations/ConstructorDeclaration.java

package com.juanpa.nebula.transpiler.ast.declarations;

import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.ast.ASTVisitor; // Import ASTVisitor
import com.juanpa.nebula.transpiler.ast.statements.BlockStatement;
import com.juanpa.nebula.transpiler.lexer.Token;

import java.util.ArrayList; // Import ArrayList
import java.util.List;
import java.util.stream.Collectors; // Import Collectors

/**
 * AST node representing a constructor declaration.
 * Constructors do not have a return type and their name matches the class name.
 */
public class ConstructorDeclaration implements ASTNode
{
	private final List<Token> modifiers; // e.g., PUBLIC, PRIVATE
	private final Token name;            // The constructor name (should match class name)
	private final List<Token> parameters; // For simplicity, just tokens for now
	private final BlockStatement body;

	public ConstructorDeclaration(List<Token> modifiers, Token name, List<Token> parameters, BlockStatement body)
	{
		this.modifiers = new ArrayList<>(modifiers); // Make a copy
		this.name = name;
		this.parameters = new ArrayList<>(parameters); // Make a copy
		this.body = body;
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

	@Override
	public <R> R accept(ASTVisitor<R> visitor) {
		return visitor.visitConstructorDeclaration(this);
	}

	private String formatParameters(List<Token> params) {
		if (params.isEmpty()) return "";
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < params.size(); i += 2) { // Assuming pairs of (type, name)
			sb.append(params.get(i).getLexeme()).append(" ").append(params.get(i+1).getLexeme());
			if (i + 2 < params.size()) {
				sb.append(", ");
			}
		}
		return sb.toString();
	}

	private String indent(String text, int level) {
		StringBuilder indentedText = new StringBuilder();
		String prefix = "  ".repeat(level);
		for (String line : text.split("\n")) {
			indentedText.append(prefix).append(line).append("\n");
		}
		return indentedText.toString();
	}
}

