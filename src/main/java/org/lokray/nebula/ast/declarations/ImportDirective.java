// File: src/main/java/com/juanpa/nebula/transpiler/ast/declarations/ImportDirective.java
// Changed: File name from UsingDirective.java to ImportDirective.java

package org.lokray.nebula.ast.declarations;

import org.lokray.nebula.ast.ASTNode;
import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.ast.expressions.Expression;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.lexer.TokenType;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Represents an 'import' directive in Nebula source code.
 * It can be a standard namespace import, a static import,
 * or a global import.
 * Examples:
 * import System.Text;
 * import static System.Console;
 * global import MyNamespace;
 * global static import AnotherType;
 */
public class ImportDirective implements ASTNode
{ // Changed: Class name to ImportDirective
	private final List<Token> modifiers; // Tokens for 'global', 'static'
	private final Expression qualifiedName; // The namespace or type name (e.g., System.Text, System.Console)
	private final Token importKeyword; // Changed: The 'import' keyword token

	public ImportDirective(List<Token> modifiers, Token importKeyword, Expression qualifiedName)
	{ // Changed: Constructor parameter to importKeyword
		this.modifiers = modifiers; // Assume the list is already a copy if needed
		this.importKeyword = importKeyword; // Changed: Assignment
		this.qualifiedName = qualifiedName;
	}

	public List<Token> getModifiers()
	{
		return modifiers;
	}

	public boolean isGlobal()
	{
		return modifiers.stream().anyMatch(m -> m.getType() == TokenType.GLOBAL);
	}

	public boolean isStatic()
	{
		return modifiers.stream().anyMatch(m -> m.getType() == TokenType.STATIC);
	}

	public Expression getQualifiedName()
	{
		return qualifiedName;
	}

	public Token getImportKeyword()
	{ // Changed: Getter to getImportKeyword
		return importKeyword;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitImportDirective(this); // Changed: visitUsingDirective to visitImportDirective
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		if(!modifiers.isEmpty())
		{
			sb.append(modifiers.stream().map(Token::getLexeme).collect(Collectors.joining(" "))).append(" ");
		}
		sb.append("import ").append(qualifiedName.toString()).append(";"); // Changed: "using" to "import"
		return sb.toString();
	}
}
