// File: src/main/java/com/juanpa.nebula.transpiler/ast/expressions/NewExpression.java

package org.lokray.nebula.ast.expressions;

import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.lexer.TokenType;
import org.lokray.nebula.semantics.MethodSymbol;
import org.lokray.nebula.semantics.Symbol;
import org.lokray.nebula.semantics.Type;

import java.util.Collections;
import java.util.List;

/**
 * AST node representing an object instantiation expression (e.g., `new MyClass(arg1, arg2)`).
 */
public class NewExpression implements Expression
{
	private final Token newKeyword; // The 'new' keyword token
	private final Expression className; // The class name being instantiated (IdentifierExpression or DotExpression)
	private final Token paren; // The opening parenthesis token for argument list
	private final List<Expression> arguments; // List of arguments passed to the constructor
	private MethodSymbol resolvedConstructor; // Added to store the resolved constructor symbol
	private Type resolvedType; // NEW: Field for resolved type of the NewExpression

	public NewExpression(Token newKeyword, Expression className, Token paren, List<Expression> arguments)
	{
		if(newKeyword.getType() != TokenType.NEW)
		{
			throw new IllegalArgumentException("Token for NewExpression must be 'new'.");
		}
		if(paren.getType() != TokenType.LEFT_PAREN)
		{
			throw new IllegalArgumentException("Token for NewExpression's parenthesis must be '('.");
		}
		this.newKeyword = newKeyword;
		this.className = className;
		this.paren = paren;
		this.arguments = Collections.unmodifiableList(arguments);
	}

	public Token getNewKeyword()
	{
		return newKeyword;
	}

	public Expression getClassName()
	{
		return className;
	}

	public Token getParen()
	{
		return paren;
	}

	public List<Expression> getArguments()
	{
		return arguments;
	}

	// Getter for resolved constructor
	public MethodSymbol getResolvedConstructor()
	{
		return resolvedConstructor;
	}

	// Setter for resolved constructor
	public void setResolvedConstructor(MethodSymbol resolvedConstructor)
	{
		this.resolvedConstructor = resolvedConstructor;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitNewExpression(this);
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append(newKeyword.getLexeme()).append(" ");
		sb.append(className.toString());
		sb.append("(");
		for(int i = 0; i < arguments.size(); i++)
		{
			sb.append(arguments.get(i).toString());
			if(i < arguments.size() - 1)
			{
				sb.append(", ");
			}
		}
		sb.append(")");
		return sb.toString();
	}

	@Override
	public Token getFirstToken()
	{
		return newKeyword;
	}

	@Override
	public Symbol getResolvedSymbol()
	{
		// The resolved symbol of a new expression is its resolved constructor
		return resolvedConstructor;
	}

	@Override
	public void setResolvedSymbol(Symbol symbol)
	{
		// If the symbol is a MethodSymbol (constructor), set it.
		if (symbol instanceof MethodSymbol) {
			this.resolvedConstructor = (MethodSymbol) symbol;
		}
	}

	// NEW: Implementation for getResolvedType()
	@Override
	public Type getResolvedType() {
		// The type of a new expression is the return type of its resolved constructor,
		// which is typically the class type itself (or void for constructors).
		// SemanticAnalyzer should set the type of the expression directly.
		return resolvedType;
	}

	// Setter for the resolved type
	public void setResolvedType(Type resolvedType) {
		this.resolvedType = resolvedType;
	}
}
