// File: src/main/java/com/juanpa/nebula/transpiler/ast/declarations/PropertyDeclaration.java
package org.lokray.nebula.ast.declarations;

import org.lokray.nebula.ast.ASTNode;
import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.semantics.PropertySymbol;

import java.util.List;

public class PropertyDeclaration implements ASTNode
{
	private final List<Token> modifiers;
	private final Token typeToken;
	private final int arrayRank; // --- ADD THIS ---
	private final Token name;
	private final AccessorDeclaration getAccessor;
	private final AccessorDeclaration setAccessor;
	private PropertySymbol resolvedSymbol;

	public PropertyDeclaration(List<Token> modifiers, Token typeToken, int arrayRank, Token name, AccessorDeclaration getAccessor, AccessorDeclaration setAccessor)
	{
		this.modifiers = modifiers;
		this.typeToken = typeToken;
		this.arrayRank = arrayRank; // --- ADD THIS ---
		this.name = name;
		this.getAccessor = getAccessor;
		this.setAccessor = setAccessor;

		if (getAccessor == null && setAccessor == null)
		{
			throw new IllegalArgumentException("Property must have at least a 'get' or 'set' accessor.");
		}
		if (getAccessor != null && setAccessor != null && getAccessor.isAuto() != setAccessor.isAuto())
		{
			throw new IllegalArgumentException("Cannot mix auto-implemented and full accessors in a property.");
		}
	}

	public List<Token> getModifiers()
	{
		return modifiers;
	}

	public Token getTypeToken()
	{
		return typeToken;
	}

	// --- ADD THIS GETTER ---
	public int getArrayRank()
	{
		return arrayRank;
	}

	public Token getName()
	{
		return name;
	}

	public AccessorDeclaration getGetAccessor()
	{
		return getAccessor;
	}

	public AccessorDeclaration getSetAccessor()
	{
		return setAccessor;
	}

	public boolean isAuto()
	{
		return (getAccessor != null && getAccessor.isAuto()) || (setAccessor != null && setAccessor.isAuto());
	}

	public PropertySymbol getResolvedSymbol()
	{
		return resolvedSymbol;
	}

	public void setResolvedSymbol(PropertySymbol resolvedSymbol)
	{
		this.resolvedSymbol = resolvedSymbol;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		return visitor.visitPropertyDeclaration(this);
	}
}