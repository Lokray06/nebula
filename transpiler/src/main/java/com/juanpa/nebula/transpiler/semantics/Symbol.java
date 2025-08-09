// File: src/main/java/com/juanpa.nebula.transpiler/semantics/Symbol.java

package com.juanpa.nebula.transpiler.semantics;

import com.juanpa.nebula.transpiler.lexer.Token;

/**
 * Abstract base class for all symbols in the symbol table.
 * A symbol represents a declared entity in the program (e.g., variable, method, class).
 */
public abstract class Symbol
{
	private final String name;
	private Type type;
	private final Token declarationToken; // The token where this symbol was declared
	protected boolean isPublic; // Add this field for visibility status

	/**
	 * Constructor for a Symbol.
	 *
	 * @param name             The name of the symbol.
	 * @param type             The type of the symbol.
	 * @param declarationToken The token representing the declaration of this symbol.
	 * @param isPublic         True if the symbol is public, false otherwise.
	 */
	public Symbol(String name, Type type, Token declarationToken, boolean isPublic)
	{
		this.name = name;
		this.type = type;
		this.declarationToken = declarationToken;
		this.isPublic = isPublic;
	}

	/**
	 * Constructor for a Symbol (defaults to non-public).
	 *
	 * @param name             The name of the symbol.
	 * @param type             The type of the symbol.
	 * @param declarationToken The token representing the declaration of this symbol.
	 */
	public Symbol(String name, Type type, Token declarationToken)
	{
		this(name, type, declarationToken, false); // Default to non-public
	}

	public String getName()
	{
		return name;
	}

	public Type getType()
	{
		return type;
	}

	public void setType(Type type)
	{
		this.type = type;
	}

	public Token getDeclarationToken()
	{
		return declarationToken;
	}

	/**
	 * Indicates whether this symbol represents a static member (field or method).
	 *
	 * @return True if the symbol is static, false otherwise.
	 */
	public abstract boolean isStatic();

	/**
	 * Indicates whether this symbol is public.
	 *
	 * @return True if the symbol is public, false otherwise.
	 */
	public boolean isPublic()
	{
		return isPublic;
	}

	/**
	 * Sets the public status of this symbol.
	 *
	 * @param aPublic True to mark the symbol as public, false otherwise.
	 */
	public void setPublic(boolean aPublic)
	{
		isPublic = aPublic;
	}

	@Override
	public String toString()
	{
		return "Symbol{" + "name='" + name + '\'' + ", type=" + type + ", line=" + declarationToken.getLine() + ", public=" + isPublic + '}';
	}
}
