// File: src/main/java/com/juanpa/nebula/transpiler/semantics/InstanceSymbol.java
package com.juanpa.nebula.transpiler.semantics;

import com.juanpa.nebula.transpiler.lexer.Token;

public class InstanceSymbol extends Symbol
{
	private final ClassSymbol classRepresented; // The ClassSymbol this instance is of

	public InstanceSymbol(String name, Type type, Token declarationToken, ClassSymbol classRepresented)
	{
		super(name, type, declarationToken, false); // Instances are never public in themselves
		this.classRepresented = classRepresented;
	}

	@Override
	public boolean isStatic()
	{
		return false; // An instance is never static
	}

	public ClassSymbol getClassRepresented()
	{
		return classRepresented;
	}

	@Override
	public String toString()
	{
		return "InstanceSymbol: " + getName() + " (Type: " + getType().getName() + ")";
	}
}