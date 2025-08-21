// File: src/main/java/com/juanpa/nebula/transpiler/semantics/PropertySymbol.java
package com.juanpa.nebula.transpiler.semantics;

import com.juanpa.nebula.transpiler.lexer.Token;

public class PropertySymbol extends Symbol
{
	private final boolean isStatic;
	private MethodSymbol getter;
	private MethodSymbol setter;
	private ClassSymbol ownerClass; // <-- ADD THIS LINE
	private VariableSymbol backingField;

	public PropertySymbol(String name, Type type, Token declarationToken, boolean isStatic, boolean isPublic)
	{
		super(name, type, declarationToken, isPublic);
		this.isStatic = isStatic;
	}

	@Override
	public boolean isStatic()
	{
		return isStatic;
	}

	public MethodSymbol getGetter()
	{
		return getter;
	}

	public void setGetter(MethodSymbol getter)
	{
		this.getter = getter;
	}

	public MethodSymbol getSetter()
	{
		return setter;
	}

	public void setSetter(MethodSymbol setter)
	{
		this.setter = setter;
	}

	public ClassSymbol getOwnerClass()
	{
		return ownerClass;
	}

	public void setOwnerClass(ClassSymbol ownerClass)
	{
		this.ownerClass = ownerClass;
	}

	public void setBackingField(VariableSymbol backingField)
	{
		this.backingField = backingField;
	}

	public VariableSymbol getBackingField()
	{
		return this.backingField;
	}
}