// File: src/main/java/com/juanpa.nebula.transpiler/semantics/VariableSymbol.java

package com.juanpa.nebula.transpiler.semantics;

import com.juanpa.nebula.transpiler.lexer.Token;

/**
 * Represents a variable (local variable, parameter, or field) in the symbol table.
 * Stores its name, type, initialization status, static status, and const status.
 */
public class VariableSymbol extends Symbol
{
	private boolean initialized;
	private final boolean isStatic;
	private final boolean isConst;
	private final boolean isWrapper; // ADD THIS
	private final String cppTarget;  // ADD THIS
	private ClassSymbol ownerClass;

	/**
	 * Constructs a VariableSymbol.
	 *
	 * @param name             The name of the variable.
	 * @param type             The type of the variable.
	 * @param declarationToken The token where this variable was declared.
	 * @param initialized      True if the variable is initialized at declaration, false otherwise.
	 * @param isStatic         True if this is a static field, false otherwise.
	 * @param isConst          True if this is a constant variable/field, false otherwise.
	 * @param isPublic         True if this field is public, false otherwise. (Relevant for fields)
	 */
	public VariableSymbol(String name, Type type, Token declarationToken, boolean initialized, boolean isStatic, boolean isConst, boolean isPublic, boolean isWrapper, String cppTarget)
	{
		super(name, type, declarationToken, isPublic);
		this.initialized = initialized;
		this.isStatic = isStatic;
		this.isConst = isConst;
		this.ownerClass = null;
		this.isWrapper = isWrapper; // ADD THIS
		this.cppTarget = cppTarget; // ADD THIS
	}

	// To maintain compatibility, you need to create a constructor chain.
	// The most specific one is above. Let's adapt the existing ones.
	public VariableSymbol(String name, Type type, Token declarationToken, boolean initialized, boolean isStatic, boolean isConst, boolean isPublic)
	{
		this(name, type, declarationToken, initialized, isStatic, isConst, isPublic, false, null);
	}

	public VariableSymbol(String name, Type type, Token declarationToken, boolean initialized)
	{
		this(name, type, declarationToken, initialized, false, false, false, false, null);
	}

	public VariableSymbol(String name, Type type, Token declarationToken, boolean initialized, boolean isStatic, boolean isConst)
	{
		this(name, type, declarationToken, initialized, isStatic, isConst, false, false, null);
	}


	public boolean isInitialized()
	{
		return initialized;
	}

	public void setInitialized(boolean initialized)
	{
		this.initialized = initialized;
	}

	@Override
	public boolean isStatic()
	{
		return isStatic;
	}

	/**
	 * Indicates whether this variable/field is constant.
	 *
	 * @return True if the variable/field is constant, false otherwise.
	 */
	public boolean isConst()
	{
		return isConst;
	}

	public boolean isWrapper()
	{
		return isWrapper;
	}

	public String getCppTarget()
	{
		return cppTarget;
	}

	/**
	 * Gets the owning ClassSymbol for this variable, if it is a field.
	 *
	 * @return The ClassSymbol that owns this variable, or null if it's a local variable or parameter.
	 */
	public ClassSymbol getOwnerClass()
	{
		return ownerClass;
	}

	/**
	 * Sets the owning ClassSymbol for this variable. This should be called by the SemanticAnalyzer
	 * when defining fields within a class.
	 *
	 * @param ownerClass The ClassSymbol that owns this variable.
	 */
	public void setOwnerClass(ClassSymbol ownerClass)
	{
		this.ownerClass = ownerClass;
	}


	@Override
	public String toString()
	{
		return "VariableSymbol{" + "name='" + getName() + '\'' + ", type=" + getType() + ", initialized=" + initialized + ", static=" + isStatic() + ", const=" + isConst() + ", public=" + isPublic() + (ownerClass != null ? ", owner=" + ownerClass.getName() : "") + '}';
	}
}
