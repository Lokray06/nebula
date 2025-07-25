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
	private final boolean isStatic; // Indicates if it's a static field
	private final boolean isConst;  // Indicates if it's a constant variable/field
	private ClassSymbol ownerClass; // NEW: Reference to the ClassSymbol this variable (if a field) belongs to


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
	public VariableSymbol(String name, Type type, Token declarationToken, boolean initialized, boolean isStatic, boolean isConst, boolean isPublic)
	{
		super(name, type, declarationToken, isPublic); // Pass isPublic to super
		this.initialized = initialized;
		this.isStatic = isStatic;
		this.isConst = isConst;
		this.ownerClass = null; // Initialize to null; set by SemanticAnalyzer for fields
	}

	/**
	 * Constructs a VariableSymbol (for non-static, non-const, non-public variables, e.g., local vars, params).
	 */
	public VariableSymbol(String name, Type type, Token declarationToken, boolean initialized)
	{
		this(name, type, declarationToken, initialized, false, false, false); // Default to non-static, non-const, non-public
	}

	// Overloaded constructor for parameters/local variables where public/static/const might be less relevant or default
	public VariableSymbol(String name, Type type, Token declarationToken, boolean initialized, boolean isStatic, boolean isConst)
	{
		this(name, type, declarationToken, initialized, isStatic, isConst, false); // Default to non-public
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
