package com.juanpa.nebula.compiler.semantics;

import com.juanpa.nebula.compiler.lexer.Token;

/**
 * Represents a variable (local variable, parameter, or field) in the symbol table.
 * Stores its name, type, initialization status, static status, and const status.
 */
public class VariableSymbol extends Symbol
{
	private boolean isInitialized;
	private final boolean isStatic;
	private final boolean isConst;
	private final boolean isWrapper;
	private final String cppTarget;
	private ClassSymbol ownerClass;
	private final Object constantValue;

	// --- NEW ---
	// Holds the inferred ownership kind. Defaults to SHARED for safety.
	private OwnershipKind ownership = OwnershipKind.SHARED;

	/**
	 * Full constructor for a VariableSymbol.
	 *
	 * @param name             The name of the variable.
	 * @param type             The type of the variable.
	 * @param declarationToken The token where it was declared.
	 * @param isInitialized    Whether the variable is initialized at declaration.
	 * @param isStatic         Whether the variable is static.
	 * @param isConst          Whether the variable is constant.
	 * @param isPublic         Whether the variable is public.
	 * @param isWrapper        Whether this is a wrapper for native C++ code.
	 * @param cppTarget        The native C++ code to generate if it's a wrapper.
	 */
	public VariableSymbol(String name, Type type, Token declarationToken, boolean isInitialized, boolean isStatic, boolean isConst, boolean isPublic, boolean isWrapper, String cppTarget, Object constantValue)
	{
		super(name, type, declarationToken, isPublic);
		this.isInitialized = isInitialized;
		this.isStatic = isStatic;
		this.isConst = isConst;
		this.isWrapper = isWrapper;
		this.cppTarget = cppTarget;
		this.constantValue = constantValue;
	}

	// Add a new getter for this field
	public Object getConstantValue()
	{
		return constantValue;
	}

	// --- CORRECTED CONSTRUCTOR CHAINS ---
	public VariableSymbol(String name, Type type, Token declarationToken, boolean isInitialized, boolean isStatic, boolean isConst, boolean isPublic, boolean isWrapper, String cppTarget)
	{
		this(name, type, declarationToken, isInitialized, isStatic, isConst, isPublic, isWrapper, cppTarget, null);
	}

	public VariableSymbol(String name, Type type, Token declarationToken, boolean isInitialized, boolean isStatic, boolean isConst, boolean isPublic)
	{
		this(name, type, declarationToken, isInitialized, isStatic, isConst, isPublic, false, null, null);
	}

	public VariableSymbol(String name, Type type, Token declarationToken, boolean isInitialized)
	{
		this(name, type, declarationToken, isInitialized, false, false, false, false, null, null);
	}

	public VariableSymbol(String name, Type type, Token declarationToken, boolean isInitialized, boolean isStatic, boolean isConst)
	{
		this(name, type, declarationToken, isInitialized, isStatic, isConst, false, false, null, null);
	}

	public boolean isInitialized()
	{
		return isInitialized;
	}

	public void setInitialized(boolean initialized)
	{
		this.isInitialized = initialized;
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

	// --- NEW: Getter and Setter for OwnershipKind ---
	public OwnershipKind getOwnership()
	{
		return ownership;
	}

	public void setOwnership(OwnershipKind ownership)
	{
		this.ownership = ownership;
	}


	@Override
	public String toString()
	{
		return "VariableSymbol{" +
				"name='" + getName() + '\'' +
				", type=" + getType() +
				", ownership=" + ownership + // Added for debugging
				", initialized=" + isInitialized +
				", static=" + isStatic() +
				", const=" + isConst() +
				", public=" + isPublic() +
				(ownerClass != null ? ", owner=" + ownerClass.getName() : "") +
				'}';
	}
}