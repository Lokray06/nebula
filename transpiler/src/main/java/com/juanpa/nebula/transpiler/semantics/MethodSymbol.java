// File: src/main/java/com/juanpa.nebula.transpiler/semantics/MethodSymbol.java

package com.juanpa.nebula.transpiler.semantics;

import com.juanpa.nebula.transpiler.lexer.Token;

import java.util.Collections;
import java.util.List;

/**
 * Represents a method (or constructor, which is a special type of method) in the symbol table.
 * Stores its name, return type, parameter types, and a reference to its scope.
 */
public class MethodSymbol extends Symbol
{
	private List<Type> parameterTypes;
	private final SymbolTable methodScope;
	private final boolean isStatic;
	private final boolean isConstructor;
	private final boolean isOperator; // Add this
	private final boolean isWrapper; // ADD THIS
	private final String cppTarget;  // ADD THIS
	private ClassSymbol ownerClass;
	private String mangledName;

	/**
	 * Constructor for a regular MethodSymbol.
	 *
	 * @param name             The name of the method.
	 * @param returnType       The return type of the method.
	 * @param parameterTypes   A list of types for the method's parameters.
	 * @param declarationToken The token where this method was declared.
	 * @param methodScope      The symbol table for the method's local scope.
	 * @param isStatic         True if this is a static method, false otherwise.
	 * @param isPublic         True if this method is public, false otherwise.
	 */
	public MethodSymbol(String name, Type returnType, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isStatic, boolean isPublic, boolean isWrapper, String cppTarget, boolean isOperator)
	{
		super(name, returnType, declarationToken, isPublic);
		this.parameterTypes = Collections.unmodifiableList(parameterTypes);
		this.methodScope = methodScope;
		this.isStatic = isStatic;
		this.isConstructor = false;
		this.isWrapper = isWrapper;
		this.cppTarget = cppTarget;
		this.isOperator = isOperator; // Initialize the new field
		this.mangledName = name; // Default mangled name is the original name
	}

	// Update chained constructors to pass the new `isOperator` flag (defaulting to false)
	public MethodSymbol(String name, Type returnType, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isStatic, boolean isPublic, boolean isWrapper, String cppTarget)
	{
		this(name, returnType, parameterTypes, declarationToken, methodScope, isStatic, isPublic, isWrapper, cppTarget, false);
	}

	public MethodSymbol(String name, Type returnType, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isStatic, boolean isPublic)
	{
		this(name, returnType, parameterTypes, declarationToken, methodScope, isStatic, isPublic, false, null, false);
	}

	public MethodSymbol(String name, Type returnType, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isStatic)
	{
		this(name, returnType, parameterTypes, declarationToken, methodScope, isStatic, false, false, null, false);
	}

	// Constructor for ConstructorSymbol (cannot be a wrapper)
	public MethodSymbol(String name, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isPublic)
	{
		super(name, PrimitiveType.VOID, declarationToken, isPublic);
		this.parameterTypes = Collections.unmodifiableList(parameterTypes);
		this.methodScope = methodScope;
		this.isStatic = false;
		this.isConstructor = true;
		this.isOperator = false;
		this.isWrapper = false; // Constructors can't be wrappers
		this.cppTarget = null;  // Constructors can't be wrappers
	}

	public ClassSymbol getOwnerClass()
	{
		return ownerClass;
	}

	public void setOwnerClass(ClassSymbol ownerClass)
	{
		this.ownerClass = ownerClass;
	}

	public List<Type> getParameterTypes()
	{
		return parameterTypes;
	}

	public SymbolTable getMethodScope()
	{
		return methodScope;
	}

	@Override
	public boolean isStatic()
	{
		return isStatic;
	}

	public boolean isConstructor()
	{
		return isConstructor;
	}

	public boolean isWrapper()
	{
		return isWrapper;
	}

	public String getCppTarget()
	{
		return cppTarget;
	}
	public boolean isOperator() {
		return isOperator;
	}

	public String getMangledName()
	{
		return mangledName;
	}

	public void setMangledName(String mangledName)
	{
		this.mangledName = mangledName;
	}

	/**
	 * Checks if the given actual argument types are compatible with this method's parameter types.
	 *
	 * @param actualArgTypes The list of actual argument types.
	 * @return True if the arguments match this method's signature, false otherwise.
	 */
	public boolean matchesArguments(List<Type> actualArgTypes)
	{
		if (this.parameterTypes.size() != actualArgTypes.size())
		{
			return false;
		}
		for (int i = 0; i < this.parameterTypes.size(); i++)
		{
			Type formal = this.parameterTypes.get(i);
			Type actual = actualArgTypes.get(i);
			// Check type compatibility (actual must be assignable to formal)
			if (actual instanceof ErrorType || formal instanceof ErrorType)
			{
				continue; // Don't block on error types, allow semantic analysis to continue
			}
			if (!actual.isAssignableTo(formal))
			{
				// This method needs to be implemented in your Type hierarchy
				return false;
			}
		}
		return true;
	}

	@Override
	public String toString()
	{
		String methodOrConstructor = isConstructor ? "ConstructorSymbol" : "MethodSymbol";
		return methodOrConstructor + "{" +
				"name='" + getName() + '\'' +
				", returnType=" + getType() +
				", parameterTypes=" + parameterTypes +
				", static=" + isStatic() +
				", public=" + isPublic() +
				'}';
	}

	public void setParameterTypes(List<Type> parameterTypes)
	{
		this.parameterTypes = parameterTypes;
	}
}