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
	private final List<Type> parameterTypes;
	private final SymbolTable methodScope; // The scope associated with this method's body
	private final boolean isStatic; // Indicates if the method is static
	private final boolean isConstructor; // Flag to explicitly mark if this symbol represents a constructor
	private ClassSymbol ownerClass; // Reference to the ClassSymbol this method belongs to


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
	public MethodSymbol(String name, Type returnType, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isStatic, boolean isPublic)
	{
		super(name, returnType, declarationToken, isPublic);
		this.parameterTypes = Collections.unmodifiableList(parameterTypes);
		this.methodScope = methodScope;
		this.isStatic = isStatic;
		this.isConstructor = false; // Regular methods are not constructors
	}

	/**
	 * Constructor for a MethodSymbol (defaults to non-public if not specified).
	 *
	 * @param name             The name of the method.
	 * @param returnType       The return type of the method.
	 * @param parameterTypes   A list of types for the method's parameters.
	 * @param declarationToken The token where this method was declared.
	 * @param methodScope      The symbol table for the method's local scope.
	 * @param isStatic         True if this is a static method, false otherwise.
	 */
	public MethodSymbol(String name, Type returnType, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isStatic)
	{
		this(name, returnType, parameterTypes, declarationToken, methodScope, isStatic, false);
	}

	/**
	 * Constructor for a ConstructorSymbol (special type of method without explicit return type).
	 * Assumed to have a return type of VOID for internal representation.
	 * Constructors are typically non-static. Their public status is passed explicitly.
	 *
	 * @param name             The name of the constructor (same as class name).
	 * @param parameterTypes   A list of types for the constructor's parameters.
	 * @param declarationToken The token where this constructor was declared.
	 * @param methodScope      The symbol table for the constructor's local scope.
	 * @param isPublic         True if this constructor is public, false otherwise.
	 */
	public MethodSymbol(String name, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isPublic)
	{
		// Constructors are non-static, implicitly return VOID, and are explicitly marked as constructors
		super(name, PrimitiveType.VOID, declarationToken, isPublic);
		this.parameterTypes = Collections.unmodifiableList(parameterTypes);
		this.methodScope = methodScope;
		this.isStatic = false; // Constructors are never static
		this.isConstructor = true; // NEW: Mark as constructor
	}

	/**
	 * Constructor for a ConstructorSymbol (special type of method without explicit return type),
	 * defaults to public.
	 */
	public MethodSymbol(String name, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope)
	{
		this(name, parameterTypes, declarationToken, methodScope, true); // Default to public
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

	/**
	 * Checks if the given actual argument types are compatible with this method's parameter types.
	 *
	 * @param actualArgTypes The list of actual argument types.
	 * @return True if the arguments match this method's signature, false otherwise.
	 */
	public boolean matchesArguments(List<Type> actualArgTypes)
	{
		if(this.parameterTypes.size() != actualArgTypes.size())
		{
			return false;
		}
		for(int i = 0; i < this.parameterTypes.size(); i++)
		{
			Type formal = this.parameterTypes.get(i);
			Type actual = actualArgTypes.get(i);
			// Check type compatibility (actual must be assignable to formal)
			if(actual instanceof ErrorType || formal instanceof ErrorType)
			{
				continue; // Don't block on error types, allow semantic analysis to continue
			}
			if(!actual.isAssignableTo(formal))
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
}