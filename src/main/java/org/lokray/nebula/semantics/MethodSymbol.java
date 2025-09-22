package org.lokray.nebula.semantics;

import org.lokray.nebula.lexer.Token;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Represents a method (or constructor) in the symbol table.
 */
public class MethodSymbol extends Symbol
{
	private List<Type> parameterTypes;
	private final SymbolTable methodScope;
	private final boolean isStatic;
	private final boolean isConstructor;
	private final boolean isOperator;
	private final boolean isWrapper;
	private final String cppTarget;
	private ClassSymbol ownerClass;
	private String mangledName;

	// --- NEW ---
	// Holds ownership information for the return value and each parameter.
	private OwnershipKind returnOwnership = OwnershipKind.SHARED;
	private List<OwnershipKind> parameterOwnerships = new ArrayList<>();


	public MethodSymbol(String name, Type returnType, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isStatic, boolean isPublic, boolean isWrapper, String cppTarget, boolean isOperator)
	{
		super(name, returnType, declarationToken, isPublic);
		this.parameterTypes = Collections.unmodifiableList(parameterTypes);
		this.methodScope = methodScope;
		this.isStatic = isStatic;
		this.isConstructor = false;
		this.isWrapper = isWrapper;
		this.cppTarget = cppTarget;
		this.isOperator = isOperator;
		this.mangledName = name;
		// Initialize parameter ownerships to default (SHARED)
		for (int i = 0; i < parameterTypes.size(); i++)
		{
			this.parameterOwnerships.add(OwnershipKind.SHARED);
		}
	}

	// Constructor chains updated to call the main constructor
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

	// Constructor for ConstructorSymbol
	public MethodSymbol(String name, List<Type> parameterTypes, Token declarationToken, SymbolTable methodScope, boolean isPublic)
	{
		super(name, PrimitiveType.VOID, declarationToken, isPublic);
		this.parameterTypes = Collections.unmodifiableList(parameterTypes);
		this.methodScope = methodScope;
		this.isStatic = false;
		this.isConstructor = true;
		this.isOperator = false;
		this.isWrapper = false;
		this.cppTarget = null;
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

	public boolean isOperator()
	{
		return isOperator;
	}

	public String getMangledName()
	{
		// If a specific mangled name exists, return it.
		if (this.mangledName != null)
		{
			return this.mangledName;
		}
		// Otherwise, always fall back to the simple name. This prevents returning null.
		return this.getName();
	}

	public void setMangledName(String mangledName)
	{
		this.mangledName = mangledName;
	}

	public void setParameterTypes(List<Type> parameterTypes)
	{
		this.parameterTypes = parameterTypes;
	}

	// --- NEW: Getters and Setters for Ownership Info ---

	public OwnershipKind getReturnOwnership()
	{
		return returnOwnership;
	}

	public void setReturnOwnership(OwnershipKind returnOwnership)
	{
		this.returnOwnership = returnOwnership;
	}

	public List<OwnershipKind> getParameterOwnerships()
	{
		return Collections.unmodifiableList(parameterOwnerships);
	}

	public void setParameterOwnerships(List<OwnershipKind> parameterOwnerships)
	{
		this.parameterOwnerships = new ArrayList<>(parameterOwnerships);
	}


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
				continue;
			}
			if (!actual.isAssignableTo(formal))
			{
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