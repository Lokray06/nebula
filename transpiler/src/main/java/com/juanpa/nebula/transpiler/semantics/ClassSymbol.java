// File: src/main/java/com/juanpa/nebula/transpiler/semantics/ClassSymbol.java
package com.juanpa.nebula.transpiler.semantics;

import com.juanpa.nebula.transpiler.lexer.Token;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ClassSymbol extends Symbol
{
	private ClassType type;
	private final Token declarationToken;
	private final SymbolTable classScope; // For fields and nested classes/types

	// NEW: Dedicated map to store methods by name, where each name maps to a list of MethodSymbols (for overloading)
	public final Map<String, List<MethodSymbol>> methodsByName;


	public ClassSymbol(String name, ClassType type, Token declarationToken, SymbolTable classScope)
	{
		super(name, type, declarationToken, true);
		this.type = type;
		this.declarationToken = declarationToken;
		this.classScope = classScope;
		this.methodsByName = new HashMap<>(); // Initialize
	}

	public ClassType getType()
	{
		return type;
	}

	public void setType(ClassType type)
	{
		this.type = type;
	}

	public Token getDeclarationToken()
	{
		return declarationToken;
	}

	public SymbolTable getClassScope()
	{
		return classScope;
	}


	/**
	 * Defines a method or constructor in this class's scope.
	 * This handles method overloading by storing multiple methods with the same name.
	 *
	 * @param methodSymbol The method or constructor symbol to define.
	 */
	public void defineMethod(MethodSymbol methodSymbol)
	{
		methodsByName.computeIfAbsent(methodSymbol.getName(), k -> new ArrayList<>()).add(methodSymbol);
		// Note: We are NOT adding methods/constructors to `classScope.symbols` directly anymore
		// to avoid the `IllegalArgumentException` for overloaded methods.
		// Fields will still be defined in `classScope.symbols`.
	}

	/**
	 * Resolves a member (field, method, constructor) within this class's scope, including overloading.
	 * This method will be used by SemanticAnalyzer to look up members.
	 *
	 * @param memberName    The name of the member to resolve.
	 * @param argumentTypes The types of arguments (null for fields, empty list for no-arg methods).
	 *                      This is crucial for method/constructor overloading resolution.
	 * @return The resolved Symbol (VariableSymbol, MethodSymbol), or null if not found.
	 */
	public Symbol resolveMember(String memberName, List<Type> argumentTypes)
	{
		// 1. If argumentTypes is null, primarily look for a field.
		//    If argumentTypes is not null, primarily look for a method.

		// Attempt to resolve as a field first (fields are stored in classScope's direct map)
		// A field is identified by name, NOT by argument types.
		Symbol field = classScope.resolveCurrentScope(memberName);
		if(field instanceof VariableSymbol)
		{
			// If this call is specifically looking for a field (argumentTypes == null)
			// OR if it's a method call but no matching method is found later, it might default to the field.
			// However, for strictness, if arguments are provided, it's *expected* to be a method.
			if(argumentTypes == null)
			{ // This is how `visitDotExpression` should call it for fields
				return field;
			}
		}

		// Try to resolve as a method/constructor (from the dedicated methodsByName map)
		List<MethodSymbol> candidates = methodsByName.get(memberName);
		if(candidates != null && argumentTypes != null) // Only attempt method resolution if argumentTypes are provided
		{
			MethodSymbol bestMatch = null;
			for(MethodSymbol candidate : candidates)
			{
				if(candidate.matchesArguments(argumentTypes)) // Use the matchesArguments helper
				{
					bestMatch = candidate;
					// For simplicity, we take the first match. For full specificity, need to rank overloads.
					break;
				}
			}
			if(bestMatch != null)
			{
				return bestMatch;
			}
		}

		// 3. (Optional) Check inherited members if inheritance is implemented.
		// If currentClass has a superClassType, resolve member in superclass.
		if(type.getSuperClassType() instanceof ClassType)
		{
			ClassSymbol superClassSymbol = ((ClassType) type.getSuperClassType()).getClassSymbol();
			if(superClassSymbol != null)
			{
				return superClassSymbol.resolveMember(memberName, argumentTypes); // Recursive call
			}
		}

		return null; // Member not found
	}

	// Overload for cases where no arguments are involved (e.g., field access).
// This should explicitly look for a field.
	public Symbol resolveMember(String memberName)
	{
		return resolveMember(memberName, null); // Call the main method indicating 'no arguments' (i.e., look for field)
	}


	@Override
	public boolean isStatic()
	{
		return true; // Classes themselves are considered static entities
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder("ClassSymbol: " + getName() + " -> Type: " + (type != null ? type.getName() : "null") + " (Scope: " + classScope.getScopeName() + ")\n");
		sb.append("  Fields:\n");
		classScope.symbols.values().stream()
				.filter(s -> s instanceof VariableSymbol)
				.forEach(s -> sb.append("    - ").append(s).append("\n"));
		sb.append("  Methods (Overloaded):\n");
		methodsByName.values().stream()
				.flatMap(List::stream) // Flatten the list of lists of methods
				.forEach(m -> sb.append("    - ").append(m).append("\n"));
		return sb.toString();
	}

	public String getFqn()
	{
		return type.getFqn();
	}
}