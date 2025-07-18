// File: src/main/java/com/juanpa.nebula.transpiler/semantics/SymbolTable.java

package com.juanpa.nebula.transpiler.semantics;

import com.juanpa.nebula.transpiler.lexer.Token;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a symbol table for a specific scope in the Nebula language.
 * It maps identifier names to their corresponding Symbol objects.
 * Symbol tables form a tree structure, allowing for hierarchical scope management.
 */
public class SymbolTable
{
	final Map<String, Symbol> symbols;
	private final SymbolTable enclosingScope; // Reference to the parent scope
	private final String scopeName; // For debugging/identification (e.g., "global", "method:main", "block:if")
	private Type currentScopeType; // Added to track the Type of the scope (e.g., ClassType for class scope)

	public SymbolTable(SymbolTable enclosingScope, String scopeName)
	{
		this.symbols = new HashMap<>();
		this.enclosingScope = enclosingScope;
		this.scopeName = scopeName;
	}

	/**
	 * Defines a new symbol in the current scope.
	 *
	 * @param symbol The symbol to define.
	 * @throws IllegalArgumentException if a symbol with the same name already exists in this scope.
	 */
	public void define(Symbol symbol)
	{
		if(symbols.containsKey(symbol.getName()))
		{
			// This check should ideally be handled by a specific error in SemanticAnalyzer,
			// but for now, a runtime exception indicates a semantic error during definition.
			throw new IllegalArgumentException("Symbol '" + symbol.getName() + "' already defined in scope '" + scopeName + "'.");
		}
		symbols.put(symbol.getName(), symbol);
	}

	/**
	 * Looks up a symbol, starting from the current scope and moving up to enclosing scopes.
	 *
	 * @param name The name of the symbol to look up.
	 * @return The found Symbol, or null if not found in any enclosing scope.
	 */
	public Symbol resolve(String name)
	{
		Symbol symbol = symbols.get(name);
		if(symbol != null)
		{
			return symbol;
		}
		// If not found in current scope, try to resolve in the enclosing scope
		if(enclosingScope != null)
		{
			return enclosingScope.resolve(name);
		}
		return null; // Symbol not found
	}

	/**
	 * Looks up a symbol only in the current scope.
	 *
	 * @param name The name of the symbol to look up.
	 * @return The found Symbol, or null if not found in this scope.
	 */
	public Symbol resolveCurrentScope(String name)
	{
		return symbols.get(name);
	}

	public SymbolTable getEnclosingScope()
	{
		return enclosingScope;
	}

	public String getScopeName()
	{
		return scopeName;
	}

	// Getter and Setter for currentScopeType
	public Type getCurrentScopeType()
	{
		return currentScopeType;
	}

	public void setCurrentScopeType(Type currentScopeType)
	{
		this.currentScopeType = currentScopeType;
	}

	@Override
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("Scope '").append(scopeName).append("':\n");
		for(Map.Entry<String, Symbol> entry : symbols.entrySet())
		{
			sb.append("  ").append(entry.getValue()).append("\n");
		}
		return sb.toString();
	}
}