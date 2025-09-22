// File: src/main/java/com/juanpa.nebula.transpiler/semantics/ClassType.java
package com.juanpa.nebula.compiler.semantics;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Represents a user-defined class type in the Nebula language.
 * This type stores the fully qualified name of the class and a reference to its
 * corresponding {@link ClassSymbol} which holds its members and scope.
 */
public class ClassType extends Type
{
	// Fully qualified name (e.g., "System.Console", "MyNamespace.MyClass")
	private final String fqn;
	// Reference to the ClassSymbol that defines this class.
	// This is often set in two phases: first ClassType is created, then ClassSymbol is resolved.
	public ClassSymbol classSymbol; // Public for direct access during initialization
	private Type superClassType; // Can be null (for Object or classes not extending anything implicitly)

	public ClassType(String fqn, ClassSymbol classSymbol)
	{
		super(fqn);
		this.fqn = fqn;
		this.classSymbol = classSymbol;
		this.superClassType = null; // Initially null, set during semantic analysis
	}

	public ClassType(String fqn, ClassSymbol classSymbol, Type superClassType)
	{
		super(fqn);
		this.fqn = fqn;
		this.classSymbol = classSymbol;
		this.superClassType = superClassType;
	}

	public String getFqn()
	{
		return fqn;
	}

	public ClassSymbol getClassSymbol()
	{
		return classSymbol;
	}

	// Removed setClassSymbol as classSymbol is public and directly assigned.
	// public void setClassSymbol(ClassSymbol classSymbol) {
	// 	this.classSymbol = classSymbol;
	// }

	public Type getSuperClassType()
	{
		return superClassType;
	}

	public void setSuperClassType(Type superClassType)
	{
		this.superClassType = superClassType;
	}

	/**
	 * Checks if this class type can be assigned TO the target type.
	 * Handles inheritance (subclass to superclass assignments).
	 *
	 * @param targetType The type to which this class type is being assigned.
	 * @return True if this class type can be assigned to targetType, false otherwise.
	 */
	@Override
	public boolean isAssignableTo(Type targetType)
	{
		if (super.isAssignableTo(targetType))
		{
			return true; // Handles self-assignment and ErrorType
		}

		// A ClassType instance (non-null object) cannot be assigned TO NullType.
		// Only NullType.INSTANCE can be assigned to NullType.
		if (targetType instanceof NullType)
		{
			return false;
		}

		// A ClassType can only be assigned to another ClassType or Object.
		if (!(targetType instanceof ClassType))
		{
			return false;
		}

		ClassType targetClassType = (ClassType) targetType;

		// Check if 'this' is the same class as targetType, or a subclass of targetType.
		// Traverse up the inheritance hierarchy of 'this' class.
		ClassType currentTypeInHierarchy = this;
		while (currentTypeInHierarchy != null)
		{
			if (currentTypeInHierarchy.equals(targetClassType))
			{
				return true; // Found a match in the inheritance chain
			}
			// Move to the superclass. Assuming superClassType is properly set during analysis.
			if (currentTypeInHierarchy.getSuperClassType() instanceof ClassType)
			{
				currentTypeInHierarchy = (ClassType) currentTypeInHierarchy.getSuperClassType();
			}
			else
			{
				currentTypeInHierarchy = null; // Reached the top (Object) or a non-ClassType superclass
			}
		}

		// TODO: Add interface assignment checks here if you implement interfaces.

		return false;
	}

	@Override
	public boolean isAssignableFrom(Type other)
	{
		if (this.equals(other))
		{
			return true; // Same class type is always assignable
		}

		// Allow assigning null to any class type (reference type)
		if (other instanceof NullType)
		{
			return true;
		}

		// For class types, check inheritance hierarchy
		if (other instanceof ClassType)
		{
			ClassType otherClass = (ClassType) other;
			// A full implementation would traverse the 'extends' chain of otherClass.classSymbol
			// to see if 'this' class is in its hierarchy. For now, a simplified check.
			// E.g., `this` is `Object`, `other` is `MyClass`, then `MyClass` is assignable to `Object`.
			// If `otherClass.classSymbol` exists, and it is a subclass of `this.classSymbol`.
			// This would require a `isSubclassOf` method on ClassSymbol.
			if (otherClass.classSymbol != null && this.classSymbol != null)
			{
				// Assuming `isSubclassOf` correctly traverses inheritance
				// For now, simplify to check if they are the same ClassSymbol
				// or if the `otherClass` represents a "subclass" conceptually that can be assigned.
				// This is where proper inheritance tree walking is needed.
				// For now, let's keep it simple: only exact match for user-defined classes,
				// unless `other` is `NullType`.
				// More robust: 'otherClass' must be 'this' or a subclass of 'this'.
				ClassType currentOtherInHierarchy = otherClass;
				while (currentOtherInHierarchy != null)
				{
					if (currentOtherInHierarchy.equals(this))
					{
						return true;
					}
					if (currentOtherInHierarchy.getSuperClassType() instanceof ClassType)
					{
						currentOtherInHierarchy = (ClassType) currentOtherInHierarchy.getSuperClassType();
					}
					else
					{
						currentOtherInHierarchy = null;
					}
				}
			}
			return false; // Default to false if no explicit assignability rules met
		}
		return false;
	}

	@Override
	public boolean isCompatibleWith(Type other)
	{
		if (this.equals(other))
		{
			return true; // Same class type is compatible
		}
		// Generally, class types are not "compatible" for binary operations
		// unless it's a special case like String concatenation (which is handled in SemanticAnalyzer).
		// For general arithmetic or logical ops, they are not.
		return false;
	}

	/**
	 * Looks up a method by name and number of arguments within this class.
	 * This is a simplified lookup and doesn't handle overloading or full signature matching yet.
	 *
	 * @param methodName The name of the method.
	 * @param numArgs    The number of arguments the method call has.
	 * @return The resolved MethodSymbol, or null if not found.
	 */
	public MethodSymbol lookupMethod(String methodName, int numArgs)
	{
		if (this.classSymbol == null || this.classSymbol.getClassScope() == null)
		{
			return null;
		}
		// Iterate through all symbols in the class scope to find a matching method.
		for (Symbol symbol : this.classSymbol.getClassScope().symbols.values())
		{
			if (symbol instanceof MethodSymbol)
			{
				MethodSymbol method = (MethodSymbol) symbol;
				// Check name and parameter count
				if (method.getName().equals(methodName) && method.getParameterTypes().size() == numArgs)
				{
					// In a real compiler, you would also check parameter types for exact match (overloading)
					return method;
				}
			}
		}
		return null;
	}

	/**
	 * Looks up a constructor by argument types within this class.
	 * This is a simplified lookup.
	 *
	 * @param argTypes The list of argument types provided in the 'new' expression.
	 * @return The resolved MethodSymbol (representing a constructor), or null if no matching constructor found.
	 */
	public MethodSymbol lookupConstructor(List<Type> argTypes)
	{
		if (this.classSymbol == null || this.classSymbol.getClassScope() == null)
		{
			return null;
		}

		List<MethodSymbol> constructors = this.classSymbol.methodsByName.values().stream() // Corrected: use methodsByName
				.flatMap(List::stream)
				.filter(s -> s.isConstructor() && s.getName().equals(this.getName())) // Ensure it's a constructor with matching name
				.collect(Collectors.toList());

		for (MethodSymbol constructor : constructors)
		{
			if (constructor.matchesArguments(argTypes)) // Use the matchesArguments helper on MethodSymbol
			{
				return constructor;
			}
		}
		return null; // No matching constructor
	}


	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		// Specifically allow ClassType to equal PrimitiveType.STRING if their FQNs match
		// This handles cases where 'string' keyword maps to Nebula.Lang.String class.
		if (o instanceof PrimitiveType && ((PrimitiveType) o).getName().equals("String") && this.getFqn().equals("nebula.core.String"))
		{
			return true;
		}
		if (o == null || getClass() != o.getClass())
		{
			return false;
		}
		ClassType classType = (ClassType) o;
		return fqn.equals(classType.fqn); // Equality based on fully qualified name
	}

	@Override
	public int hashCode()
	{
		return fqn.hashCode();
	}
}
