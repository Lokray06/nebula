// File: src/main/java/com/juanpa/nebula/transpiler/semantics/Type.java

package com.juanpa.nebula.transpiler.semantics;

/**
 * Abstract base class for all types in the Nebula language.
 * This includes primitive types (int, bool, String, etc.) and user-defined class types.
 */
public abstract class Type
{
	protected String name;

	public Type(String name)
	{
		this.name = name;
	}

	public String getName()
	{
		return name;
	}

	/**
	 * Checks if this type is compatible with another type for binary operations.
	 * This method should be overridden by concrete type implementations.
	 *
	 * @param other The other type to check for binary operation compatibility.
	 * @return True if compatible, false otherwise.
	 */
	public abstract boolean isCompatibleWith(Type other);

	/**
	 * Checks if this type is a numeric type.
	 *
	 * @return True if numeric, false otherwise.
	 */
	public boolean isNumeric()
	{
		// Default implementation: most types are not numeric unless specified.
		// PrimitiveType will override this.
		return false;
	}

	/**
	 * Returns the wider of two numeric types.
	 * This should ideally be a static helper or a method on a numeric type utility.
	 * Placed here for now to satisfy `SemanticAnalyzer`'s `Type.getWiderNumericType` calls.
	 *
	 * @param type1 The first type.
	 * @param type2 The second type.
	 * @return The wider numeric type, or ErrorType.INSTANCE if not compatible.
	 */
	public static Type getWiderNumericType(Type type1, Type type2)
	{
		if (!type1.isNumeric() || !type2.isNumeric())
		{
			return ErrorType.INSTANCE; // Not numeric, or one is not numeric
		}

		// Simple hierarchy: byte -> char -> int -> float -> double
		// For more complex hierarchy, you might use an enum for type ranks.

		if (type1.equals(PrimitiveType.DOUBLE) || type2.equals(PrimitiveType.DOUBLE))
		{
			return PrimitiveType.DOUBLE;
		}
		if (type1.equals(PrimitiveType.FLOAT) || type2.equals(PrimitiveType.FLOAT))
		{
			return PrimitiveType.FLOAT;
		}
		if (type1.equals(PrimitiveType.INT) || type2.equals(PrimitiveType.INT))
		{
			return PrimitiveType.INT;
		}
		if (type1.equals(PrimitiveType.CHAR) || type2.equals(PrimitiveType.CHAR))
		{
			return PrimitiveType.CHAR;
		}
		if (type1.equals(PrimitiveType.BYTE) || type2.equals(PrimitiveType.BYTE))
		{
			return PrimitiveType.BYTE;
		}

		// Should not be reached if both are numeric and handled above
		return ErrorType.INSTANCE;
	}

	/**
	 * Checks if two types are comparable (e.g., for ==, != operators).
	 * This should ideally be a static helper.
	 *
	 * @param type1 The first type.
	 * @param type2 The second type.
	 * @return True if comparable, false otherwise.
	 */
	public static boolean isComparable(Type type1, Type type2)
	{
		if (type1.equals(type2))
		{
			return true; // Same type is always comparable
		}

		// Null is comparable with any reference type
		if (type1 instanceof NullType && (type2 instanceof ClassType || type2 instanceof ArrayType))
		{
			return true;
		}
		if (type2 instanceof NullType && (type1 instanceof ClassType || type1 instanceof ArrayType))
		{
			return true;
		}

		// Numeric types are comparable with other numeric types
		if (type1.isNumeric() && type2.isNumeric())
		{
			return true;
		}

		// Boolean types are comparable only with other boolean types
		if (type1.equals(PrimitiveType.BOOL) && type2.equals(PrimitiveType.BOOL))
		{
			return true;
		}

		// Class types can be compared if they are in the same inheritance hierarchy (simplified here)
		// For a full implementation, you'd check if one is assignable from the other.
		if (type1 instanceof ClassType && type2 instanceof ClassType)
		{
			return type1.isAssignableFrom(type2) || type2.isAssignableFrom(type1);
		}

		return false;
	}

	/**
	 * Checks if this type (the type of `this` object) is assignable to the target type.
	 * This method defines the type compatibility rules of the language.
	 *
	 * @param targetType The type to which this type is being assigned.
	 * @return True if this type can be assigned to targetType, false otherwise.
	 */
	public boolean isAssignableTo(Type targetType)
	{
		// A type is always assignable to itself.
		if (this.equals(targetType))
		{
			return true;
		}
		// Error types can be assigned to anything (or nothing) to prevent cascades.
		if (this instanceof ErrorType || targetType instanceof ErrorType)
		{
			return true; // Allows semantic analysis to continue
		}
		return false;
	}

	/**
	 * Checks if this type is a reference type (i.e., can hold a null value).
	 * Non-primitive types are reference types by default.
	 *
	 * @return true if this type is a reference type, false otherwise.
	 */
	public boolean isReferenceType()
	{
		// Any type that is not an instance of PrimitiveType is a reference type.
		return !(this instanceof PrimitiveType);
	}

	/**
	 * Checks if this type is assignable from another type.
	 * This is the primary method for type compatibility checks.
	 *
	 * @param other The type to check for assignability.
	 * @return True if 'other' can be assigned to this type, false otherwise.
	 */
	public boolean isAssignableFrom(Type other)
	{
		// A type is always assignable from itself.
		if (this.equals(other))
		{
			return true;
		}

		// The null literal can be assigned to any reference type.
		if (other instanceof NullType)
		{
			return this.isReferenceType();
		}

		return false;
	}

	@Override
	public String toString()
	{
		return name;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (o == null || getClass() != o.getClass())
		{
			return false;
		}
		Type type = (Type) o;
		return name.equals(type.name);
	}

	@Override
	public int hashCode()
	{
		return name.hashCode();
	}
}