// File: src/main/java/com/juanpa.nebula.transpiler/semantics/ArrayType.java
package com.juanpa.nebula.compiler.semantics;

/**
 * Represents an array type in the Nebula language.
 * An array type is defined by its element type (e.g., int[], String[][]).
 */
public class ArrayType extends Type
{
	private final Type elementType;
	// Potentially add dimensions if multi-dimensional arrays are distinct types (e.g., int[][])
	// For simplicity, this assumes single-dimensional arrays or that multi-dimensional
	// arrays are represented as arrays of arrays.

	public ArrayType(Type elementType)
	{
		super(elementType.getName() + "[]"); // Name like "int[]"
		this.elementType = elementType;
	}

	public Type getElementType()
	{
		return elementType;
	}

	/**
	 * Checks if this array type can be assigned TO the target type.
	 * Handles assignability for arrays (e.g., subclass array to superclass array).
	 *
	 * @param targetType The type to which this array type is being assigned.
	 * @return True if this array type can be assigned to targetType, false otherwise.
	 */
	@Override
	public boolean isAssignableTo(Type targetType)
	{
		if(super.isAssignableTo(targetType))
			return true; // Handles self-assignment and ErrorType

		// An ArrayType instance (non-null array) cannot be assigned TO NullType.
		// Only NullType.INSTANCE can be assigned to NullType.
		if(targetType instanceof NullType)
		{
			return false;
		}

		// An array type can only be assigned to another array type.
		if(!(targetType instanceof ArrayType))
		{
			return false;
		}

		ArrayType otherArrayType = (ArrayType) targetType;

		// Covariance: If T' is a subtype of T, then T'[] is assignable to T[].
		// This means `this.elementType` must be assignable to `otherArrayType.elementType`.
		// E.g., `String[]` is assignable to `Object[]` if `String` is assignable to `Object`.
		return this.elementType.isAssignableTo(otherArrayType.elementType);
	}

	@Override
	public boolean isAssignableFrom(Type other)
	{
		if(this.equals(other))
		{
			return true; // Same array type is always assignable
		}
		// Allow assigning null to any array type
		if (other instanceof NullType)
		{
			return true;
		}
		// Allow assigning an array of a more specific type to a more general type (covariance, if supported)
		// For simplicity, require exact match for now, or allow if element types are assignable.
		if(other instanceof ArrayType)
		{
			ArrayType otherArrayType = (ArrayType) other;
			// Covariance: A String[] is assignable to an Object[] (if Object is base type)
			// For now, require exact element type match or allow assignability of element types.
			return this.elementType.isAssignableFrom(otherArrayType.elementType);
		}
		return false;
	}

	@Override
	public boolean isCompatibleWith(Type other)
	{
		if(this.equals(other))
		{
			return true; // Same array type is compatible
		}
		// Arrays are generally not compatible with other types for binary operations,
		// unless specific language features like array concatenation are introduced.
		// For now, only compatible with other ArrayTypes of compatible element types.
		if(other instanceof ArrayType)
		{
			ArrayType otherArrayType = (ArrayType) other;
			return this.elementType.isCompatibleWith(otherArrayType.elementType);
		}
		return false;
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
			return true;
		if(o == null || getClass() != o.getClass())
			return false;
		// Do not call super.equals(o) directly if Type's equals method checks getClass()
		// Instead, directly check the name field inherited from Type.
		// If super.equals() checks getClass(), it would return false if the other is not exactly Type.
		// Type's equals: `if (o == null || getClass() != o.getClass()) return false;`
		// This means we CANNOT call super.equals() if `o` might be a subclass.
		// We need to re-implement the name check here directly.
		// The `name` field is protected, so `((Type)o).name` is accessible.
		if(!this.name.equals(((Type) o).name))
			return false;

		ArrayType arrayType = (ArrayType) o;
		return elementType.equals(arrayType.elementType);
	}

	@Override
	public int hashCode()
	{
		return super.hashCode() + elementType.hashCode();
	}
}
