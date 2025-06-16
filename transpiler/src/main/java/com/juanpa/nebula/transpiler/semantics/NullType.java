// File: src/main/java/com/juanpa.nebula.transpiler/semantics/NullType.java
package com.juanpa.nebula.transpiler.semantics;

/**
 * Represents the type of the `null` literal.
 * `null` can be assigned to any reference type (ClassType, ArrayType).
 */
public class NullType extends Type
{
	public static final NullType INSTANCE = new NullType();

	private NullType()
	{
		super("null");
	}

	/**
	 * Checks if this `NullType` is assignable TO the target type.
	 * `null` can be assigned to any ClassType or ArrayType.
	 *
	 * @param targetType The type to which `NullType.INSTANCE` is being assigned.
	 * @return True if `NullType` can be assigned to targetType, false otherwise.
	 */
	@Override
	public boolean isAssignableTo(Type targetType)
	{
		if(super.isAssignableTo(targetType))
			return true; // Handles self-assignment and ErrorType

		// Null can be assigned to any ClassType or ArrayType (reference types)
		return targetType instanceof ClassType || targetType instanceof ArrayType;
	}

	@Override
	public boolean isAssignableFrom(Type other)
	{
		// Null can be assigned to any reference type (ClassType, ArrayType).
		// It cannot be assigned FROM other types, only TO them.
		// This method defines what can be assigned *to* THIS type.
		// So, this method should return false because nothing is assignable FROM NullType,
		// except NullType itself. The check for "assigning null to X" should be
		// `X.isAssignableFrom(NullType.INSTANCE)`.
		return false;
	}

	@Override
	public boolean isCompatibleWith(Type other)
	{
		// Null is not compatible with other types for most operations directly.
		// Its compatibility is defined by assignability to reference types.
		return false;
	}

	@Override
	public boolean equals(Object o)
	{
		return this == o; // Singleton equality
	}

	@Override
	public int hashCode()
	{
		return super.hashCode(); // Consistent with singleton
	}
}