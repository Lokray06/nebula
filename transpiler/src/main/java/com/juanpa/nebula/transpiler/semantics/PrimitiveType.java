// File: src/main/java/com/juanpa/nebula/transpiler/semantics/PrimitiveType.java

package com.juanpa.nebula.transpiler.semantics;

/**
 * Represents primitive types in the Nebula language (e.g., int, bool, String, double).
 * Implements type compatibility rules for these basic types.
 */
public class PrimitiveType extends Type
{
	// Pre-defined singletons for common primitive types
	public static final PrimitiveType INT = new PrimitiveType("int");
	public static final PrimitiveType BOOL = new PrimitiveType("bool");
	// Note: String is treated as a class type for C# like behavior, not a primitive here.
	// This PrimitiveType.STRING instance might be a remnant or used for conceptual compatibility.
	// For "string" keyword, it will map to Nebula.Lang.String ClassSymbol.
	public static final PrimitiveType STRING = new PrimitiveType("string"); // This will be phased out for ClassType "nebula.core.String"
	public static final PrimitiveType DOUBLE = new PrimitiveType("double");
	public static final PrimitiveType FLOAT = new PrimitiveType("float");
	public static final PrimitiveType BYTE = new PrimitiveType("byte");
	public static final PrimitiveType CHAR = new PrimitiveType("char");
	public static final PrimitiveType VOID = new PrimitiveType("void"); // For method return types

	public PrimitiveType(String name)
	{
		super(name);
	}

	@Override
	public boolean isAssignableFrom(Type other)
	{
		if(this.equals(other))
		{
			return true; // Same type is always assignable
		}

		// Allow implicit numeric conversions (widening conversions)
		if(this.equals(DOUBLE) && (other.equals(INT) || other.equals(FLOAT) || other.equals(BYTE) || other.equals(CHAR)))
		{
			return true;
		}
		if(this.equals(FLOAT) && (other.equals(INT) || other.equals(BYTE) || other.equals(CHAR)))
		{
			return true;
		}
		if(this.equals(INT) && (other.equals(BYTE) || other.equals(CHAR)))
		{
			return true;
		}
		// Explicit char to byte or vice versa is not typically implicit, but for simplicity:
		// If char is 16-bit unsigned and byte is 8-bit signed, this is a narrowing conversion.
		// Assuming char and byte are both 8-bit for simplicity, or char is compatible with int.
		if(this.equals(CHAR) && other.equals(BYTE))
		{ // Byte -> Char (potential data loss if char is unsigned and byte is signed)
			return true;
		}
		if(this.equals(BYTE) && other.equals(CHAR))
		{ // Char -> Byte (definite data loss potential)
			return true;
		}

		// Null can be assigned to any ClassType or ArrayType, not PrimitiveType.
		if(other instanceof NullType)
		{
			return false;
		}

		// String is generally not implicitly convertible from other primitives directly here.
		// 'bool' is not convertible from other primitives.
		// 'void' is only for return types, not assignable.
		return false;
	}

	/**
	 * Checks if this primitive type is assignable to the target type.
	 * Implements widening conversions for primitive types (e.g., int -> double).
	 *
	 * @param targetType The type to which this type is being assigned.
	 * @return True if this primitive type can be assigned to targetType, false otherwise.
	 */
	@Override
	public boolean isAssignableTo(Type targetType)
	{
		if(super.isAssignableTo(targetType))
			return true; // Handles self-assignment and ErrorType

		if(!(targetType instanceof PrimitiveType))
			return false; // Can only assign primitives to primitives (or special cases like String from char[])

		PrimitiveType target = (PrimitiveType) targetType;

		// Widening conversions (example rules, adjust as per Nebula spec)
		if(this == INT)
		{
			return target == FLOAT || target == DOUBLE || target == BYTE; // int can widen to float, double, byte
		}
		if(this == BYTE)
		{
			return target == INT || target == FLOAT || target == DOUBLE; // byte can widen to int, float, double
		}
		if(this == FLOAT)
		{
			return target == DOUBLE; // float can widen to double
		}
		// bool, char, void usually have no implicit widening conversions to other primitives
		return false;
	}

	@Override
	public boolean isCompatibleWith(Type other)
	{
		if(this.equals(other))
		{
			return true;
		}

		// Numeric types are generally compatible for arithmetic operations
		if(this.isNumeric() && other.isNumeric())
		{
			return true;
		}

		// Boolean types are compatible only with other booleans for logical ops
		if(this.equals(BOOL) && other.equals(BOOL))
		{
			return true;
		}

		// Void is never compatible for operations
		return false;
	}

	@Override
	public boolean isNumeric()
	{
		return this.equals(INT) || this.equals(FLOAT) || this.equals(DOUBLE) || this.equals(BYTE) || this.equals(CHAR);
	}

	// This is a singleton pattern, but without preventing instantiation
	// to allow for potential future extensions where PrimitiveType might need
	// slight variations or custom creation (though not recommended for basic primitives).
	// A more strict singleton would have private constructor and public static final instances only.
}