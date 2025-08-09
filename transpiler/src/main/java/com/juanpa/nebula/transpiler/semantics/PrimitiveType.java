// File: src/main/java/com/juanpa/nebula/transpiler/semantics/PrimitiveType.java

package com.juanpa.nebula.transpiler.semantics;

/**
 * Represents primitive types in the Nebula language (e.g., int, bool, String, double).
 * Implements type compatibility rules for these basic types.
 */
public class PrimitiveType extends Type
{
	// Pre-defined singletons for common primitive types
	public static final PrimitiveType VOID = new PrimitiveType("void");
	public static final PrimitiveType BOOL = new PrimitiveType("bool");
	public static final PrimitiveType CHAR = new PrimitiveType("char");
	public static final PrimitiveType CHAR16 = new PrimitiveType("char16");
	public static final PrimitiveType CHAR32 = new PrimitiveType("char32");
	public static final PrimitiveType INT8 = new PrimitiveType("int8");
	public static final PrimitiveType INT16 = new PrimitiveType("int16");
	public static final PrimitiveType INT32 = new PrimitiveType("int32");
	public static final PrimitiveType INT64 = new PrimitiveType("int64");
	public static final PrimitiveType UINT8 = new PrimitiveType("uint8");
	public static final PrimitiveType UINT16 = new PrimitiveType("uint16");
	public static final PrimitiveType UINT32 = new PrimitiveType("uint32");
	public static final PrimitiveType UINT64 = new PrimitiveType("uint64");
	public static final PrimitiveType FLOAT = new PrimitiveType("float");
	public static final PrimitiveType DOUBLE = new PrimitiveType("double");

	// Aliases
	public static final PrimitiveType BYTE = INT8;
	public static final PrimitiveType SHORT = INT16;
	public static final PrimitiveType INT = INT32;
	public static final PrimitiveType LONG = INT64;
	public static final PrimitiveType UBYTE = UINT8;
	public static final PrimitiveType USHORT = UINT16;
	public static final PrimitiveType UINT = UINT32;
	public static final PrimitiveType ULONG = UINT64;

	public static final PrimitiveType STRING = new PrimitiveType("string");

	public PrimitiveType(String name)
	{
		super(name);
	}

	@Override
	public boolean isReferenceType()
	{
		// The 'null' literal is also considered a reference type for assignment compatibility
		return this.equals(STRING);
	}

	@Override
	public boolean isAssignableFrom(Type other)
	{
		// Use the base class logic to handle the null literal and self-assignment.
		if (super.isAssignableFrom(other))
		{
			return true;
		}

		if (!(other instanceof PrimitiveType))
		{
			return false;
		}

		// --- The rest of your existing widening conversion rules ---

		// A DOUBLE can be assigned from any other numeric type
		if (this.equals(DOUBLE))
		{
			return other.isNumeric() || other.equals(FLOAT);
		}

		// A FLOAT can be assigned from any integer or char type
		if (this.equals(FLOAT))
		{
			return other.isNumeric() && !(other.equals(DOUBLE)); // All numerics except double
		}

		// An INT64 can be assigned from any smaller integer or char type
		if (this.equals(INT64) || this.equals(LONG))
		{
			return other.equals(INT32) || other.equals(INT) ||
					other.equals(INT16) || other.equals(SHORT) ||
					other.equals(INT8) || other.equals(BYTE) ||
					other.equals(UINT32) || other.equals(UINT) ||
					other.equals(UINT16) || other.equals(USHORT) ||
					other.equals(UINT8) || other.equals(UBYTE) ||
					other.equals(CHAR32) || other.equals(CHAR16) || other.equals(CHAR);
		}

		// An INT32 can be assigned from smaller signed/unsigned integers and chars
		if (this.equals(INT32) || this.equals(INT))
		{
			return other.equals(INT16) || other.equals(SHORT) ||
					other.equals(INT8) || other.equals(BYTE) ||
					other.equals(UINT16) || other.equals(USHORT) ||
					other.equals(UINT8) || other.equals(UBYTE) ||
					other.equals(CHAR16) || other.equals(CHAR);
		}

		if (this.equals(INT16) || this.equals(SHORT))
		{
			return other.equals(INT8) || other.equals(BYTE) ||
					other.equals(UINT8) || other.equals(UBYTE) ||
					other.equals(CHAR);
		}

		// Unsigned integer widening
		if (this.equals(UINT64) || this.equals(ULONG))
		{
			return other.equals(UINT32) || other.equals(UINT) ||
					other.equals(UINT16) || other.equals(USHORT) ||
					other.equals(UINT8) || other.equals(UBYTE) ||
					other.equals(CHAR32) || other.equals(CHAR16) || other.equals(CHAR);
		}

		if (this.equals(UINT32) || this.equals(UINT))
		{
			return other.equals(UINT16) || other.equals(USHORT) ||
					other.equals(UINT8) || other.equals(UBYTE) ||
					other.equals(CHAR16) || other.equals(CHAR);
		}

		if (this.equals(UINT16) || this.equals(USHORT))
		{
			return other.equals(UINT8) || other.equals(UBYTE) ||
					other.equals(CHAR);
		}

		// Char widening
		if (this.equals(CHAR32))
		{
			return other.equals(CHAR16) || other.equals(CHAR);
		}
		if (this.equals(CHAR16))
		{
			return other.equals(CHAR);
		}

		return false; // No other implicit conversions are allowed
	}

	@Override
	public boolean isCompatibleWith(Type other)
	{
		// A type is always compatible with itself
		if (this.equals(other))
		{
			return true;
		}

		// Only allow compatibility with other primitive types
		if (!(other instanceof PrimitiveType))
		{
			return false;
		}

		// Check if this type can be implicitly converted to the other type
		return this.isAssignableTo(other);
	}

	@Override
	public boolean isAssignableTo(Type targetType)
	{
		if (super.isAssignableTo(targetType))
		{
			return true;
		}

		// Handle widening conversions
		if (targetType instanceof PrimitiveType)
		{
			return targetType.isAssignableFrom(this);
		}

		return false;
	}

	@Override
	public boolean isNumeric()
	{
		return this.equals(INT8) || this.equals(INT16) || this.equals(INT32) || this.equals(INT64)
				|| this.equals(UINT8) || this.equals(UINT16) || this.equals(UINT32) || this.equals(UINT64)
				|| this.equals(FLOAT) || this.equals(DOUBLE)
				|| this.equals(CHAR) || this.equals(CHAR16) || this.equals(CHAR32);
	}
}