// File: src/main/java/com/juanpa/nebula/transpiler/semantics/PrimitiveType.java

package org.lokray.nebula.semantics;

// Add these imports

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Represents primitive types in the Nebula language (e.g., int, bool, String, double).
 * Implements type compatibility rules for these basic types.
 */
public class PrimitiveType extends Type
{
	// NEW: A map to hold static properties like 'max' and 'min'.
	// This is initially mutable so it can be populated by our static initializer.
	private Map<String, Symbol> staticProperties = new HashMap<>();

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

	// NEW: A central place to initialize all static properties for primitives.
	// This avoids circular dependency issues during static instantiation.
	private static boolean staticPropertiesInitialized = false;

	// In PrimitiveType.java
	public static void initializeStaticProperties()
	{
		if (staticPropertiesInitialized)
		{
			return;
		}

		// --- INT8 / BYTE ---
		Map<String, Symbol> int8Props = new HashMap<>();
		// The key change is replacing the C++ string with the actual Java constant value.
		int8Props.put("max", new VariableSymbol("max", INT8, null, true, true, true, true, false, null, Byte.MAX_VALUE));
		int8Props.put("min", new VariableSymbol("min", INT8, null, true, true, true, true, false, null, Byte.MIN_VALUE));
		INT8.staticProperties = Collections.unmodifiableMap(int8Props);

		// --- INT16 / SHORT ---
		Map<String, Symbol> int16Props = new HashMap<>();
		int16Props.put("max", new VariableSymbol("max", INT16, null, true, true, true, true, false, null, Short.MAX_VALUE));
		int16Props.put("min", new VariableSymbol("min", INT16, null, true, true, true, true, false, null, Short.MIN_VALUE));
		INT16.staticProperties = Collections.unmodifiableMap(int16Props);

		// --- INT32 / INT ---
		Map<String, Symbol> int32Props = new HashMap<>();
		int32Props.put("max", new VariableSymbol("max", INT32, null, true, true, true, true, false, null, Integer.MAX_VALUE));
		int32Props.put("min", new VariableSymbol("min", INT32, null, true, true, true, true, false, null, Integer.MIN_VALUE));
		INT32.staticProperties = Collections.unmodifiableMap(int32Props);

		// --- INT64 / LONG ---
		Map<String, Symbol> int64Props = new HashMap<>();
		int64Props.put("max", new VariableSymbol("max", INT64, null, true, true, true, true, false, null, Long.MAX_VALUE));
		int64Props.put("min", new VariableSymbol("min", INT64, null, true, true, true, true, false, null, Long.MIN_VALUE));
		INT64.staticProperties = Collections.unmodifiableMap(int64Props);

		// --- UINT8 / UBYTE ---
		Map<String, Symbol> uint8Props = new HashMap<>();
		uint8Props.put("max", new VariableSymbol("max", UINT8, null, true, true, true, true, false, null, 255));
		uint8Props.put("min", new VariableSymbol("min", UINT8, null, true, true, true, true, false, null, 0));
		UINT8.staticProperties = Collections.unmodifiableMap(uint8Props);

		// --- UINT16 / USHORT ---
		Map<String, Symbol> uint16Props = new HashMap<>();
		uint16Props.put("max", new VariableSymbol("max", UINT16, null, true, true, true, true, false, null, 65535));
		uint16Props.put("min", new VariableSymbol("min", UINT16, null, true, true, true, true, false, null, 0));
		UINT16.staticProperties = Collections.unmodifiableMap(uint16Props);

		// --- UINT32 / UINT ---
		Map<String, Symbol> uint32Props = new HashMap<>();
		uint32Props.put("max", new VariableSymbol("max", UINT32, null, true, true, true, true, false, null, 0xFFFFFFFFL));
		uint32Props.put("min", new VariableSymbol("min", UINT32, null, true, true, true, true, false, null, 0L));
		UINT32.staticProperties = Collections.unmodifiableMap(uint32Props);

		// --- UINT64 / ULONG ---
		Map<String, Symbol> uint64Props = new HashMap<>();
		// In two's complement, -1L represents the bit pattern for the maximum unsigned 64-bit integer.
		uint64Props.put("max", new VariableSymbol("max", UINT64, null, true, true, true, true, false, null, -1L));
		uint64Props.put("min", new VariableSymbol("min", UINT64, null, true, true, true, true, false, null, 0L));
		UINT64.staticProperties = Collections.unmodifiableMap(uint64Props);

		// --- FLOAT ---
		Map<String, Symbol> floatProps = new HashMap<>();
		floatProps.put("max", new VariableSymbol("max", FLOAT, null, true, true, true, true, false, null, Float.MAX_VALUE));
		floatProps.put("min", new VariableSymbol("min", FLOAT, null, true, true, true, true, false, null, -Float.MAX_VALUE));
		floatProps.put("epsilon", new VariableSymbol("epsilon", FLOAT, null, true, true, true, true, false, null, Float.MIN_NORMAL));
		FLOAT.staticProperties = Collections.unmodifiableMap(floatProps);

		// --- DOUBLE ---
		Map<String, Symbol> doubleProps = new HashMap<>();
		doubleProps.put("max", new VariableSymbol("max", DOUBLE, null, true, true, true, true, false, null, Double.MAX_VALUE));
		doubleProps.put("min", new VariableSymbol("min", DOUBLE, null, true, true, true, true, false, null, -Double.MAX_VALUE));
		doubleProps.put("epsilon", new VariableSymbol("epsilon", DOUBLE, null, true, true, true, true, false, null, Double.MIN_NORMAL));
		DOUBLE.staticProperties = Collections.unmodifiableMap(doubleProps);

		staticPropertiesInitialized = true;
	}

	// NEW: Method to resolve a static property by name.
	public Symbol resolveStaticProperty(String name)
	{
		return staticProperties.get(name);
	}

	@Override
	public boolean isReferenceType()
	{
		// The 'null' literal is also considered a reference type for assignment compatibility
		return this.equals(STRING);
	}

	/**
	 * Determines if this type is assignable from another type.
	 * This method should be used for variable-to-variable assignments.
	 */
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

		// This is a common implicit conversion for character literals.
		if (this.equals(CHAR) || this.equals(CHAR16) || this.equals(CHAR32))
		{
			if (other.isNumeric())
			{
				return true;
			}
		}

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

		// Allow signed -> unsigned widening if the target has larger or equal width
		if (this.equals(UINT64) || this.equals(ULONG))
		{
			// uint64 can take any signed 32/64-bit value (all int32 values fit)
			return other.equals(INT32) || other.equals(INT64) ||
					other.equals(UINT32) || other.equals(UINT16) || other.equals(UINT8) ||
					other.equals(CHAR) || other.equals(CHAR16) || other.equals(CHAR32);
		}

		if (this.equals(UINT32) || this.equals(UINT))
		{
			// uint32 can take int16, int8, etc. (all their values fit)
			return other.equals(INT16) || other.equals(INT8) ||
					other.equals(UINT16) || other.equals(UINT8) ||
					other.equals(CHAR) || other.equals(CHAR16);
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

		// --- START OF FIX ---
		// For a binary operation, types are compatible if EITHER can be assigned to the other.
		return this.isAssignableTo(other) || other.isAssignableTo(this);
		// --- END OF FIX ---
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

	@Override
	public boolean isInteger()
	{
		return this.equals(INT8) || this.equals(INT16) || this.equals(INT32) || this.equals(INT64)
				|| this.equals(UINT8) || this.equals(UINT16) || this.equals(UINT32) || this.equals(UINT64);
	}

	public boolean isFloatingPoint()
	{
		return this.equals(FLOAT) || this.equals(DOUBLE);
	}

	public static boolean fitsInRange(Type targetType, long value)
	{
		if (!(targetType instanceof PrimitiveType))
		{
			return false;
		}

		if (targetType.equals(PrimitiveType.INT8))
		{
			return value >= Byte.MIN_VALUE && value <= Byte.MAX_VALUE;
		}
		if (targetType.equals(PrimitiveType.UINT8))
		{
			return value >= 0 && value <= 255;
		}
		if (targetType.equals(PrimitiveType.INT16))
		{
			return value >= Short.MIN_VALUE && value <= Short.MAX_VALUE;
		}
		if (targetType.equals(PrimitiveType.UINT16))
		{
			return value >= 0 && value <= 65535;
		}
		if (targetType.equals(PrimitiveType.INT32))
		{
			return value >= Integer.MIN_VALUE && value <= Integer.MAX_VALUE;
		}
		if (targetType.equals(PrimitiveType.UINT32))
		{
			return value >= 0 && value <= 0xFFFFFFFFL;
		}
		// INT64 and UINT64 — practically always fits if parsed as long
		return true;
	}


	public boolean isSigned()
	{
		return this.equals(INT8) || this.equals(INT16) || this.equals(INT32) || this.equals(INT64) || this.equals(FLOAT) || this.equals(DOUBLE);
	}
}