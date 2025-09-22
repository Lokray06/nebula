// File: src/main/java/com/juanpa/nebula/transpiler/semantics/ErrorType.java
package com.juanpa.nebula.compiler.semantics;

/**
 * Represents an error type, used to propagate type checking errors.
 * This is a singleton.
 */
public class ErrorType extends Type
{
	public static final ErrorType INSTANCE = new ErrorType();

	private ErrorType()
	{
		super("error");
	}

	@Override
	public boolean isAssignableFrom(Type other)
	{
		// An error type can be assigned from anything (to allow error propagation),
		// but nothing can be assigned to it unless it's also an error.
		return other instanceof ErrorType;
	}

	@Override
	public boolean isCompatibleWith(Type other)
	{
		// An error type is compatible only with other error types for operations.
		return other instanceof ErrorType;
	}

	@Override
	public boolean equals(Object o)
	{
		return this == o; // Singleton equality
	}

	@Override
	public int hashCode()
	{
		return super.hashCode();
	}
}