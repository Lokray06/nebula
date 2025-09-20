// File: src/main/java/com/juanpa/nebula/transpiler/semantics/NamespaceType.java
package com.juanpa.nebula.compiler.semantics;

/**
 * Represents a namespace as a type. Used for structural information rather than a type with values.
 */
public class NamespaceType extends Type
{
	private final String qualifiedName;
	private final String parentNamespaceName; // Null for top-level namespaces

	public NamespaceType(String qualifiedName, String parentNamespaceName)
	{
		super(qualifiedName); // Name is the FQN
		this.qualifiedName = qualifiedName;
		this.parentNamespaceName = parentNamespaceName;
	}

	public String getQualifiedName()
	{
		return qualifiedName;
	}

	public String getParentNamespaceName()
	{
		return parentNamespaceName;
	}

	@Override
	public boolean isAssignableFrom(Type other)
	{
		// Namespaces are not assignable in the typical sense of values.
		// This is primarily a structural type.
		return false;
	}

	@Override
	public boolean isCompatibleWith(Type other)
	{
		// Namespaces are not compatible with other types for operations.
		return false;
	}

	@Override
	public boolean equals(Object o)
	{
		if(this == o)
			return true;
		if(o == null || getClass() != o.getClass())
			return false;
		if(!super.equals(o))
			return false; // Check name equality from base Type
		NamespaceType that = (NamespaceType) o;
		return qualifiedName.equals(that.qualifiedName);
	}

	@Override
	public int hashCode()
	{
		return super.hashCode();
	}
}