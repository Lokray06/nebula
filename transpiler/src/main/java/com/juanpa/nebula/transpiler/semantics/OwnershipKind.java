package com.juanpa.nebula.transpiler.semantics;

/**
 * Represents the inferred ownership semantics for a variable or type in Nebula.
 * This metadata guides the CppGenerator on whether to use stack allocation,
 * unique_ptr, shared_ptr, or other strategies.
 */
public enum OwnershipKind
{
	/**
	 * The object's lifetime is strictly lexical and confined to its scope.
	 * It can be allocated directly on the C++ stack (e.g., `MyClass obj;`).
	 * This is the most efficient option.
	 */
	STACK,

	/**
	 * The object has a single, clear owner. Its lifetime is managed by
	 * a `std::unique_ptr`, allowing for efficient heap allocation and automatic
	 * cleanup. Ownership can be moved but not shared.
	 */
	UNIQUE,

	/**
	 * The object has multiple owners, and its lifetime is uncertain or shared
	 * across different scopes. This is the safest, most flexible, but potentially
	 * least performant heap allocation strategy, using `std::shared_ptr`.
	 * This is the default conservative fallback.
	 */
	SHARED,

	/**
	 * A non-owning reference to an object managed by a `std::shared_ptr`.
	 * Used to break reference cycles (e.g., parent-child back-references).
	 * Maps to `std::weak_ptr`.
	 */
	WEAK,

	/**
	 * Represents a raw C++ pointer or a native, non-smart-pointer type.
	 * Used for interacting with native C++ libraries or for wrapper types
	 * where Nebula's ownership model does not apply.
	 */
	RAW
}