// File: src/main/java/com/juanpa/nebula/transpiler/codegen/CppGenerator.java
package com.juanpa.nebula.transpiler.codegen;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.ast.declarations.*;
import com.juanpa.nebula.transpiler.ast.expressions.*;
import com.juanpa.nebula.transpiler.ast.statements.*;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType;
import com.juanpa.nebula.transpiler.semantics.*;

import java.util.*;
import java.util.stream.Collectors;

/**
 * CppGenerator is responsible for traversing the Abstract Syntax Tree (AST)
 * and generating corresponding C++ source code.
 * --- MODIFIED ---
 * This version is ownership-aware and generates optimized C++ by choosing between
 * stack allocation, std::unique_ptr, and std::shared_ptr based on semantic analysis.
 */
public class CppGenerator implements ASTVisitor<String>
{
	private StringBuilder currentClassCodeBuilder;
	private StringBuilder currentHeaderCodeBuilder;
	private final SemanticAnalyzer semanticAnalyzer;
	private int indentLevel = 0;
	private ClassSymbol currentClassSymbol;
	private String currentNamespacePrefix;
	private final Map<String, String> generatedClassCodeMap;
	private final Map<String, ClassSymbol> declaredClasses;
	private boolean inConstructor = false; // <-- ADD THIS FIELD
	private MethodSymbol currentMethodSymbol = null; // <-- ADD THIS FIELD

	// --- NEW ---
	// State to track the expected ownership for the expression currently being visited.
	// This allows a parent node (like a variable declaration) to inform a child
	// (like a 'new' expression) how it should be allocated.
	private OwnershipKind expectedOwnership = OwnershipKind.SHARED;

	public CppGenerator(Map<String, ClassSymbol> declaredClasses, SemanticAnalyzer semanticAnalyzer)
	{
		this.declaredClasses = declaredClasses;
		this.currentClassCodeBuilder = null;
		this.currentHeaderCodeBuilder = null;
		this.currentClassSymbol = null;
		this.currentNamespacePrefix = "";
		this.generatedClassCodeMap = new HashMap<>();
		this.semanticAnalyzer = semanticAnalyzer;
	}

	private void error(Token token, String message)
	{
		System.err.println("Code Generation Error at " + token.getLine() + ":" + token.getColumn() + ": " + message);
	}

	// ... generate() and generateMainCppIfNeeded() remain unchanged ...
	public Map<String, String> generate(Program program)
	{
		// First pass to populate the map for classes and headers (and resolve symbols if applicable)
		for (NamespaceDeclaration namespaceDecl : program.getNamespaceDeclarations())
		{
			namespaceDecl.accept(this);
		}

		// Now generate main.cpp if needed, using the direct FQN construction
		generateMainCppIfNeeded(program);

		return generatedClassCodeMap;
	}

	private void generateMainCppIfNeeded(Program program)
	{
		Optional<ClassDeclaration> mainClassOpt = program.getNamespaceDeclarations().stream()
				.flatMap(ns -> ns.getClassDeclarations().stream())
				.filter(cls -> cls.getMethods().stream()
						.anyMatch(m -> m.getName().getLexeme().equals("main") &&
								m.getModifiers().stream().anyMatch(mod -> mod.getType() == TokenType.STATIC) &&
								m.getParameters().isEmpty() &&
								"void".equals(m.getReturnType().getLexeme()))) // Keep consistent with your new condition
				.findFirst();

		if (mainClassOpt.isPresent())
		{
			ClassDeclaration mainClassDecl = mainClassOpt.get();

			// *** This is the old, working FQN retrieval logic ***
			String mainClassFqn = mainClassDecl.getContainingNamespace().isEmpty() ?
					mainClassDecl.getName().getLexeme() :
					mainClassDecl.getContainingNamespace().replace(".", "::") + "::" + mainClassDecl.getName().getLexeme();

			// Construct the header path based on the FQN
			String mainClassHeaderPath = mainClassFqn.replace("::", "/") + ".h";

			StringBuilder mainCppBuilder = new StringBuilder();

			// Add necessary includes for the main.cpp
			// Based on your original working code, you had specific includes,
			// but the newer code just included the main class header.
			// I'll combine both for robustness, feel free to remove if not needed.
			mainCppBuilder.append("#include <iostream>\n");
			mainCppBuilder.append("#include <memory>\n"); // For std::shared_ptr
			mainCppBuilder.append("#include \"nebula/io/Console.h\"\n");
			mainCppBuilder.append("#include \"nebula/core/Object.h\"\n");
			mainCppBuilder.append("#include \"nebula/core/String.h\"\n");
			mainCppBuilder.append("#include \"").append(mainClassHeaderPath).append("\"\n\n");


			mainCppBuilder.append("// The C++ standard main function, which will call Nebula's main entry point.\n");
			mainCppBuilder.append("int main() {\n");
			mainCppBuilder.append("    ").append(mainClassFqn).append("::main(); // Call the Nebula program's entry point\n");
			mainCppBuilder.append("    return 0;\n");
			mainCppBuilder.append("}\n");

			generatedClassCodeMap.put("main.cpp", mainCppBuilder.toString());
		}
		else
		{
			System.out.println("No static 'main()' method found. Skipping main.cpp generation.");
		}
	}

	// ... appendLine, appendHeaderLine, indent, dedent, resolveTypeFromToken helpers remain unchanged ...
	private void appendLine(String line)
	{
		if (currentClassCodeBuilder == null)
		{
			System.err.println("Error: currentClassCodeBuilder is null. This should be initialized per class in visitClassDeclaration.");
			return;
		}
		for (int i = 0; i < indentLevel; i++)
		{
			currentClassCodeBuilder.append("	");
		}
		currentClassCodeBuilder.append(line).append("\n");
	}

	private void appendHeaderLine(String line)
	{
		if (currentHeaderCodeBuilder == null)
		{
			System.err.println("Error: currentHeaderCodeBuilder is null. This should be initialized per class in visitClassDeclaration.");
			return;
		}
		for (int i = 0; i < indentLevel; i++)
		{
			currentHeaderCodeBuilder.append("	");
		}
		currentHeaderCodeBuilder.append(line).append("\n");
	}

	private void indent()
	{
		indentLevel++;
	}

	private void dedent()
	{
		if (indentLevel > 0)
		{
			indentLevel--;
		}
	}

	private Type resolveTypeFromToken(Token typeToken, int arrayRank)
	{
		Type baseType;
		switch (typeToken.getType())
		{
			case INT:
				baseType = PrimitiveType.INT;
				break;
			case BOOL:
				baseType = PrimitiveType.BOOL;
				break;
			case FLOAT:
				baseType = PrimitiveType.FLOAT;
				break;
			case DOUBLE:
				baseType = PrimitiveType.DOUBLE;
				break;
			case BYTE:
				baseType = PrimitiveType.BYTE;
				break;
			case VOID:
				baseType = PrimitiveType.VOID;
				break;
			case CHAR:
				baseType = PrimitiveType.CHAR;
				break;
			case STRING_KEYWORD:
				ClassSymbol stringClass = declaredClasses.get("nebula.core.String");
				baseType = (stringClass != null) ? stringClass.getType() : ErrorType.INSTANCE;
				break;
			case IDENTIFIER:
				// This is a simplified lookup. A more robust implementation would check
				// current namespace, imports, etc., similar to SemanticAnalyzer.
				ClassSymbol byFqn = declaredClasses.get(currentNamespacePrefix + "." + typeToken.getLexeme());
				if (byFqn != null)
				{
					baseType = byFqn.getType();
				}
				else
				{
					Optional<ClassSymbol> found = declaredClasses.values().stream()
							.filter(cs -> cs.getName().equals(typeToken.getLexeme()))
							.findFirst();
					baseType = found.map(Symbol::getType).orElse(ErrorType.INSTANCE);
				}
				break;
			default:
				baseType = ErrorType.INSTANCE;
				break;
		}

		if (baseType instanceof ErrorType)
		{
			return baseType;
		}

		Type currentType = baseType;
		for (int i = 0; i < arrayRank; i++)
		{
			currentType = new ArrayType(currentType);
		}
		return currentType;
	}

	private Type resolveTypeFromToken(Token typeToken)
	{
		// Keep the old method for calls that don't involve array ranks.
		return resolveTypeFromToken(typeToken, 0);
	}
	// --- NEW HELPER ---

	/**
	 * Gets the inferred OwnershipKind from a resolved symbol.
	 * Defaults to SHARED if the symbol is not a variable or has no ownership info.
	 *
	 * @param symbol The symbol resolved by the semantic analyzer.
	 * @return The inferred OwnershipKind.
	 */
	private OwnershipKind getOwnership(Symbol symbol)
	{
		if (symbol instanceof VariableSymbol)
		{
			// Assumes SemanticAnalyzer has populated this field.
			return ((VariableSymbol) symbol).getOwnership();
		}
		// Methods can also have ownership info for returns/params, but for expressions,
		// we are primarily concerned with the variables holding the values.
		return OwnershipKind.SHARED; // The safest default.
	}

	// --- REWRITTEN ---

	/**
	 * Converts a Nebula Type to its C++ equivalent, now respecting ownership semantics.
	 *
	 * @param type      The Nebula Type to convert.
	 * @param ownership The inferred OwnershipKind guiding C++ type selection.
	 * @return The corresponding C++ type as a string.
	 */
	private String toCppType(Type type, OwnershipKind ownership)
	{
		if (type instanceof PrimitiveType)
		{
			return toCppPrimitiveType(type);
		}
		else if (type instanceof ClassType)
		{
			ClassType classType = (ClassType) type;
			String cppFqn = classType.getFqn().replace(".", "::");
			switch (ownership)
			{
				case STACK:
					return cppFqn; // e.g., MyClass
				case UNIQUE:
					return "std::unique_ptr<" + cppFqn + ">";
				case WEAK:
					return "std::weak_ptr<" + cppFqn + ">";
				case SHARED:
				default:
					return "std::shared_ptr<" + cppFqn + ">";
			}
		}
		else if (type instanceof ArrayType)
		{
			ArrayType arrayType = (ArrayType) type;
			// Element ownership within containers is typically shared or by value.
			// For simplicity, we'll assume elements follow the shared model for now.
			String elementCppType = toCppType(arrayType.getElementType(), OwnershipKind.SHARED);
			String vectorType = "std::vector<" + elementCppType + ">";
			switch (ownership)
			{
				case STACK:
					return vectorType;
				case UNIQUE:
					return "std::unique_ptr<" + vectorType + ">";
				case SHARED:
				default:
					return "std::shared_ptr<" + vectorType + ">";
			}
		}
		else if (type instanceof NullType)
		{
			return "decltype(nullptr)";
		}
		else if (type instanceof ErrorType)
		{
			return "/* ERROR_TYPE */ void*";
		}
		return "void*";
	}

	/**
	 * Overloaded helper for convenience. Defaults to SHARED ownership if not specified.
	 */
	private String toCppType(Type type)
	{
		return toCppType(type, OwnershipKind.SHARED);
	}

	/**
	 * Helper to map primitive Nebula types to C++ primitive types. Unchanged.
	 */
	private String toCppPrimitiveType(Type type)
	{
		if (type.equals(PrimitiveType.BOOL))
		{
			return "bool";
		}
		if (type.equals(PrimitiveType.CHAR))
		{
			return "char";
		}
		if (type.equals(PrimitiveType.CHAR16))
		{
			return "char16_t";
		}
		if (type.equals(PrimitiveType.CHAR32))
		{
			return "char32_t";
		}
		if (type.equals(PrimitiveType.INT8) || type.equals(PrimitiveType.BYTE))
		{
			return "int8_t";
		}
		if (type.equals(PrimitiveType.INT16) || type.equals(PrimitiveType.SHORT))
		{
			return "int16_t";
		}
		if (type.equals(PrimitiveType.INT32) || type.equals(PrimitiveType.INT))
		{
			return "int32_t";
		}
		if (type.equals(PrimitiveType.INT64) || type.equals(PrimitiveType.LONG))
		{
			return "int64_t";
		}
		if (type.equals(PrimitiveType.UINT8) || type.equals(PrimitiveType.UBYTE))
		{
			return "uint8_t";
		}
		if (type.equals(PrimitiveType.UINT16) || type.equals(PrimitiveType.USHORT))
		{
			return "uint16_t";
		}
		if (type.equals(PrimitiveType.UINT32) || type.equals(PrimitiveType.UINT))
		{
			return "uint32_t";
		}
		if (type.equals(PrimitiveType.UINT64) || type.equals(PrimitiveType.ULONG))
		{
			return "uint64_t";
		}
		if (type.equals(PrimitiveType.FLOAT))
		{
			return "float";
		}
		if (type.equals(PrimitiveType.DOUBLE))
		{
			return "double";
		}
		if (type.equals(PrimitiveType.VOID))
		{
			return "void";
		}
		return "/* unknown_primitive */";
	}

	// ... getSimpleClassName and formatCppParameters remain unchanged ...
	private String getSimpleClassName(String fqn)
	{
		int lastDot = fqn.lastIndexOf('.');
		return lastDot == -1 ? fqn : fqn.substring(lastDot + 1);
	}

	private String formatCppParameters(List<Token> parameters)
	{
		if (parameters.isEmpty())
		{
			return "";
		}
		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < parameters.size(); i += 2) // Iterate by pairs (type, name)
		{
			Token paramTypeToken = parameters.get(i);
			Token paramNameToken = parameters.get(i + 1);

			Type paramType = resolveTypeFromToken(paramTypeToken);
			String cppType = toCppType(paramType);
			String paramName = paramNameToken.getLexeme();

			// For Nebula class types, pass shared_ptr by const reference to avoid copying smart pointers
			if (paramType instanceof ClassType || paramType instanceof ArrayType) // Also pass vectors by const ref
			{
				sb.append("const ").append(cppType).append("& ").append(paramName);
			}
			else
			{
				sb.append(cppType).append(" ").append(paramName);
			}

			if (i < parameters.size() - 2) // Check if there are more parameter pairs
			{
				sb.append(", ");
			}
		}
		return sb.toString();
	}

	// ... visitProgram, visitImportDirective, visitNamespaceDeclaration, visitClassDeclaration, etc. remain unchanged up to the statement/expression visitors ...
	@Override
	public String visitProgram(Program program)
	{
		// The 'generate' method already orchestrates the namespace and class visits.
		// This method is primarily for AST traversal.
		return null;
	}

	@Override
	public String visitImportDirective(ImportDirective directive)
	{
		// Import directives are handled at the class header generation phase
		// to determine which C++ headers to include. No direct C++ code generation here.
		return null;
	}

	@Override
	public String visitNamespaceDeclaration(NamespaceDeclaration declaration)
	{
		String previousNamespacePrefix = this.currentNamespacePrefix;
		String namespacePart = getQualifiedNameFromExpressionInternal(declaration.getNameExpression());
		this.currentNamespacePrefix = previousNamespacePrefix.isEmpty() ? namespacePart : previousNamespacePrefix + "." + namespacePart;

		for (ClassDeclaration classDecl : declaration.getClassDeclarations())
		{
			classDecl.accept(this);
		}

		this.currentNamespacePrefix = previousNamespacePrefix;
		return null;
	}


	@Override
	public String visitClassDeclaration(ClassDeclaration declaration)
	{
		// This method now acts as the entry point for generating a single class's .h and .cpp files.
		currentClassCodeBuilder = new StringBuilder();
		currentHeaderCodeBuilder = new StringBuilder();
		indentLevel = 0;

		ClassSymbol resolvedClassSymbol = (declaration.getNameExpression().getResolvedSymbol() instanceof ClassSymbol)
				? (ClassSymbol) declaration.getNameExpression().getResolvedSymbol()
				: declaredClasses.get(declaration.getContainingNamespace() + "." + declaration.getName().getLexeme());

		if (resolvedClassSymbol == null)
		{
			error(declaration.getName(), "Class symbol not resolved for class '" + declaration.getName().getLexeme() + "'. Skipping code generation.");
			return null;
		}

		ClassSymbol previousClassSymbol = this.currentClassSymbol;
		this.currentClassSymbol = resolvedClassSymbol;

		String fqn = currentClassSymbol.getFqn();
		String simpleName = currentClassSymbol.getName();
		String headerGuard = fqn.toUpperCase().replace('.', '_') + "_H";

		// --- Generate Header (.h) ---
		appendHeaderLine("// " + fqn.replace('.', '/') + ".h");
		appendHeaderLine("#ifndef " + headerGuard);
		appendHeaderLine("#define " + headerGuard);
		appendHeaderLine("");
		appendHeaderLine("#include <memory>");
		appendHeaderLine("#include <string>");
		appendHeaderLine("#include <vector>");
		appendHeaderLine("#include <cmath>");
		appendHeaderLine("#include <iostream>");
		appendHeaderLine("#include <iomanip> // For std::setprecision");
		appendHeaderLine("#include <limits>  // For std::numeric_limits");
		appendHeaderLine("");

		Set<String> headerDeps = collectClassDependencies(declaration, false);

		if (currentClassSymbol.getType().getSuperClassType() instanceof ClassType)
		{
			headerDeps.add(((ClassType) currentClassSymbol.getType().getSuperClassType()).getFqn());
		}

		for (String dep : headerDeps)
		{
			if (!dep.equals(fqn))
			{
				appendHeaderLine("#include \"" + dep.replace('.', '/') + ".h\"");
			}
		}
		appendHeaderLine("");
		appendHeaderLine("namespace nebula { namespace core { class Object; class String; } }");
		appendHeaderLine("");
		String[] namespaceParts = currentNamespacePrefix.split("\\.");
		for (String ns : namespaceParts)
		{
			if (!ns.isEmpty())
			{
				appendHeaderLine("namespace " + ns + " {");
				indent();
			}
		}

		String inheritance = ""; // Start with no inheritance
		String fqnForInheritance = fqn.replace(".", "::");

		// ONLY add an inheritance clause if this class is NOT the base Object class.
		if (!"nebula.core.Object".equals(fqn))
		{
			// Determine the direct base class, defaulting to Object.
			String baseClass = "nebula::core::Object";
			if (currentClassSymbol.getType().getSuperClassType() instanceof ClassType)
			{
				baseClass = ((ClassType) currentClassSymbol.getType().getSuperClassType()).getFqn().replace(".", "::");
			}

			inheritance = " : public " + baseClass;

			// MODIFICATION: Only inherit from enable_shared_from_this if the class is NOT stack-allocatable.
			if (!currentClassSymbol.isNative() && !semanticAnalyzer.getStackAllocatedClasses().contains(fqn))
			{
				inheritance += ", public std::enable_shared_from_this<" + fqnForInheritance + ">";
			}
		}


		appendHeaderLine("class " + simpleName + inheritance + " {");
		appendHeaderLine("public:");
		indent();

		// +++ FIX: Add virtual destructor to Object for polymorphism +++
		if ("nebula.core.Object".equals(fqn))
		{
			appendHeaderLine("virtual ~Object() = default; // Enable RTTI for dynamic_cast");
		}

		for (FieldDeclaration field : declaration.getFields())
		{
			VariableSymbol varSymbol = (VariableSymbol) currentClassSymbol.getClassScope().resolve(field.getName().getLexeme());
			if (varSymbol != null)
			{
				String fieldCppType = toCppType(field.getResolvedType());
				String staticQualifier = varSymbol.isStatic() ? "static " : "";
				appendHeaderLine(staticQualifier + fieldCppType + " " + field.getName().getLexeme() + ";");
			}
		}
		appendHeaderLine("");

		for (ConstructorDeclaration ctor : declaration.getConstructors())
		{
			appendHeaderLine(simpleName + "(" + formatCppParameters(ctor.getParameters()) + ");");
		}
		if (declaration.getConstructors().isEmpty())
		{
			appendHeaderLine(simpleName + "(); // Default constructor");
		}
		appendHeaderLine("");

		for (MethodDeclaration method : declaration.getMethods())
		{
			MethodSymbol methodSymbol = method.getResolvedSymbol();
			if (methodSymbol != null)
			{
				String returnType = toCppType(methodSymbol.getType());
				String methodName = methodSymbol.isOperator() ? "operator" + methodSymbol.getName() : methodSymbol.getMangledName();
				String staticQualifier = methodSymbol.isStatic() ? "static " : "";

				// --- FIX 2: Correctly determine which methods should be const ---
				boolean isConst = !methodSymbol.isStatic();

				// For non-native classes, be more selective about what is const.
				if (!currentClassSymbol.isNative())
				{
					isConst = !methodSymbol.isStatic() && (methodName.startsWith("get_") || methodName.equals("toString"));
				}
				String constQualifier = isConst ? " const" : "";


				appendHeaderLine(staticQualifier + returnType + " " + methodName + "(" + formatCppParameters(method.getParameters()) + ")" + constQualifier + ";");
			}
		}

		// Property Getter/Setter Declarations
		for (PropertyDeclaration prop : declaration.getProperties())
		{
			PropertySymbol propSymbol = prop.getResolvedSymbol();
			if (propSymbol != null)
			{
				if (propSymbol.getGetter() != null)
				{
					MethodSymbol getter = propSymbol.getGetter();
					String returnType = toCppType(getter.getType());
					// Getters are always const
					appendHeaderLine(returnType + " " + getter.getName() + "() const;");
				}
				if (propSymbol.getSetter() != null)
				{
					MethodSymbol setter = propSymbol.getSetter();
					String paramType = toCppType(propSymbol.getType());
					// Setters are never const
					appendHeaderLine("void " + setter.getName() + "(const " + paramType + "& value);");
				}
			}
		}

		// This handles the special case for the native String class's private members
		if (currentClassSymbol.isNative() && "nebula.core.String".equals(fqn))
		{
			// FIX: Pass clean strings without manual escape characters.
			// Use an empty string for a blank line.
			appendHeaderLine("");
			appendHeaderLine("// Special constructor for wrapping std::string");
			appendHeaderLine("String(const std::string& raw_str);");
			appendHeaderLine("");
			appendHeaderLine("// Accessor for the raw C++ string data");
			appendHeaderLine("const std::string& raw() const;");
		}

		// Backing Fields for Auto-Properties
		List<PropertyDeclaration> autoProps = declaration.getProperties().stream()
				.filter(PropertyDeclaration::isAuto)
				.toList();

		if (!autoProps.isEmpty())
		{
			dedent(); // Exit public section
			appendHeaderLine("private:");
			indent();
			for (PropertyDeclaration prop : autoProps)
			{
				PropertySymbol propSymbol = prop.getResolvedSymbol();
				if (propSymbol != null)
				{
					String fieldType = toCppType(propSymbol.getType());
					String fieldName = "_" + propSymbol.getName();
					appendHeaderLine(fieldType + " " + fieldName + ";");
				}
			}
		}

		// Add the private _data field for the native String class
		if (currentClassSymbol.isNative() && "nebula.core.String".equals(fqn))
		{
			if (autoProps.isEmpty())
			{ // Only start a private section if one isn't already open
				dedent();
				appendHeaderLine("private:");
				indent();
			}
			appendHeaderLine("std::string _data;");
		}

		dedent();
		appendHeaderLine("}; // class " + simpleName);

		for (int i = namespaceParts.length - 1; i >= 0; i--)
		{
			if (!namespaceParts[i].isEmpty())
			{
				dedent();
				appendHeaderLine("} // namespace " + namespaceParts[i]);
			}
		}
		appendHeaderLine("\n#endif // " + headerGuard);
		generatedClassCodeMap.put(fqn.replace('.', '/') + ".h", currentHeaderCodeBuilder.toString());

		// --- Generate Source (.cpp) ---
		appendLine("// " + fqn.replace('.', '/') + ".cpp");
		appendLine("#include \"" + fqn.replace('.', '/') + ".h\"");

		Set<String> cppDeps = collectClassDependencies(declaration, true);
		for (String dep : cppDeps)
		{
			if (currentClassSymbol.getType().getSuperClassType() instanceof ClassType &&
					dep.equals(((ClassType) currentClassSymbol.getType().getSuperClassType()).getFqn()))
			{
				continue;
			}
			appendLine("#include \"" + dep.replace('.', '/') + ".h\"");
		}

		appendLine("#include <sstream>");
		appendLine("#include <functional>");
		appendLine("#include <cstdint>");
		appendLine("");

		for (String ns : namespaceParts)
		{
			if (!ns.isEmpty())
			{
				appendLine("namespace " + ns + " {");
				indent();
			}
		}

		generateGenericClassCpp(declaration);

		for (int i = namespaceParts.length - 1; i >= 0; i--)
		{
			if (!namespaceParts[i].isEmpty())
			{
				dedent();
				appendLine("} // namespace " + namespaceParts[i]);
			}
		}
		generatedClassCodeMap.put(fqn.replace('.', '/') + ".cpp", currentClassCodeBuilder.toString());

		this.currentClassSymbol = previousClassSymbol;
		return null;
	}

	@Override
	public String visitPropertyDeclaration(PropertyDeclaration declaration)
	{
		// This visitor is responsible for generating the C++ code for the property's
		// get and set methods. The declaration of the backing field (for auto-props)
		// happens in the header generation part of visitClassDeclaration.

		PropertySymbol propSymbol = declaration.getResolvedSymbol();
		if (propSymbol == null)
		{
			error(declaration.getName(), "Property symbol not resolved for '" + declaration.getName().getLexeme() + "'.");
			return null;
		}

		String simpleName = currentClassSymbol.getName();

		// --- Generate Getter ---
		if (declaration.getGetAccessor() != null)
		{
			MethodSymbol getter = propSymbol.getGetter();
			appendLine(toCppType(getter.getType()) + " " + simpleName + "::" + getter.getName() + "() const {");
			indent();
			if (declaration.isAuto())
			{
				appendLine("return this->_" + propSymbol.getName() + ";");
			}
			else
			{
				declaration.getGetAccessor().getBody().accept(this);
			}
			dedent();
			appendLine("}\n");
		}

		// --- Generate Setter ---
		if (declaration.getSetAccessor() != null)
		{
			MethodSymbol setter = propSymbol.getSetter();
			appendLine("void " + simpleName + "::" + setter.getName() + "(const " + toCppType(propSymbol.getType()) + "& value) {");
			indent();
			if (declaration.isAuto())
			{
				appendLine("this->_" + propSymbol.getName() + " = value;");
			}
			else
			{
				declaration.getSetAccessor().getBody().accept(this);
			}
			dedent();
			appendLine("}\n");
		}

		return null;
	}

	@Override
	public String visitConstructorDeclaration(ConstructorDeclaration declaration)
	{
		// This is handled within visitClassDeclaration.
		return null;
	}

	@Override
	public String visitMethodDeclaration(MethodDeclaration declaration)
	{
		// This is handled within visitClassDeclaration.
		return null;
	}

	@Override
	public String visitFieldDeclaration(FieldDeclaration declaration)
	{
		OwnershipKind ownership = OwnershipKind.SHARED;

		if (currentClassSymbol != null)
		{
			Symbol sym = currentClassSymbol.getClassScope().resolve(declaration.getName().getLexeme());
			if (sym instanceof VariableSymbol)
			{
				ownership = getOwnership(sym);
			}
		}

		if (declaration.getInitializer() != null)
		{
			OwnershipKind old = this.expectedOwnership;
			this.expectedOwnership = ownership;
			String code = declaration.getInitializer().accept(this);
			this.expectedOwnership = old;
			return code;
		}
		return null;
	}


	@Override
	public String visitCastExpression(CastExpression expression)
	{
		Type castType = expression.getResolvedType();
		Type exprType = expression.getExpression().getResolvedType();
		String expr = expression.getExpression().accept(this);

		ClassSymbol stringClass = declaredClasses.get("nebula.core.String");

		// Check for any primitive type cast to a String.
		if (stringClass != null && castType.equals(stringClass.getType()) && (exprType instanceof PrimitiveType))
		{
			PrimitiveType primitiveExprType = (PrimitiveType) exprType;

			// Special case for casting a character primitive to a String.
			// This must be handled separately from other numeric types.
			if (primitiveExprType.equals(PrimitiveType.CHAR) ||
					primitiveExprType.equals(PrimitiveType.CHAR16) ||
					primitiveExprType.equals(PrimitiveType.CHAR32))
			{
				return "std::make_shared<nebula::core::String>(std::string(1, " + expr + "))";
			}
			// Special case for casting a boolean primitive to a String.
			else if (primitiveExprType.equals(PrimitiveType.BOOL))
			{
				return "std::make_shared<nebula::core::String>(" + expr + " ? \"true\" : \"false\")";
			}
			// General case for casting any other numeric primitive to a String.
			else if (primitiveExprType.isNumeric())
			{
				return "std::make_shared<nebula::core::String>(std::to_string(" + expr + "))";
			}
			// Fallback for any other primitive cast to a string (e.g., void)
			else
			{
				// We can't handle other cases and must generate a compilation error here.
				// Or, return a default empty string for now.
				return "std::make_shared<nebula::core::String>(\"UNKNOWN\")";
			}
		}

		// Default behavior for all other casts (class to class, numeric to numeric, etc.),
		// which is a direct static_cast.
		String cppType = toCppType(castType);
		return "static_cast<" + cppType + ">(" + expr + ")";
	}

	@Override
	public String visitTernaryExpression(TernaryExpression expression)
	{
		String condition = expression.getCondition().accept(this);
		String thenBranch = expression.getThenBranch().accept(this);
		String elseBranch = expression.getElseBranch().accept(this);
		return "(" + condition + " ? " + thenBranch + " : " + elseBranch + ")";
	}

	@Override
	public String visitBlockStatement(BlockStatement statement)
	{
		for (Statement stmt : statement.getStatements())
		{
			String generatedCode = stmt.accept(this);
			if (generatedCode != null && !generatedCode.trim().isEmpty())
			{
				appendLine(generatedCode);
			}
		}
		return null;
	}


	@Override
	public String visitExpressionStatement(ExpressionStatement statement)
	{
		return statement.getExpression().accept(this) + ";";
	}

	@Override
	public String visitIfStatement(IfStatement statement)
	{
		String conditionCode = statement.getCondition().accept(this);
		appendLine("if (" + conditionCode + ") {");
		indent();
		statement.getThenBranch().accept(this);
		dedent();
		if (statement.getElseBranch() != null)
		{
			appendLine("} else {");
			indent();
			statement.getElseBranch().accept(this);
			dedent();
		}
		appendLine("}");
		return null;
	}

	@Override
	public String visitWhileStatement(WhileStatement statement)
	{
		String conditionCode = statement.getCondition().accept(this);
		appendLine("while (" + conditionCode + ") {");
		indent();
		statement.getBody().accept(this);
		dedent();
		appendLine("}");
		return null;
	}

	@Override
	public String visitForStatement(ForStatement statement)
	{
		// For loop initializers often include declarations, which might already end with ';'.
		// Need to be careful not to double-add.
		String initializerCode = statement.getInitializer() != null ? statement.getInitializer().accept(this) : "";
		String conditionCode = statement.getCondition() != null ? statement.getCondition().accept(this) : "";
		String incrementCode = statement.getIncrement() != null ? statement.getIncrement().accept(this) : "";

		// visitor for statements already adds the semicolon, so remove it from the initializer part of the for loop
		if (initializerCode.endsWith(";"))
		{
			initializerCode = initializerCode.substring(0, initializerCode.length() - 1);
		}


		appendLine("for (" + initializerCode + "; " + conditionCode + "; " + incrementCode + ") {");
		indent();
		statement.getBody().accept(this);
		dedent();
		appendLine("}");
		return null;
	}

	// ADD THIS ENTIRE METHOD
	@Override
	public String visitForEachStatement(ForEachStatement statement)
	{
		// 1. Get C++ code for the collection expression
		String collectionCode = statement.getCollection().accept(this);

		// 2. Resolve the loop variable's type to its C++ equivalent
		Type nebulaVarType = resolveTypeFromToken(statement.getTypeToken(), statement.getArrayRank());
		String cppVarType = toCppType(nebulaVarType);

		// 3. Get the loop variable's name
		String varName = statement.getVariableName().getLexeme();

		// 4. Determine the iterable expression in C++
		Type collectionType = statement.getCollection().getResolvedType();
		String iterableExpression;

		if (collectionType instanceof ArrayType)
		{
			// Arrays are shared_ptr<vector>, so dereference it for the loop
			iterableExpression = "(*" + collectionCode + ")";
		}
		else
		{
			// Strings are shared_ptr<nebula::core::String>, use raw() to get the std::string
			iterableExpression = collectionCode + "->raw()";
		}

		// 5. Build and append the C++ range-based for loop
		appendLine("for (" + cppVarType + " const& " + varName + " : " + iterableExpression + ") {");
		indent();
		statement.getBody().accept(this);
		dedent();
		appendLine("}");

		return null; // The method appends code directly
	}

	@Override
	public String visitReturnStatement(ReturnStatement statement)
	{
		if (statement.getValue() != null)
		{
			OwnershipKind ownership = OwnershipKind.SHARED;
			if (currentMethodSymbol != null && currentMethodSymbol.getReturnOwnership() != null)
			{
				ownership = currentMethodSymbol.getReturnOwnership();
			}

			OwnershipKind old = this.expectedOwnership;
			this.expectedOwnership = ownership;
			String value = statement.getValue().accept(this);
			this.expectedOwnership = old;

			return "return " + value + ";";
		}
		return "return;";
	}

	// --- MODIFIED ---
	@Override
	public String visitVariableDeclarationStatement(VariableDeclarationStatement statement)
	{
		// --- THIS IS THE FIX ---
		// Instead of searching scopes, get the symbol directly from the AST node.
		// The SemanticAnalyzer has already attached it for us.
		Symbol symbol = statement.getResolvedSymbol();
		// --- END OF FIX ---

		if (symbol == null)
		{
			// This is a fallback/error condition. If the symbol isn't in the method scope,
			// something went wrong in the semantic analyzer. We default to SHARED.
			error(statement.getName(), "Internal code generation error: Could not resolve symbol for local variable '" + statement.getName().getLexeme() + "'.");
		}

		OwnershipKind ownership = getOwnership(symbol);
		Type varType = resolveTypeFromToken(statement.getTypeToken(), statement.getArrayRank());
		String cppType = statement.getTypeToken().getType() == TokenType.VAR ? "auto" : toCppType(varType, ownership);
		String declaration = cppType + " " + statement.getName().getLexeme();

		if (statement.getInitializer() != null)
		{
			OwnershipKind oldOwnership = this.expectedOwnership;
			this.expectedOwnership = ownership;

			String initializerCode = statement.getInitializer().accept(this);
			declaration += " = " + initializerCode;

			this.expectedOwnership = oldOwnership;
		}
		else if (ownership != OwnershipKind.STACK)
		{
			declaration += " = nullptr";
		}

		return declaration + ";";
	}

	@Override
	public String visitSwitchStatement(SwitchStatement statement)
	{
		String switchExprCode = statement.getSwitchExpression().accept(this);
		appendLine("switch (" + switchExprCode + ") {");
		indent();
		for (SwitchCase sc : statement.getCases())
		{
			sc.accept(this);
		}
		if (statement.getDefaultBlock() != null)
		{
			appendLine("default:");
			indent();
			statement.getDefaultBlock().accept(this);
			dedent();
		}
		dedent();
		appendLine("}");
		return null;
	}

	@Override
	public String visitBreakStatement(BreakStatement statement)
	{
		return "";
	}

	/**
	 * Visits an array creation expression (e.g., `new int[10]`) and generates
	 * the C++ equivalent for a std::vector, like `std::vector<int>(10)`.
	 */
	@Override
	public String visitArrayCreationExpression(ArrayCreationExpression expression)
	{
		// 1. Get the resolved type of the array, which is an ArrayType.
		Type resolvedType = expression.getResolvedType();

		// 2. Safely cast to ArrayType and get the element type (e.g., PrimitiveType.INT).
		Type elementType;
		if (resolvedType instanceof ArrayType)
		{
			elementType = ((ArrayType) resolvedType).getElementType();
		}
		else
		{
			// Fallback or error handling for safety.
			error(expression.getFirstToken(), "Array creation expression did not resolve to an array type.");
			return "nullptr";
		}

		// 3. Convert the element type to its C++ string representation (e.g., "int32_t").
		String cppElementType = toCppType(elementType);
		String sizeExpr = expression.getSizeExpression().accept(this);

		// 4. Construct the C++ code to create a vector of the correct element type.
		return "std::make_shared<std::vector<" + cppElementType + ">>(" + sizeExpr + ")";
	}

	/**
	 * Visits an array initializer expression (e.g., `{1, 2, 3}`) and generates
	 * the equivalent C++ initializer list `{1, 2, 3}`.
	 */
	@Override
	public String visitArrayInitializerExpression(ArrayInitializerExpression expression)
	{
		Type resolvedType = expression.getResolvedType();
		if (!(resolvedType instanceof ArrayType))
		{
			error(expression.getFirstToken(), "Array initializer expression does not resolve to an array type.");
			return "nullptr /* ERROR: Invalid array initializer */";
		}

		ArrayType arrayType = (ArrayType) resolvedType;
		String elementType = toCppType(arrayType.getElementType());

		// Handle the case of an empty array initializer specifically.
		if (expression.getElements().isEmpty())
		{
			return "std::make_shared<std::vector<" + elementType + ">>()";
		}

		String elements = expression.getElements().stream()
				.map(expr -> expr.accept(this))
				.collect(Collectors.joining(", "));

		return "std::make_shared<std::vector<" + elementType + ">>(std::vector<" + elementType + ">{" + elements + "})";
	}

	private void generateGenericClassCpp(ClassDeclaration declaration)
	{
		String simpleName = currentClassSymbol.getName();
		String fqn = currentClassSymbol.getFqn();
		boolean isNative = currentClassSymbol.isNative();

		// 1. Define and initialize static fields
		for (FieldDeclaration field : declaration.getFields())
		{
			VariableSymbol varSymbol = (VariableSymbol) currentClassSymbol.getClassScope().resolve(field.getName().getLexeme());
			if (varSymbol != null && varSymbol.isStatic())
			{
				String fieldCppType = toCppType(field.getResolvedType());
				String initializer = (field.getInitializer() != null) ? field.getInitializer().accept(this) : "{}";
				if (varSymbol.isWrapper())
				{
					initializer = varSymbol.getCppTarget();
				}
				appendLine(fieldCppType + " " + simpleName + "::" + field.getName().getLexeme() + " = " + initializer + ";");
			}
		}
		appendLine("");

		// 2. Constructors (for both native and non-native)
		this.inConstructor = true; // <-- SET FLAG TO TRUE
		if (declaration.getConstructors().isEmpty())
		{
			// Generate default constructor definition
			String initializer = "nebula.core.String".equals(fqn) ? " : _data(\"\")" : "";
			appendLine(simpleName + "::" + simpleName + "()" + initializer + " {} \n");
		}
		else
		{
			for (ConstructorDeclaration ctor : declaration.getConstructors())
			{
				// --- ADD THIS BLOCK to set currentMethodSymbol ---
				List<Type> paramTypes = new ArrayList<>();
				for (int i = 0; i < ctor.getParameters().size(); i += 2)
				{
					paramTypes.add(resolveTypeFromToken(ctor.getParameters().get(i)));
				}
				Symbol resolvedCtor = currentClassSymbol.resolveMember(ctor.getName().getLexeme(), paramTypes);
				if (resolvedCtor instanceof MethodSymbol)
				{
					this.currentMethodSymbol = (MethodSymbol) resolvedCtor;
				}
				// --- END OF ADDED BLOCK ---

				String params = formatCppParameters(ctor.getParameters());
				String ctorSignature = simpleName + "::" + simpleName + "(" + params + ")";

				if ("nebula.core.String".equals(fqn))
				{
					if (ctor.getParameters().isEmpty())
					{
						ctorSignature += " : _data(\"\")";
					}
					else
					{
						ctorSignature += " : _data(other ? other->raw() : \"\")";
					}
				}

				appendLine(ctorSignature + " {");
				indent();
				if (ctor.getBody() != null)
				{
					ctor.getBody().accept(this);
				}
				dedent();
				appendLine("}\n");

				this.currentMethodSymbol = null; // <-- ADD THIS to reset after
			}
		}
		this.inConstructor = false; // <-- SET FLAG TO FALSE

		if (isNative && "nebula.core.String".equals(fqn))
		{
			appendLine(simpleName + "::String(const std::string& raw_str) : _data(raw_str) {}\n");
			appendLine("const std::string& " + simpleName + "::raw() const {");
			indent();
			appendLine("return _data;");
			dedent();
			appendLine("}\n");
		}

		// 3. Methods (from explicit method declarations)
		for (MethodDeclaration method : declaration.getMethods())
		{
			MethodSymbol methodSymbol = method.getResolvedSymbol();
			if (methodSymbol == null)
			{
				continue;
			}
			this.currentMethodSymbol = methodSymbol; // <-- ADD THIS to set for current method

			if (method.isWrapper())
			{
				String returnType = toCppType(methodSymbol.getType());
				String methodName = methodSymbol.isOperator() ? "operator" + methodSymbol.getName() : methodSymbol.getName();
				String params = formatCppParameters(method.getParameters());

				boolean isConst = !methodSymbol.isStatic();

				if (!currentClassSymbol.isNative())
				{
					isConst = !methodSymbol.isStatic() &&
							(methodName.startsWith("get_") || methodName.equals("toString"));
				}
				String constQualifier = isConst ? " const" : "";

				appendLine(returnType + " " + simpleName + "::" + methodName + "(" + params + ")" + constQualifier + " {");
				indent();
				String body = processWrapperBody(methodSymbol, method.getParameters());
				if (!methodSymbol.getType().equals(PrimitiveType.VOID))
				{
					appendLine("return " + body + ";");
				}
				else
				{
					appendLine(body + ";");
				}
				dedent();
				appendLine("}\n");
			}
			else if (!isNative)
			{
				String returnType = toCppType(methodSymbol.getType());
				String methodName = methodSymbol.isOperator() ? "operator" + methodSymbol.getName() : methodSymbol.getMangledName();
				String params = formatCppParameters(method.getParameters());
				// --- FIX 2 (applied again for definition): Correctly determine which methods should be const ---
				boolean isConst = !methodSymbol.isStatic() &&
						(methodName.startsWith("get_") || methodName.equals("toString"));
				String constQualifier = isConst ? " const" : "";

				appendLine(returnType + " " + simpleName + "::" + methodName + "(" + params + ")" + constQualifier + " {");
				indent();
				if (method.getBody() != null)
				{
					method.getBody().accept(this);
				}
				dedent();
				appendLine("}\n");
			}
			this.currentMethodSymbol = null; // <-- ADD THIS to reset after
		}

		// 4. Properties (generates the bodies of get/set methods)
		for (PropertyDeclaration propDecl : declaration.getProperties())
		{
			propDecl.accept(this); // This will call our new visitPropertyDeclaration
		}
	}

	@Override
	public String visitSwitchCase(SwitchCase switchCase)
	{
		String caseValueCode = switchCase.getValue().accept(this);
		appendLine("case " + caseValueCode + ": {");
		indent();
		for (Statement stmt : switchCase.getBody())
		{
			String generatedCode = stmt.accept(this);
			if (generatedCode != null && !generatedCode.isEmpty())
			{
				appendLine(generatedCode);
			}
		}
		appendLine("break;"); // Add break statement for switch cases
		dedent();
		appendLine("}");
		return null;
	}

	@Override
	public String visitConstructorChainingCallStatement(ConstructorChainingCallStatement statement)
	{
		// This method is called to produce the initializer list part of a constructor.
		StringBuilder call = new StringBuilder();
		String keyword = statement.getKeyword().getLexeme();

		List<String> argCodes = statement.getArguments().stream()
				.map(arg -> arg.accept(this))
				.collect(Collectors.toList());

		if (keyword.equals("this"))
		{
			// Call the current class's constructor
			// currentClassSymbol.getName() returns the simple name of the class
			call.append(currentClassSymbol.getName()).append("(");
		}
		else if (keyword.equals("super"))
		{
			// Call the super class's constructor
			String superCppName = "nebula::core::Object"; // Default
			if (currentClassSymbol != null && currentClassSymbol.getType().getSuperClassType() != null)
			{
				ClassType superClassType = (ClassType) currentClassSymbol.getType().getSuperClassType(); // Your cast
				superCppName = superClassType.getFqn().replace(".", "::");
			}
			// Get the simple name for the superclass constructor call if it's not the default Object
			if (superCppName.startsWith("nebula::core::"))
			{
				superCppName = getSimpleClassName(superCppName.replace("::", ".")); // Convert back to dot for simple name extraction
			}
			call.append(superCppName).append("(");
		}
		call.append(String.join(", ", argCodes)).append(")");

		return call.toString();
	}

	@Override
	public String visitBinaryExpression(BinaryExpression expression)
	{
		String left = expression.getLeft().accept(this);
		String right = expression.getRight().accept(this);
		String op = expression.getOperator().getLexeme();
		Type leftType = expression.getLeft().getResolvedType();
		Type rightType = expression.getRight().getResolvedType();

		// Handle null comparisons first
		if ((op.equals("==") || op.equals("!=")) && (leftType.isReferenceType() || rightType.isReferenceType()))
		{
			if (rightType instanceof NullType)
			{
				right = "nullptr";
			}
			if (leftType instanceof NullType)
			{
				left = "nullptr";
			}
			return "(" + left + " " + op + " " + right + ")";
		}

		// --- NEW: Power operator handling ---
		if (op.equals("**"))
		{
			return "std::pow(" + left + ", " + right + ")";
		}

		// --- REVISED STRING CONCATENATION LOGIC ---
		if (op.equals("+") && (isNebulaString(leftType) || isNebulaString(rightType)))
		{
			String leftOperand = left;
			String rightOperand = right;

			// If the left type is a string, but the right isn't, convert the right.
			if (isNebulaString(leftType) && !isNebulaString(rightType))
			{
				rightOperand = convertToNebulaString(expression.getRight());
			}
			// If the right type is a string, but the left isn't, convert the left.
			else if (!isNebulaString(leftType) && isNebulaString(rightType))
			{
				leftOperand = convertToNebulaString(expression.getLeft());
			}

			return "(*" + leftOperand + " + " + rightOperand + ")";
		}

		// --- Original Logic for other operators ---
		if (leftType instanceof ClassType || leftType instanceof ArrayType)
		{
			// For any class type, dereference the left operand to access member operators.
			return "(*" + left + " " + op + " " + right + ")";
		}

		// Default binary expression handling for primitives (e.g., int + int)
		return "(" + left + " " + op + " " + right + ")";
	}

	// Add this new helper method to encapsulate the conversion logic
	private String convertToNebulaString(Expression expression)
	{
		// Unwrap any grouping expressions to get to the core expression
		Expression innerExpression = expression;
		while (innerExpression instanceof GroupingExpression)
		{
			innerExpression = ((GroupingExpression) innerExpression).getExpression();
		}

		Type type = innerExpression.getResolvedType();
		String operand = expression.accept(this);

		if (type instanceof PrimitiveType)
		{
			PrimitiveType primitiveType = (PrimitiveType) type;
			if (primitiveType.equals(PrimitiveType.BOOL))
			{
				return "std::make_shared<nebula::core::String>(" + operand + " ? \"true\" : \"false\")";
			}
			else if (primitiveType.equals(PrimitiveType.CHAR) ||
					primitiveType.equals(PrimitiveType.CHAR16) ||
					primitiveType.equals(PrimitiveType.CHAR32))
			{
				return "std::make_shared<nebula::core::String>(std::string(1, " + operand + "))";
			}
			else if (primitiveType.equals(PrimitiveType.FLOAT) || primitiveType.equals(PrimitiveType.DOUBLE))
			{
				return generateHighPrecisionString(innerExpression);
			}
			else if (primitiveType.isNumeric())
			{
				return "std::make_shared<nebula::core::String>(std::to_string(" + operand + "))";
			}
		}
		else if (type instanceof ClassType)
		{
			// If the operand is another Nebula class, call its toString() method.
			if (!isNebulaString(type))
			{
				Symbol operandSymbol = innerExpression.getResolvedSymbol();
				OwnershipKind ownership = OwnershipKind.SHARED; // Default to shared ownership

				// Safely retrieve the ownership from the symbol if it's a VariableSymbol
				if (operandSymbol instanceof VariableSymbol)
				{
					ownership = ((VariableSymbol) operandSymbol).getOwnership();
				}

				String accessor = (ownership == OwnershipKind.STACK) ? "." : "->";
				return operand + accessor + "toString()";
			}
		}
		// For other cases, just return the operand as is (e.g., if it's already a string).
		return operand;
	}

	@Override
	public String visitUnaryExpression(UnaryExpression expression)
	{
		String operandCode = expression.getRight().accept(this);
		String operator = expression.getOperator().getLexeme();

		// For prefix increment/decrement, the operator comes before the operand
		if (operator.equals("++") || operator.equals("--"))
		{
			return operator + operandCode;
		}
		// For other unary operators (like negation '-', logical NOT '!')
		return operator + operandCode;
	}

	@Override
	public String visitLiteralExpression(LiteralExpression expression)
	{
		Object value = expression.getValue();
		if (expression.getLiteralToken().getType() == TokenType.STRING_LITERAL)
		{
			// Use the new helper to make the string C++ safe
			String escapedValue = escapeCppString(value.toString());
			return "std::make_shared<nebula::core::String>(\"" + escapedValue + "\")";
		}
		if (expression.getLiteralToken().getType() == TokenType.CHAR_LITERAL)
		{
			// Use the new helper to make the string C++ safe
			return "'" + value.toString() + "'";
		}
		if (expression.getLiteralToken().getType() == TokenType.BOOLEAN_LITERAL)
		{
			// Output 'true' or 'false' directly
			return value.toString().toLowerCase();
		}
		if (expression.getLiteralToken().getType() == TokenType.NULL)
		{
			return "nullptr";
		}
		return value.toString();
	}

	@Override
	public String visitIdentifierExpression(IdentifierExpression expression)
	{
		Symbol symbol = expression.getResolvedSymbol();

		// ================== START OF FIX ==================
		if (symbol instanceof PropertySymbol)
		{
			PropertySymbol propSymbol = (PropertySymbol) symbol;
			MethodSymbol getter = propSymbol.getGetter();
			if (getter == null)
			{
				error(expression.getName(), "Property '" + propSymbol.getName() + "' is write-only and cannot be read.");
				return "/* ERROR: read from write-only property */";
			}

			// If it's an instance property, it's an implicit 'this' call.
			if (!propSymbol.isStatic())
			{
				return "this->" + getter.getName() + "()";
			}
			else
			{
				// Handle static property access.
				return propSymbol.getOwnerClass().getFqn().replace(".", "::") + "::" + getter.getName() + "()";
			}
		}
		// =================== END OF FIX ===================

		if (symbol instanceof VariableSymbol)
		{
			VariableSymbol varSym = (VariableSymbol) symbol;
			if (varSym.getOwnerClass() != null && !varSym.isStatic())
			{ // It's an instance field
				return "this->" + varSym.getName();
			}
			else if (varSym.getOwnerClass() != null && varSym.isStatic())
			{
				// It's a static field
				return varSym.getOwnerClass().getFqn().replace(".", "::") + "::" + varSym.getName();
			}
			return varSym.getName();
		}
		if (symbol instanceof ClassSymbol)
		{
			return ((ClassSymbol) symbol).getFqn().replace(".", "::");
		}
		return expression.getName().getLexeme();
	}


	@Override
	public String visitAssignmentExpression(AssignmentExpression expression)
	{
		// Check if the target of the assignment is a property.
		Symbol targetSymbol = expression.getTarget().getResolvedSymbol();

		if (targetSymbol instanceof PropertySymbol)
		{
			PropertySymbol propSymbol = (PropertySymbol) targetSymbol;
			MethodSymbol setter = propSymbol.getSetter();
			if (setter == null)
			{
				error(expression.getTarget().getFirstToken(),
						"Property '" + propSymbol.getName() + "' is read-only.");
				return "/* ERROR: assignment to read-only property */";
			}

			String targetObjectCode;
			String accessor;

			if (expression.getTarget() instanceof DotExpression)
			{
				DotExpression dot = (DotExpression) expression.getTarget();
				Expression left = dot.getLeft();
				targetObjectCode = left.accept(this);
				OwnershipKind ownerOwnership = getOwnership(left.getResolvedSymbol());
				accessor = (ownerOwnership == OwnershipKind.STACK) ? "." : "->";
			}
			else
			{
				targetObjectCode = "this";
				accessor = "->";
			}

			String value = expression.getValue().accept(this);
			return targetObjectCode + accessor + setter.getName() + "(" + value + ")";
		}

		// --- Non-property assignment handling ---
		String target = expression.getTarget().accept(this);

		OwnershipKind ownership = OwnershipKind.SHARED;
		Symbol targetSym = expression.getTarget().getResolvedSymbol();
		if (targetSym != null)
		{
			ownership = getOwnership(targetSym);
		}

		OwnershipKind old = this.expectedOwnership;
		this.expectedOwnership = ownership;
		String value = expression.getValue().accept(this);
		this.expectedOwnership = old;

		// Move semantics for UNIQUE
		if (ownership == OwnershipKind.UNIQUE)
		{
			value = "std::move(" + value + ")";
		}

		String op = expression.getOperator().getLexeme();

		// --- Special case: POWER_ASSIGN ("**=") ---
		if (op.equals("**="))
		{
			// In C++, exponentiation is done with std::pow()
			// We also make sure to assign back to the same target
			return target + " = std::pow(" + target + ", " + value + ")";
		}

		// --- Default handling ---
		return target + " " + op + " " + value;
	}

	@Override
	public String visitCallExpression(CallExpression expr)
	{
		String args = expr.getArguments().stream()
				.map(arg -> arg.accept(this))
				.collect(Collectors.joining(", "));

		Symbol resolvedSymbol = expr.getCallee().getResolvedSymbol();
		if (!(resolvedSymbol instanceof MethodSymbol))
		{
			// Fallback or error for when the resolved symbol isn't a method
			return expr.getCallee().accept(this) + "(" + args + ")";
		}

		MethodSymbol resolvedMethod = (MethodSymbol) resolvedSymbol;
		String calleeString;

		String methodName = resolvedMethod.getMangledName() != null
				? resolvedMethod.getMangledName()
				: resolvedMethod.getName();

		if (expr.getCallee() instanceof IdentifierExpression)
		{
			if (resolvedMethod.isStatic())
			{
				calleeString = this.currentClassSymbol.getFqn().replace(".", "::") + "::" + methodName;
			}
			else
			{
				// Implicit 'this' calls are always on a pointer.
				calleeString = "this->" + methodName;
			}
		}
		else if (expr.getCallee() instanceof DotExpression)
		{
			DotExpression dot = (DotExpression) expr.getCallee();
			String left = dot.getLeft().accept(this);

			// --- START OF FIX ---
			String separator;
			if (resolvedMethod.isStatic())
			{
				separator = "::";
			}
			else
			{
				// Get the ownership of the object on the left of the dot
				Symbol leftSymbol = dot.getLeft().getResolvedSymbol();
				OwnershipKind leftOwnership = getOwnership(leftSymbol);
				// Choose the correct operator based on ownership
				separator = (leftOwnership == OwnershipKind.STACK) ? "." : "->";
			}
			// --- END OF FIX ---

			calleeString = left + separator + methodName;
		}
		else
		{
			calleeString = expr.getCallee().accept(this); // Fallback
		}

		return calleeString + "(" + args + ")";
	}

	// --- MODIFIED ---
	@Override
	public String visitDotExpression(DotExpression expr)
	{
		Symbol symbol = expr.getResolvedSymbol();

		// Property access is handled via getter calls
		if (symbol instanceof PropertySymbol)
		{
			PropertySymbol propSymbol = (PropertySymbol) symbol;
			MethodSymbol getter = propSymbol.getGetter();
			if (getter == null)
			{
				error(expr.getMemberName(), "Property '" + propSymbol.getName() + "' is write-only.");
				return "/* ERROR: access to write-only property */";
			}
			String left = expr.getLeft().accept(this);
			// --- NEW: Check ownership of the left side ---
			OwnershipKind leftOwnership = getOwnership(expr.getLeft().getResolvedSymbol());
			String accessor = (leftOwnership == OwnershipKind.STACK) ? "." : "->";
			return left + accessor + getter.getName() + "()";
		}

		// --- Handle wrapped static fields ---
		if (symbol instanceof VariableSymbol)
		{
			VariableSymbol varSymbol = (VariableSymbol) symbol;
			if (varSymbol.isStatic() && varSymbol.isWrapper() && varSymbol.getCppTarget() != null)
			{
				return varSymbol.getCppTarget();
			}
		}

		String left = expr.getLeft().accept(this);
		String memberName = expr.getMemberName().getLexeme();
		Type leftType = expr.getLeft().getResolvedType();

		// Handle array.size and string.length
		if (leftType instanceof ArrayType && memberName.equals("size"))
		{
			return "static_cast<int>(" + left + "->size())";
		}
		if (isNebulaString(leftType) && memberName.equals("length"))
		{
			return left + "->length()";
		}

		// --- NEW: Ownership-aware member access ---
		Symbol leftSymbol = expr.getLeft().getResolvedSymbol();
		OwnershipKind leftOwnership = getOwnership(leftSymbol);

		boolean isStaticAccess = (symbol instanceof MethodSymbol && ((MethodSymbol) symbol).isStatic()) ||
				(symbol instanceof VariableSymbol && ((VariableSymbol) symbol).isStatic());

		String separator = isStaticAccess ? "::" : (leftOwnership == OwnershipKind.STACK ? "." : "->");

		return left + separator + memberName;
	}

	@Override
	public String visitThisExpression(ThisExpression expression)
	{
		// If the current class has been identified as a stack-allocatable "value type",
		// then `shared_from_this()` is illegal and we must use the raw `this` pointer.
		if (currentClassSymbol != null && semanticAnalyzer.getStackAllocatedClasses().contains(currentClassSymbol.getFqn()))
		{
			return "this";
		}
		else
		{
			// Otherwise, it's a "reference type" managed by shared_ptr.
			// Use the original logic: `this` in a constructor, `shared_from_this()` elsewhere.
			return inConstructor ? "this" : "shared_from_this()";
		}
	}

	// --- MODIFIED ---
	@Override
	public String visitNewExpression(NewExpression expression)
	{
		Type classType = expression.getResolvedType();
		if (classType == null || !(classType instanceof ClassType))
		{
			error(expression.getClassName().getFirstToken(), "Could not resolve class for 'new' expression.");
			return "/* ERROR */";
		}

		ClassType resolvedClassType = (ClassType) classType;
		String cppFqn = resolvedClassType.getFqn().replace(".", "::");

		List<String> argCodes = expression.getArguments().stream()
				.map(arg -> arg.accept(this))
				.collect(Collectors.toList());
		String args = String.join(", ", argCodes);

		System.err.println("Generating a new statement for " + expression.getClassName() + " with the expected ownership " + this.expectedOwnership + ": " + expression);
		// --- NEW: Generate code based on the expected ownership context ---
		switch (this.expectedOwnership)
		{
			case STACK:
				// For stack allocation, we just call the constructor directly.
				return cppFqn + "(" + args + ")";
			case UNIQUE:
				return "std::make_unique<" + cppFqn + ">(" + args + ")";
			case SHARED:
			default:
				// The default, safe behavior.
				return "std::make_shared<" + cppFqn + ">(" + args + ")";
		}
	}

	@Override
	public String visitPostfixUnaryExpression(PostfixUnaryExpression expression)
	{
		String operandCode = expression.getOperand().accept(this);
		String operator = expression.getOperator().getLexeme();

		// For postfix increment/decrement, the operator comes after the operand
		return operandCode + operator;
	}

	// --- MODIFIED ---
	@Override
	public String visitArrayAccessExpression(ArrayAccessExpression expression)
	{
		String arrayExpr = expression.getArray().accept(this);
		String indexExpr = expression.getIndex().accept(this);

		// --- NEW: Check ownership of the array object ---
		Symbol arraySymbol = expression.getArray().getResolvedSymbol();
		OwnershipKind arrayOwnership = getOwnership(arraySymbol);

		// If the array is a stack-allocated std::vector, access it directly.
		if (arrayOwnership == OwnershipKind.STACK)
		{
			return arrayExpr + "[" + indexExpr + "]";
		}

		// Otherwise, it's a smart pointer to a vector, so dereference it.
		return "(*" + arrayExpr + ")[" + indexExpr + "]";
	}

	@Override
	public String visitGroupingExpression(GroupingExpression expression)
	{
		// Enclose the inner expression in parentheses
		return "(" + expression.getExpression().accept(this) + ")";
	}

	@Override
	public String visitIsExpression(IsExpression expression)
	{
		String leftCode = expression.getLeft().accept(this);
		Type targetType = resolveTypeFromToken(expression.getTypeToken());
		if (targetType instanceof ClassType)
		{
			String targetFqn = ((ClassType) targetType).getFqn().replace(".", "::");
			return "(std::dynamic_pointer_cast<" + targetFqn + ">(" + leftCode + ") != nullptr)";
		}
		else if (targetType instanceof ArrayType)
		{
			// `is` check for arrays is not directly supported in C++ RTTI this way.
			// This is a language feature decision. For now, we return false.
			error(expression.getTypeToken(), "'is' expression for array types is not yet supported in C++ codegen.");
			return "false";
		}
		error(expression.getTypeToken(), "'is' expression can only be used with class types.");
		return "false";
	}

	/**
	 * Extracts the fully qualified name (FQN) from an Expression that represents a name
	 * (either an IdentifierExpression or a DotExpression). This helper is for *internal*
	 * FQN tracking (dot-separated) within the CppGenerator, not for generating C++ output.
	 * It mirrors the logic in SemanticAnalyzer's getQualifiedNameFromExpression.
	 *
	 * @param expression The expression node representing the name.
	 * @return The dot-separated FQN string, or null if the expression type is unexpected.
	 */
	private String getQualifiedNameFromExpressionInternal(Expression expression)
	{
		if (expression instanceof IdentifierExpression)
		{
			return ((IdentifierExpression) expression).getName().getLexeme();
		}
		else if (expression instanceof DotExpression)
		{
			DotExpression dot = (DotExpression) expression;
			String leftPart = getQualifiedNameFromExpressionInternal(dot.getLeft());
			if (leftPart == null)
			{
				return null;
			}
			return leftPart + "." + dot.getMemberName().getLexeme();
		}
		return null;
	}

	/**
	 * REPLACEMENT for the original collectClassDependencies method.
	 * This version performs a full traversal of the class's AST to find all type dependencies.
	 * It can be configured to include or exclude "core" classes.
	 */
	private Set<String> collectClassDependencies(ClassDeclaration classDecl, boolean includeCoreClasses)
	{
		Set<String> dependencies = new HashSet<>();

		// 1. Superclass dependency
		if (currentClassSymbol != null && currentClassSymbol.getType().getSuperClassType() != null)
		{
			addTypeDependency(currentClassSymbol.getType().getSuperClassType(), dependencies, includeCoreClasses);
		}

		// 2. Field dependencies
		for (FieldDeclaration field : classDecl.getFields())
		{
			addTypeDependency(field.getResolvedType(), dependencies, includeCoreClasses);
			if (field.getInitializer() != null)
			{
				collectDependenciesFrom(field.getInitializer(), dependencies, includeCoreClasses);
			}
		}

		// 3. Constructor dependencies
		for (ConstructorDeclaration ctor : classDecl.getConstructors())
		{
			for (int i = 0; i < ctor.getParameters().size(); i += 2)
			{
				Token typeToken = ctor.getParameters().get(i);
				Type paramType = resolveTypeFromToken(typeToken);
				addTypeDependency(paramType, dependencies, includeCoreClasses);
			}
			if (ctor.getBody() != null)
			{
				collectDependenciesFrom(ctor.getBody(), dependencies, includeCoreClasses);
			}
			if (ctor.getChainingCall() != null)
			{
				collectDependenciesFrom(ctor.getChainingCall(), dependencies, includeCoreClasses);
			}
		}

		// 4. Method dependencies
		for (MethodDeclaration method : classDecl.getMethods())
		{
			MethodSymbol methodSymbol = method.getResolvedSymbol();
			if (methodSymbol == null)
			{
				continue;
			}

			addTypeDependency(methodSymbol.getType(), dependencies, includeCoreClasses);

			if (methodSymbol.getParameterTypes() != null)
			{
				for (Type paramType : methodSymbol.getParameterTypes())
				{
					addTypeDependency(paramType, dependencies, includeCoreClasses);
				}
			}

			if (method.getBody() != null)
			{
				collectDependenciesFrom(method.getBody(), dependencies, includeCoreClasses);
			}
		}

		return dependencies;
	}

	private boolean isCoreClass(String fqcn)
	{
		return fqcn.startsWith("nebula.core.") || fqcn.startsWith("nebula.io.");
	}

	/**
	 * Adds a type to the dependency set if it's a user-defined class.
	 * It unwraps array types to find the base element type.
	 *
	 * @param type         The type to check.
	 * @param dependencies The set of dependency FQNs.
	 */
	private void addTypeDependency(Type type, Set<String> dependencies, boolean includeCoreClasses)
	{
		if (type == null)
		{
			return;
		}

		while (type instanceof ArrayType)
		{
			type = ((ArrayType) type).getElementType();
		}

		if (type instanceof ClassType)
		{
			String fqn = ((ClassType) type).getFqn();
			if (fqn != null && !fqn.equals(currentClassSymbol.getFqn()))
			{
				if (includeCoreClasses || !isCoreClass(fqn))
				{
					dependencies.add(fqn);
				}
			}
		}
	}

	/**
	 * Recursively traverses a Statement AST node to find all type dependencies.
	 *
	 * @param stmt               The statement to traverse.
	 * @param dependencies       The set to add dependencies to.
	 * @param includeCoreClasses A flag to determine whether to include core classes (e.g., Object, String) as dependencies.
	 */
	private void collectDependenciesFrom(Statement stmt, Set<String> dependencies, boolean includeCoreClasses)
	{
		if (stmt == null)
		{
			return;
		}

		if (stmt instanceof BlockStatement)
		{
			for (Statement s : ((BlockStatement) stmt).getStatements())
			{
				collectDependenciesFrom(s, dependencies, includeCoreClasses);
			}
		}
		else if (stmt instanceof ExpressionStatement)
		{
			collectDependenciesFrom(((ExpressionStatement) stmt).getExpression(), dependencies, includeCoreClasses);
		}
		else if (stmt instanceof IfStatement)
		{
			collectDependenciesFrom(((IfStatement) stmt).getCondition(), dependencies, includeCoreClasses);
			collectDependenciesFrom(((IfStatement) stmt).getThenBranch(), dependencies, includeCoreClasses);
			if (((IfStatement) stmt).getElseBranch() != null)
			{
				collectDependenciesFrom(((IfStatement) stmt).getElseBranch(), dependencies, includeCoreClasses);
			}
		}
		else if (stmt instanceof WhileStatement)
		{
			collectDependenciesFrom(((WhileStatement) stmt).getCondition(), dependencies, includeCoreClasses);
			collectDependenciesFrom(((WhileStatement) stmt).getBody(), dependencies, includeCoreClasses);
		}
		else if (stmt instanceof ForStatement)
		{
			if (((ForStatement) stmt).getInitializer() != null)
			{
				// The initializer can be a declaration or an expression.
				if (((ForStatement) stmt).getInitializer() instanceof Statement)
				{
					collectDependenciesFrom((Statement) ((ForStatement) stmt).getInitializer(), dependencies, includeCoreClasses);
				}
				else
				{
					collectDependenciesFrom((Expression) ((ForStatement) stmt).getInitializer(), dependencies, includeCoreClasses);
				}
			}
			if (((ForStatement) stmt).getCondition() != null)
			{
				collectDependenciesFrom(((ForStatement) stmt).getCondition(), dependencies, includeCoreClasses);
			}
			if (((ForStatement) stmt).getIncrement() != null)
			{
				collectDependenciesFrom(((ForStatement) stmt).getIncrement(), dependencies, includeCoreClasses);
			}
			collectDependenciesFrom(((ForStatement) stmt).getBody(), dependencies, includeCoreClasses);
		}
		else if (stmt instanceof ReturnStatement)
		{
			if (((ReturnStatement) stmt).getValue() != null)
			{
				collectDependenciesFrom(((ReturnStatement) stmt).getValue(), dependencies, includeCoreClasses);
			}
		}
		else if (stmt instanceof VariableDeclarationStatement)
		{
			Type varType = resolveTypeFromToken(((VariableDeclarationStatement) stmt).getTypeToken(), ((VariableDeclarationStatement) stmt).getArrayRank());
			addTypeDependency(varType, dependencies, includeCoreClasses);
			if (((VariableDeclarationStatement) stmt).getInitializer() != null)
			{
				collectDependenciesFrom(((VariableDeclarationStatement) stmt).getInitializer(), dependencies, includeCoreClasses);
			}
		}
		else if (stmt instanceof SwitchStatement)
		{
			collectDependenciesFrom(((SwitchStatement) stmt).getSwitchExpression(), dependencies, includeCoreClasses);
			for (SwitchCase sc : ((SwitchStatement) stmt).getCases())
			{
				collectDependenciesFrom(sc.getValue(), dependencies, includeCoreClasses);
				for (Statement s : sc.getBody())
				{
					collectDependenciesFrom(s, dependencies, includeCoreClasses);
				}
			}
			if (((SwitchStatement) stmt).getDefaultBlock() != null)
			{
				collectDependenciesFrom(((SwitchStatement) stmt).getDefaultBlock(), dependencies, includeCoreClasses);
			}
		}
		else if (stmt instanceof ConstructorChainingCallStatement)
		{
			for (Expression arg : ((ConstructorChainingCallStatement) stmt).getArguments())
			{
				collectDependenciesFrom(arg, dependencies, includeCoreClasses);
			}
		}
	}

	/**
	 * Recursively traverses an Expression AST node to find all type dependencies.
	 *
	 * @param expr               The expression to traverse.
	 * @param dependencies       The set to add dependencies to.
	 * @param includeCoreClasses A flag to determine whether to include core classes as dependencies.
	 */
	private void collectDependenciesFrom(Expression expr, Set<String> dependencies, boolean includeCoreClasses)
	{
		if (expr == null)
		{
			return;
		}

		// Every expression node should have its type resolved by the semantic analyzer.
		// This is the most reliable source for dependencies.
		addTypeDependency(expr.getResolvedType(), dependencies, includeCoreClasses);

		// Recurse into sub-expressions.
		if (expr instanceof BinaryExpression)
		{
			collectDependenciesFrom(((BinaryExpression) expr).getLeft(), dependencies, includeCoreClasses);
			collectDependenciesFrom(((BinaryExpression) expr).getRight(), dependencies, includeCoreClasses);
		}
		else if (expr instanceof UnaryExpression)
		{
			collectDependenciesFrom(((UnaryExpression) expr).getRight(), dependencies, includeCoreClasses);
		}
		else if (expr instanceof PostfixUnaryExpression)
		{
			collectDependenciesFrom(((PostfixUnaryExpression) expr).getOperand(), dependencies, includeCoreClasses);
		}
		else if (expr instanceof CallExpression)
		{
			collectDependenciesFrom(((CallExpression) expr).getCallee(), dependencies, includeCoreClasses);
			for (Expression arg : ((CallExpression) expr).getArguments())
			{
				collectDependenciesFrom(arg, dependencies, includeCoreClasses);
			}
		}
		else if (expr instanceof DotExpression)
		{
			collectDependenciesFrom(((DotExpression) expr).getLeft(), dependencies, includeCoreClasses);
		}
		else if (expr instanceof AssignmentExpression)
		{
			collectDependenciesFrom(((AssignmentExpression) expr).getTarget(), dependencies, includeCoreClasses);
			collectDependenciesFrom(((AssignmentExpression) expr).getValue(), dependencies, includeCoreClasses);
		}
		else if (expr instanceof NewExpression)
		{
			for (Expression arg : ((NewExpression) expr).getArguments())
			{
				collectDependenciesFrom(arg, dependencies, includeCoreClasses);
			}
		}
		else if (expr instanceof ArrayCreationExpression)
		{
			collectDependenciesFrom(((ArrayCreationExpression) expr).getSizeExpression(), dependencies, includeCoreClasses);
		}
		else if (expr instanceof ArrayInitializerExpression)
		{
			for (Expression element : ((ArrayInitializerExpression) expr).getElements())
			{
				collectDependenciesFrom(element, dependencies, includeCoreClasses);
			}
		}
		else if (expr instanceof ArrayAccessExpression)
		{
			collectDependenciesFrom(((ArrayAccessExpression) expr).getArray(), dependencies, includeCoreClasses);
			collectDependenciesFrom(((ArrayAccessExpression) expr).getIndex(), dependencies, includeCoreClasses);
		}
		else if (expr instanceof GroupingExpression)
		{
			collectDependenciesFrom(((GroupingExpression) expr).getExpression(), dependencies, includeCoreClasses);
		}
		else if (expr instanceof IsExpression)
		{
			collectDependenciesFrom(((IsExpression) expr).getLeft(), dependencies, includeCoreClasses);
			Type targetType = resolveTypeFromToken(((IsExpression) expr).getTypeToken());
			addTypeDependency(targetType, dependencies, includeCoreClasses);
		}
	}

	/**
	 * Processes the C++ target string of a wrapper method, replacing placeholders
	 * like ${paramName} with the actual parameter names.
	 *
	 * @param method          The MethodSymbol of the wrapper method.
	 * @param parameterTokens The list of parameter tokens from the AST node.
	 * @return The processed C++ code for the method's body.
	 */
	private String processWrapperBody(MethodSymbol method, List<Token> parameterTokens)
	{
		String body = method.getCppTarget();
		if (body == null)
		{
			return "";
		}

		for (int i = 0; i < parameterTokens.size(); i += 2)
		{
			Token paramNameToken = parameterTokens.get(i + 1);
			String paramName = paramNameToken.getLexeme();
			String placeholder = "${" + paramName + "}";
			body = body.replace(placeholder, paramName);
		}

		// +++ FIX: Replace direct member access with a call to the public raw() method. +++
		// This handles cases like `...->toString()->_data` and `anyString->_data`
		body = body.replace("->_data", "->raw()");

		return body;
	}

	/**
	 * Generates C++ code to create a high-precision nebula::core::String from a primitive.
	 * This bypasses the low-precision std::to_string() function.
	 *
	 * @param expr The expression representing the primitive value.
	 * @return A string of C++ code that produces a shared_ptr to a String.
	 */
	private String generateHighPrecisionString(Expression expr)
	{
		String exprCode = expr.accept(this);
		Type type = expr.getResolvedType();

		// Check if the type is a floating-point number.
		if (type.equals(PrimitiveType.FLOAT) || type.equals(PrimitiveType.DOUBLE))
		{
			String cppType = toCppType(type);
			// Use std::stringstream with max_digits10 to preserve precision.
			return """
					([&]() {
					    std::stringstream ss;
					    ss << std::setprecision(std::numeric_limits<""" + cppType + """
					>::max_digits10) << """ + exprCode + """
					    ;
					    return std::make_shared<nebula::core::String>(ss.str());
					})()""";
		}
		// Fallback for other primitives
		return "std::make_shared<nebula::core::String>(std::to_string(" + exprCode + "))";
	}

	private String escapeCppString(String rawString)
	{
		if (rawString == null)
		{
			return "";
		}
		StringBuilder escaped = new StringBuilder();
		for (int i = 0; i < rawString.length(); i++)
		{
			char c = rawString.charAt(i);
			if (c == '\\')
			{
				// Look ahead to check if this is an escaped quote
				if (i + 1 < rawString.length() && rawString.charAt(i + 1) == '"')
				{
					// It's a \" escape, so we should keep it as just \" in the C++ output
					escaped.append("\\\"");
					i++; // Skip the next character as it's part of this escape sequence
				}
				else
				{
					// It's a standalone backslash, so escape it
					escaped.append("\\\\");
				}
			}
			else if (c == '"')
			{
				// It's a literal double quote that needs to be escaped
				escaped.append("\\\"");
			}
			else if (c == '\n')
			{
				escaped.append("\\n");
			}
			else if (c == '\t')
			{
				escaped.append("\\t");
			}
			else if (c == '\r')
			{
				escaped.append("\\r");
			}
			else
			{
				escaped.append(c);
			}
		}
		return escaped.toString();
	}

	private boolean isNebulaString(Type type)
	{
		return type instanceof ClassType && ((ClassType) type).getFqn().equals("nebula.core.String");
	}
}