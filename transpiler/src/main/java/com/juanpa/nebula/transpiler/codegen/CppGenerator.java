// File: src/main/java/com/juanpa/nebula/transpiler/codegen/CppGenerator.java
package com.juanpa.nebula.transpiler.codegen;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.ast.declarations.*; // Ensure ParameterDeclaration is here
import com.juanpa.nebula.transpiler.ast.expressions.*;
import com.juanpa.nebula.transpiler.ast.statements.*;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType;
import com.juanpa.nebula.transpiler.semantics.*; // Import all semantic types (including VariableSymbol as it represents fields)

import java.util.*;
import java.util.stream.Collectors;

/**
 * CppGenerator is responsible for traversing the Abstract Syntax Tree (AST)
 * and generating corresponding C++ source code. It implements the ASTVisitor
 * interface, where each visit method returns a String representing the C++
 * code for that AST node.
 * <p>
 * It relies heavily on the semantic analysis phase to ensure type correctness
 * and symbol information through resolved symbols on AST nodes.
 * This version generates C++ code for each class into separate `.h` and `.cpp` strings,
 * returned in a Map. It also conditionally generates a `main.cpp` based on a detected
 * main entry point.
 */
public class CppGenerator implements ASTVisitor<String>
{
	private StringBuilder currentClassCodeBuilder;
	private StringBuilder currentHeaderCodeBuilder;

	SemanticAnalyzer semanticAnalyzer;

	private int indentLevel = 0;

	private ClassSymbol currentClassSymbol;

	// Changed to use the FQN directly, which is built up during namespace traversal.
	private String currentNamespacePrefix;

	private final Map<String, String> generatedClassCodeMap;
	private final Map<String, ClassSymbol> declaredClasses;

	private void error(Token token, String message)
	{
		System.err.println("Code Generation Error at " + token.getLine() + ":" + token.getColumn() + ": " + message);
	}

	public CppGenerator(Map<String, ClassSymbol> declaredClasses, SemanticAnalyzer semanticAnalyzer)
	{
		this.declaredClasses = declaredClasses; // Initialize the new field
		this.currentClassCodeBuilder = null;
		this.currentHeaderCodeBuilder = null;
		this.currentClassSymbol = null;
		this.currentNamespacePrefix = "";
		this.generatedClassCodeMap = new HashMap<>();

		this.semanticAnalyzer = semanticAnalyzer;
	}

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

	/**
	 * *** NEW ***
	 * Resolves a type from a token and array rank, necessary because the generator
	 * doesn't have access to the fully resolved types on VariableDeclarationStatements.
	 */
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


	private String toCppType(Type type)
	{
		if (type instanceof PrimitiveType)
		{
			// Handle explicit-width primitives first for clarity and precision
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
			if (type.equals(PrimitiveType.INT8))
			{
				return "int8_t";
			}
			if (type.equals(PrimitiveType.INT16))
			{
				return "int16_t";
			}
			if (type.equals(PrimitiveType.INT32))
			{
				return "int32_t";
			}
			if (type.equals(PrimitiveType.INT64))
			{
				return "int64_t";
			}
			if (type.equals(PrimitiveType.UINT8))
			{
				return "uint8_t";
			}
			if (type.equals(PrimitiveType.UINT16))
			{
				return "uint16_t";
			}
			if (type.equals(PrimitiveType.UINT32))
			{
				return "uint32_t";
			}
			if (type.equals(PrimitiveType.UINT64))
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
			// Handle aliases by mapping them to their explicit-width types
			if (type.equals(PrimitiveType.BYTE))
			{
				return "int8_t";
			}
			if (type.equals(PrimitiveType.SHORT))
			{
				return "int16_t";
			}
			if (type.equals(PrimitiveType.INT))
			{
				return "int32_t";
			}
			if (type.equals(PrimitiveType.LONG))
			{
				return "int64_t";
			}
			if (type.equals(PrimitiveType.UBYTE))
			{
				return "uint8_t";
			}
			if (type.equals(PrimitiveType.USHORT))
			{
				return "uint16_t";
			}
			if (type.equals(PrimitiveType.UINT))
			{
				return "uint32_t";
			}
			if (type.equals(PrimitiveType.ULONG))
			{
				return "uint64_t";
			}
		}
		else if (type instanceof ClassType)
		{
			ClassType classType = (ClassType) type;
			// For all Nebula classes (including String and Object), use shared_ptr
			String cppFqn = classType.getFqn().replace(".", "::");
			return "std::shared_ptr<" + cppFqn + ">";
		}
		else if (type instanceof ArrayType)
		{
			// All arrays are treated as nullable shared_ptrs to a vector.
			ArrayType arrayType = (ArrayType) type;
			return "std::shared_ptr<std::vector<" + toCppType(arrayType.getElementType()) + ">>";
		}
		else if (type instanceof NullType)
		{
			return "decltype(nullptr)"; // Represents the type of 'nullptr'
		}
		else if (type instanceof ErrorType)
		{
			return "/* ERROR_TYPE */ void*"; // Placeholder for unresolved types
		}
		return "void*"; // Default fallback
	}

	// Helper to get the simple class name from a fully qualified name
	private String getSimpleClassName(String fqn)
	{
		int lastDot = fqn.lastIndexOf('.');
		return lastDot == -1 ? fqn : fqn.substring(lastDot + 1);
	}

	/**
	 * Formats a list of parameter tokens into a C++ parameter string.
	 * Parameters are stored as a flat list of alternating type and name tokens.
	 * E.g., [TokenType.INT, Token("x"), TokenType.STRING_KEYWORD, Token("s")]
	 *
	 * @param parameters A list of Token objects representing parameter types and names.
	 * @return A string formatted for C++ method/constructor parameters.
	 */
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

		String inheritance = "";
		if (currentClassSymbol.getType().getSuperClassType() instanceof ClassType)
		{
			ClassType superClassType = (ClassType) currentClassSymbol.getType().getSuperClassType();
			inheritance = " : public " + superClassType.getFqn().replace(".", "::");
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

				// Only add 'const' if the method is NOT static.
				String constQualifier = (!methodSymbol.isStatic()) ? " const" : "";

				appendHeaderLine(staticQualifier + returnType + " " + methodName + "(" + formatCppParameters(method.getParameters()) + ")" + constQualifier + ";");
			}
		}

		if (currentClassSymbol.isNative() && "nebula.core.String".equals(fqn))
		{
			appendHeaderLine("\n\t// Special constructor for wrapping std::string");
			appendHeaderLine("\tString(const std::string& raw_str);");
			appendHeaderLine("\n\t// Accessor for the raw C++ string data");
			appendHeaderLine("\tconst std::string& raw() const;");
			dedent();
			appendHeaderLine("private:");
			indent();
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
		if (declaration.getInitializer() != null)
		{
			return declaration.getInitializer().accept(this);
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

	@Override
	public String visitReturnStatement(ReturnStatement statement)
	{
		if (statement.getValue() != null)
		{
			return "return " + statement.getValue().accept(this) + ";";
		}
		return "return;";
	}

	@Override
	public String visitVariableDeclarationStatement(VariableDeclarationStatement statement)
	{
		// Use the new helper to resolve the full type, including arrays.
		Type varType = resolveTypeFromToken(statement.getTypeToken(), statement.getArrayRank());
		String cppType = statement.getTypeToken().getType() == TokenType.VAR ? "auto" : toCppType(varType);

		String declaration = cppType + " " + statement.getName().getLexeme();
		if (statement.getInitializer() != null)
		{
			// New logic: Check for empty array initializer.
			if (statement.getInitializer() instanceof ArrayInitializerExpression)
			{
				ArrayInitializerExpression arrayExpr = (ArrayInitializerExpression) statement.getInitializer();
				if (arrayExpr.getElements().isEmpty())
				{
					// Special case: An empty array initializer should be nullptr.
					declaration += " = nullptr";
				}
				else
				{
					// Non-empty array initializer, visit normally.
					declaration += " = " + statement.getInitializer().accept(this);
				}
			}
			else
			{
				// Other initializers (e.g., new expression, null literal, etc.)
				declaration += " = " + statement.getInitializer().accept(this);
			}
		}
		else if (varType instanceof ClassType || varType instanceof ArrayType)
		{
			// Default-initialize shared_ptrs to nullptr.
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

	/**
	 * Visits an array creation expression (e.g., `new int[10]`) and generates
	 * the C++ equivalent for a std::vector, like `std::vector<int>(10)`.
	 */
	@Override
	public String visitArrayCreationExpression(ArrayCreationExpression expression)
	{
		String elementType = toCppType(expression.getResolvedType());
		String sizeExpr = expression.getSizeExpression().accept(this);
		return "std::make_shared<std::vector<" + elementType + ">>(" + sizeExpr + ")";
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

	private void generateObjectCpp(ClassDeclaration declaration)
	{
		String simpleName = currentClassSymbol.getName();
		// Default constructor definition for Object
		//appendLine(simpleName + "::" + simpleName + "() { } \n");

		// Ensure const qualifier is also in implementation
		appendLine("bool " + simpleName + "::operator==(const std::shared_ptr<Object>& other) const {");
		indent();
		appendLine("return this == other.get();"); // Pointer comparison for Object
		dedent();
		appendLine("}\n");

		appendLine("std::shared_ptr<String> " + simpleName + "::hashCode() const {");
		indent();
		appendLine("std::stringstream ss;");
		appendLine("ss << static_cast<const void*>(this);");
		appendLine("return std::make_shared<String>(ss.str());");
		dedent();
		appendLine("}\n");

		appendLine("std::shared_ptr<String> " + simpleName + "::toString() const {");
		indent();
		appendLine("return this->hashCode();");
		dedent();
		appendLine("}\n");
	}

	private void generateStringCpp(ClassDeclaration declaration)
	{
		String simpleName = currentClassSymbol.getName();

		appendLine(simpleName + "::String() : _data(\"\") {}\n"); // Default constructor
		appendLine(simpleName + "::String(const std::string& raw_str) : _data(raw_str) {}\n"); // std::string constructor

		// Special copy constructor: String(const std::shared_ptr<nebula::core::String>& other)
		appendLine(simpleName + "::String(const std::shared_ptr<nebula::core::String>& other) : _data(other ? other->raw() : \"\") { /* User constructor logic */ }\n");


		appendLine("int " + simpleName + "::length() const {");
		indent();
		appendLine("return static_cast<int>(_data.length());");
		dedent();
		appendLine("}\n");

		appendLine("std::shared_ptr<String> " + simpleName + "::operator+(const std::shared_ptr<String>& other) {");
		indent();
		appendLine("return std::make_shared<String>(this->_data + (other ? other->raw() : \"\"));");
		dedent();
		appendLine("}\n");

		// Override of Object::operator==
		appendLine("bool " + simpleName + "::operator==(const std::shared_ptr<Object>& other) const {");
		indent();
		appendLine("auto other_str = std::dynamic_pointer_cast<String>(other);");
		appendLine("if (other_str) { return this->_data == other_str->_data; }");
		appendLine("return false;"); // Different types or other is null
		dedent();
		appendLine("}\n");

		// Overload for String to String comparison
		appendLine("bool " + simpleName + "::operator==(const std::shared_ptr<String>& other) const {");
		indent();
		appendLine("if (other) { return this->_data == other->_data; }");
		appendLine("return false;"); // Comparing to nullptr
		dedent();
		appendLine("}\n");


		appendLine("std::shared_ptr<String> " + simpleName + "::toString() const {");
		indent();
		appendLine("return std::make_shared<String>(this->_data);");
		dedent();
		appendLine("}\n");

		appendLine("std::shared_ptr<String> " + simpleName + "::hashCode() const {");
		indent();
		appendLine("return std::make_shared<String>(std::to_string(std::hash<std::string>{}(this->_data)));");
		dedent();
		appendLine("}\n");

		appendLine("const std::string& " + simpleName + "::raw() const {");
		indent();
		appendLine("return _data;");
		dedent();
		appendLine("}\n");
	}

	private void generateConsoleCpp(ClassDeclaration declaration)
	{
		String simpleName = currentClassSymbol.getName();
		for (MethodDeclaration method : declaration.getMethods())
		{
			String methodName = method.getName().getLexeme();
			String params = formatCppParameters(method.getParameters());
			// Use fully qualified namespace for Console method definitions
			String fullMethodName = "void " + currentNamespacePrefix.replace(".", "::") + "::" + simpleName + "::" + methodName;
			appendLine(fullMethodName + "(" + params + ") {");
			indent();

			// Assume parameters list has at least 2 tokens (type, name)
			// The original Console.neb had parameters like (Object anything), (string anyString)
			// so the second token is the actual parameter name.
			// This assumes single-parameter print/println methods.
			if (!method.getParameters().isEmpty())
			{
				Token paramNameToken = method.getParameters().get(1); // Parameter name is the second token
				Token paramTypeToken = method.getParameters().get(0); // Parameter type is the first token
				String paramName = paramNameToken.getLexeme();
				Type paramType = resolveTypeFromToken(paramTypeToken);


				if (paramType instanceof ClassType)
				{
					// Use the raw() method for printing Nebula String/Object content
					appendLine("if (" + paramName + ") { std::cout << " + paramName + "->toString()->raw(); } else { std::cout << \"null\"; }");
				}
				else if (paramType.equals(PrimitiveType.BOOL))
				{
					// Use std::boolalpha for cleaner boolean output (true/false)
					appendLine("std::cout << std::boolalpha << " + paramName + ";");
				}
				else
				{
					// For other primitives, direct print is fine
					appendLine("std::cout << " + paramName + ";");
				}
			}
			else
			{
				// Handle parameterless print/println if they were ever added
				appendLine("// No parameters to print");
			}


			if (methodName.startsWith("println"))
			{
				appendLine("std::cout << std::endl;");
			}

			dedent();
			appendLine("}\n");
		}
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
				String params = formatCppParameters(ctor.getParameters());
				String ctorSignature = simpleName + "::" + simpleName + "(" + params + ")";

				// +++ FIX: Differentiate between String constructors +++
				if ("nebula.core.String".equals(fqn))
				{
					if (ctor.getParameters().isEmpty())
					{
						// Default constructor: String()
						ctorSignature += " : _data(\"\")";
					}
					else
					{
						// Copy constructor: String(String other)
						ctorSignature += " : _data(other ? other->raw() : \"\")";
					}
				}
				// +++ END OF FIX +++

				appendLine(ctorSignature + " {");
				indent();
				if (ctor.getBody() != null)
				{
					ctor.getBody().accept(this);
				}
				dedent();
				appendLine("}\n");
			}
		}

		if (isNative && "nebula.core.String".equals(fqn))
		{
			appendLine(simpleName + "::String(const std::string& raw_str) : _data(raw_str) {}\n");
			appendLine("const std::string& " + simpleName + "::raw() const {");
			indent();
			appendLine("return _data;");
			dedent();
			appendLine("}\n");
		}

		// 3. Methods
		for (MethodDeclaration method : declaration.getMethods())
		{
			MethodSymbol methodSymbol = method.getResolvedSymbol();
			if (methodSymbol == null)
			{
				continue;
			}

			if (method.isWrapper())
			{
				String returnType = toCppType(methodSymbol.getType());
				String methodName = methodSymbol.isOperator() ? "operator" + methodSymbol.getName() : methodSymbol.getName();
				String params = formatCppParameters(method.getParameters());
				String constQualifier = !methodSymbol.isStatic() ? " const" : "";

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
				String constQualifier = !methodSymbol.isStatic() ? " const" : "";

				appendLine(returnType + " " + simpleName + "::" + methodName + "(" + params + ")" + constQualifier + " {");
				indent();
				if (method.getBody() != null)
				{
					method.getBody().accept(this);
				}
				dedent();
				appendLine("}\n");
			}
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

		// Handle null comparisons for reference types without dereferencing.
		if ((op.equals("==") || op.equals("!=")) && (leftType.isReferenceType() || rightType.isReferenceType()))
		{
			String cxxRight = right;
			if (rightType instanceof NullType)
			{
				cxxRight = "nullptr";
			}
			return "(" + left + " " + op + " " + cxxRight + ")";
		}

		// If the left side is a class or array type, it's a shared_ptr.
		// Member operators require the object to be dereferenced.
		if (leftType instanceof ClassType || leftType instanceof ArrayType)
		{
			// For any class type, dereference the left operand to access member operators.
			String leftOperand = "*" + left;

			// String concatenation requires special handling for the right operand,
			// which may need to be converted to a string.
			if (op.equals("+") && leftType instanceof ClassType && ((ClassType) leftType).getFqn().equals("nebula.core.String"))
			{
				String rightOperandForConcat = right;

				if (rightType instanceof PrimitiveType)
				{
					if (rightType.equals(PrimitiveType.INT) ||
							rightType.equals(PrimitiveType.FLOAT) ||
							rightType.equals(PrimitiveType.DOUBLE))
					{
						// For numeric primitives, use std::to_string
						rightOperandForConcat = "std::make_shared<nebula::core::String>(std::to_string(" + right + "))";
					}
					else if (rightType.equals(PrimitiveType.BOOL))
					{
						// For boolean, convert to "true" or "false" string
						rightOperandForConcat = "std::make_shared<nebula::core::String>(" + right + " ? \"true\" : \"false\")";
					}
					else if (rightType.equals(PrimitiveType.CHAR) ||
							rightType.equals(PrimitiveType.BYTE))
					{
						// For char/byte, convert to string containing single character
						rightOperandForConcat = "std::make_shared<nebula::core::String>(std::string(1, " + right + "))";
					}
				}
				else if (rightType instanceof ClassType)
				{
					// If the right operand is another Nebula class, call its toString() method.
					if (!((ClassType) rightType).getFqn().equals("nebula.core.String"))
					{
						rightOperandForConcat = right + "->toString()";
					}
				}

				return "(*" + left + " + " + rightOperandForConcat + ")";
			}

			// For all other class types and operators, use the dereferenced
			// left operand. The right operand is assumed to be of a compatible type.
			return "(" + leftOperand + " " + op + " " + right + ")";
		}

		// Default binary expression handling for primitives (e.g., int + int)
		return "(" + left + " " + op + " " + right + ")";
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
			// Ensure string literals are wrapped in std::make_shared<nebula::core::String>
			return "std::make_shared<nebula::core::String>(\"" + value.toString().replace("\"", "\\\"") + "\")";
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
		String target = expression.getTarget().accept(this);
		String value = expression.getValue().accept(this);
		String op = expression.getOperator().getLexeme();

		Type targetType = expression.getTarget().getResolvedType();

		// Check for a compound assignment operator (e.g., "+=", "-=") on a class type.
		if (targetType instanceof ClassType && op.length() > 1 && op.endsWith("="))
		{
			// Transform "a += b" into "a = (*a) + b".
			// This makes compound assignment syntactic sugar for the binary operator.
			String binaryOp = op.substring(0, op.length() - 1); // Extract "+" from "+="

			// The expression `(*target + value)` will be handled by your now-fixed
			// visitBinaryExpression, which correctly dereferences the object.
			return target + " = (*" + target + " " + binaryOp + " " + value + ")";
		}

		// Default behavior for primitives or simple assignment ("=").
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

		// NEW: Reconstruct the call to use the mangled name
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
				calleeString = "this->" + methodName;
			}
		}
		else if (expr.getCallee() instanceof DotExpression)
		{
			DotExpression dot = (DotExpression) expr.getCallee();
			String left = dot.getLeft().accept(this);
			String separator = resolvedMethod.isStatic() ? "::" : "->";
			calleeString = left + separator + methodName;
		}
		else
		{
			calleeString = expr.getCallee().accept(this); // Fallback
		}

		return calleeString + "(" + args + ")";
	}

	@Override
	public String visitDotExpression(DotExpression expr)
	{
		String left = expr.getLeft().accept(this);
		String memberName = expr.getMemberName().getLexeme();
		Symbol symbol = expr.getResolvedSymbol();
		Type leftType = expr.getLeft().getResolvedType();

		// +++ THIS IS THE FIX +++
		// When accessing '.size' on an array, cast the result to an int.
		if (leftType instanceof ArrayType)
		{
			if (memberName.equals("size"))
			{
				// Cast to int to resolve ambiguity with C++ function overloads.
				return "static_cast<int>(" + left + ".size())";
			}
		}

		if (symbol instanceof MethodSymbol && ((MethodSymbol) symbol).isStatic())
		{
			return left + "::" + memberName;
		}
		else if (symbol instanceof VariableSymbol && ((VariableSymbol) symbol).isStatic())
		{
			return left + "::" + memberName;
		}

		// For regular member access, it's a shared_ptr, so use ->
		return left + "->" + memberName;
	}

	@Override
	public String visitThisExpression(ThisExpression expression)
	{
		// 'this' in Nebula maps to 'this' in C++ when accessing instance members
		return "this";
	}

	@Override
	public String visitNewExpression(NewExpression expression)
	{
		Type classType = expression.getResolvedType();

		if (classType == null || !(classType instanceof ClassType))
		{
			error(expression.getClassName().getFirstToken(), "Could not resolve class for 'new' expression (resolved type is null or not a ClassType).");
			return "/* ERROR: Could not resolve class for new expression */ nullptr";
		}
		ClassType resolvedClassType = (ClassType) classType;

		String cppFqn = resolvedClassType.getFqn().replace(".", "::");

		List<String> argCodes = expression.getArguments().stream()
				.map(arg -> arg.accept(this))
				.collect(Collectors.toList());

		return "std::make_shared<" + cppFqn + ">(" + String.join(", ", argCodes) + ")";
	}

	@Override
	public String visitPostfixUnaryExpression(PostfixUnaryExpression expression)
	{
		String operandCode = expression.getOperand().accept(this);
		String operator = expression.getOperator().getLexeme();

		// For postfix increment/decrement, the operator comes after the operand
		return operandCode + operator;
	}

	// In CppGenerator.java

	/**
	 * Visits an array access expression (e.g., `myArray[i]`) and generates
	 * the C++ equivalent `myArray[i]`.
	 */
	@Override
	public String visitArrayAccessExpression(ArrayAccessExpression expression)
	{
		String arrayExpr = expression.getArray().accept(this);
		String indexExpr = expression.getIndex().accept(this);
		// Handle access to the raw vector through the shared_ptr.
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
	 * This version performs a full traversal of the class's AST to find all dependencies.
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
}