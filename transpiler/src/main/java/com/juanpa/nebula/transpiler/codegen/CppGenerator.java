// File: src/main/java/com/juanpa.nebula.transpiler/codegen/CppGenerator.java

package com.juanpa.nebula.transpiler.codegen;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.ast.declarations.*; // Ensure ParameterDeclaration is here
import com.juanpa.nebula.transpiler.ast.expressions.*;
import com.juanpa.nebula.transpiler.ast.statements.*;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.lexer.TokenType;
import com.juanpa.nebula.transpiler.semantics.*; // Import all semantic types (including VariableSymbol as it represents fields)

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
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

	private int indentLevel = 0;

	private ClassSymbol currentClassSymbol;
	private MethodSymbol currentMethodSymbol; // Keep track of current method for return type lookup
	private boolean inStaticContext; // Not explicitly used but kept from original structure

	// Changed to use the FQN directly, which is built up during namespace traversal.
	private String currentNamespacePrefix;

	private final Map<String, String> generatedClassCodeMap;
	private final Map<String, ClassSymbol> declaredClasses;

	private void error(Token token, String message)
	{
		System.err.println("Code Generation Error at " + token.getLine() + ":" + token.getColumn() + ": " + message);
	}

	public CppGenerator(Map<String, ClassSymbol> declaredClasses)
	{
		this.declaredClasses = declaredClasses; // Initialize the new field
		this.currentClassCodeBuilder = null;
		this.currentHeaderCodeBuilder = null;
		this.currentClassSymbol = null;
		this.currentMethodSymbol = null;
		this.inStaticContext = false;
		this.currentNamespacePrefix = "";
		this.generatedClassCodeMap = new HashMap<>();
	}

	public Map<String, String> generate(Program program)
	{
		// First pass to populate the map for classes and headers (and resolve symbols if applicable)
		for(NamespaceDeclaration namespaceDecl : program.getNamespaceDeclarations())
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

		if(mainClassOpt.isPresent())
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
		if(currentClassCodeBuilder == null)
		{
			System.err.println("Error: currentClassCodeBuilder is null. This should be initialized per class in visitClassDeclaration.");
			return;
		}
		for(int i = 0; i < indentLevel; i++)
		{
			currentClassCodeBuilder.append("	");
		}
		currentClassCodeBuilder.append(line).append("\n");
	}

	private void appendHeaderLine(String line)
	{
		if(currentHeaderCodeBuilder == null)
		{
			System.err.println("Error: currentHeaderCodeBuilder is null. This should be initialized per class in visitClassDeclaration.");
			return;
		}
		for(int i = 0; i < indentLevel; i++)
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
		if(indentLevel > 0)
		{
			indentLevel--;
		}
	}

	private Type resolveTypeFromToken(Token typeToken)
	{
		if(typeToken == null)
		{
			return ErrorType.INSTANCE;
		}

		// Handle primitive types directly
		switch(typeToken.getType())
		{
			case INT:
				return PrimitiveType.INT;
			case BOOL:
				return PrimitiveType.BOOL;
			case FLOAT:
				return PrimitiveType.FLOAT;
			case DOUBLE:
				return PrimitiveType.DOUBLE;
			case BYTE:
				return PrimitiveType.BYTE;
			case VOID:
				return PrimitiveType.VOID;
			case CHAR:
				return PrimitiveType.CHAR;
			case STRING_KEYWORD:
				// Special handling for 'string' keyword, mapping to Nebula's String class
				// Assumes that 'declaredClasses' contains the ClassSymbol for "nebula.core.String"
				ClassSymbol stringClass = declaredClasses.get("nebula.core.String");
				if(stringClass != null)
				{
					return new ClassType("nebula.core.String", stringClass);
				}
				else
				{
					error(typeToken, "String class (nebula.core.String) not found in declared classes.");
					return ErrorType.INSTANCE;
				}
			default:
				// For non-primitive types, attempt to resolve from declaredClasses.
				// First, try a direct lookup by the full lexeme (assuming it might be a FQN in some contexts)
				ClassSymbol byFqn = declaredClasses.get(typeToken.getLexeme());
				if(byFqn != null)
				{
					return byFqn.getType();
				}

				// If not found by direct FQN, iterate through declared classes to find a match by simple name
				// or by ending with the simple name (e.g., "Console" matches "Nebula.System.Console")
				String simpleName = typeToken.getLexeme(); // Corrected: use getLexeme()
				for(ClassSymbol classSymbol : declaredClasses.values())
				{
					if(classSymbol.getName().equals(simpleName) || classSymbol.getFqn().endsWith("." + simpleName))
					{
						return classSymbol.getType();
					}
				}

				// If still not found, return ErrorType.
				error(typeToken, "Could not resolve type '" + typeToken.getLexeme() + "' during C++ generation. Semantic analysis might have missed it or code generation context is insufficient.");
				return ErrorType.INSTANCE;
		}
	}


	private String toCppType(Type type)
	{
		if(type instanceof PrimitiveType)
		{
			if(type.equals(PrimitiveType.INT))
				return "int";
			if(type.equals(PrimitiveType.BOOL))
				return "bool";
			if(type.equals(PrimitiveType.FLOAT))
				return "float";
			if(type.equals(PrimitiveType.DOUBLE))
				return "double";
			if(type.equals(PrimitiveType.BYTE))
				return "char"; // Assuming byte maps to char in C++
			if(type.equals(PrimitiveType.CHAR))
				return "char";
			if(type.equals(PrimitiveType.VOID))
				return "void";
		}
		else if(type instanceof ClassType)
		{
			ClassType classType = (ClassType) type;
			// For all Nebula classes (including String and Object), use shared_ptr
			String cppFqn = classType.getFqn().replace(".", "::");
			return "std::shared_ptr<" + cppFqn + ">";
		}
		else if(type instanceof ArrayType)
		{
			// For arrays, use std::vector. If element is a class, it'll be shared_ptr.
			ArrayType arrayType = (ArrayType) type;
			return "std::vector<" + toCppType(arrayType.getElementType()) + ">";
		}
		else if(type instanceof NullType)
		{
			return "decltype(nullptr)"; // Represents the type of 'nullptr'
		}
		else if(type instanceof ErrorType)
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
		if(parameters.isEmpty())
		{
			return "";
		}
		StringBuilder sb = new StringBuilder();
		for(int i = 0; i < parameters.size(); i += 2) // Iterate by pairs (type, name)
		{
			Token paramTypeToken = parameters.get(i);
			Token paramNameToken = parameters.get(i + 1);

			Type paramType = resolveTypeFromToken(paramTypeToken);
			String cppType = toCppType(paramType);
			String paramName = paramNameToken.getLexeme();

			// For Nebula class types, pass shared_ptr by const reference to avoid copying smart pointers
			if(paramType instanceof ClassType)
			{
				sb.append("const ").append(cppType).append("& ").append(paramName);
			}
			else
			{
				sb.append(cppType).append(" ").append(paramName);
			}

			if(i < parameters.size() - 2) // Check if there are more parameter pairs
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

		for(ClassDeclaration classDecl : declaration.getClassDeclarations())
		{
			classDecl.accept(this);
		}

		this.currentNamespacePrefix = previousNamespacePrefix;
		return null;
	}


	@Override
	public String visitClassDeclaration(ClassDeclaration declaration)
	{
		currentClassCodeBuilder = new StringBuilder();
		currentHeaderCodeBuilder = new StringBuilder();
		indentLevel = 0;

		ClassSymbol resolvedClassSymbol = null;
		if(declaration.getNameExpression().getResolvedSymbol() instanceof ClassSymbol)
		{
			resolvedClassSymbol = (ClassSymbol) declaration.getNameExpression().getResolvedSymbol();
		}
		else
		{
			// Fallback if resolvedSymbol is not directly available, try lookup
			String simpleName = declaration.getName().getLexeme();
			String fullClassNameFromPrefix = currentNamespacePrefix.isEmpty() ? simpleName : currentNamespacePrefix + "." + simpleName;
			resolvedClassSymbol = declaredClasses.get(fullClassNameFromPrefix);
		}


		ClassSymbol previousClassSymbol = this.currentClassSymbol;
		this.currentClassSymbol = resolvedClassSymbol; // Set current class for symbol resolution within methods

		if(this.currentClassSymbol == null)
		{
			String className = declaration.getName().getLexeme();
			String errorFqn = currentNamespacePrefix.isEmpty() ? className : currentNamespacePrefix + "." + className;
			error(declaration.getName(), "Class symbol not resolved for class '" + errorFqn + "'. Skipping code generation for this class.");
			this.currentClassSymbol = previousClassSymbol; // Restore previous symbol before returning
			return null;
		}

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

		// Logic for includes based on namespace hierarchy
		// Calculate the number of directories to go up to reach the root 'out' directory
		int currentNamespaceDepth = currentNamespacePrefix.isEmpty() ? 0 : currentNamespacePrefix.split("\\.").length;
		String relativeUpPath = "../".repeat(currentNamespaceDepth);

		if(fqn.equals("nebula.core.Object"))
		{
			appendHeaderLine("namespace nebula { namespace core { class String; } } // Forward Declaration");
		}
		else if(fqn.equals("nebula.core.String"))
		{
			appendHeaderLine("#include \"Object.h\""); // String is in core, needs Object.h from same dir
		}
		else if(fqn.equals("nebula.io.Console"))
		{
			appendHeaderLine("#include \"../core/Object.h\""); // Console is in io, needs Object.h from ../core
			appendHeaderLine("#include \"../core/String.h\""); // Console is in io, needs String.h from ../core
		}
		else
		{ // For user-defined classes (e.g., Program.Test)
			appendHeaderLine("#include \"" + relativeUpPath + "nebula/core/Object.h\"");
			appendHeaderLine("#include \"" + relativeUpPath + "nebula/core/String.h\""); // User classes might implicitly use String (e.g. literals)
			appendHeaderLine("#include \"" + relativeUpPath + "nebula/io/Console.h\""); // User classes might implicitly use Console
		}
		appendHeaderLine("");

		String[] namespaceParts = currentNamespacePrefix.split("\\.");
		for(String ns : namespaceParts)
		{
			if(!ns.isEmpty())
			{ // Handle empty prefix for top-level classes
				appendHeaderLine("namespace " + ns + " {");
				indent();
			}
		}

		String inheritance = "";
		if(currentClassSymbol.getType().getSuperClassType() != null)
		{
			String superClassFqn = "";
			if(currentClassSymbol.getType().getSuperClassType() instanceof ClassType)
			{
				ClassType superClassType = (ClassType) currentClassSymbol.getType().getSuperClassType();
				if(superClassType.getClassSymbol() != null)
				{
					superClassFqn = superClassType.getFqn();
					inheritance = " : public " + superClassFqn.replace(".", "::");
				}
			}
		}

		appendHeaderLine("class " + simpleName + inheritance + " {");

		if(fqn.equals("nebula.core.String"))
		{
			appendHeaderLine("private:");
			indent();
			appendHeaderLine("std::string _data;");
			dedent();
		}

		appendHeaderLine("public:");
		indent();

		// Destructor
		appendHeaderLine("virtual ~" + simpleName + "() = default;");

		// Constructors
		// Handle specific constructors for String
		if(fqn.equals("nebula.core.String"))
		{
			appendHeaderLine(simpleName + "();"); // Default constructor
			appendHeaderLine(simpleName + "(const std::string& raw_str);"); // std::string constructor
			appendHeaderLine(simpleName + "(const std::shared_ptr<nebula::core::String>& other);"); // Copy constructor
		}
		else
		{
			// Generate default constructor for non-String/Object classes if no explicit ones are defined
			if(declaration.getConstructors().isEmpty())
			{
				// Only generate if it's not Object (Object's constructor will be explicitly defined)
				if(!fqn.equals("nebula.core.Object"))
				{
					appendHeaderLine(simpleName + "();");
				}
			}
		}

		for(ConstructorDeclaration ctor : declaration.getConstructors())
		{
			// Skip explicitly handled String constructors
			boolean isStringDefaultCtor = fqn.equals("nebula.core.String") && ctor.getParameters().isEmpty();
			boolean isStringRawStringCtor = fqn.equals("nebula.core.String") && ctor.getParameters().size() == 2 &&
					ctor.getParameters().get(0).getType() == TokenType.STRING_KEYWORD &&
					ctor.getParameters().get(1).getType() == TokenType.IDENTIFIER;
			// To avoid duplication, also skip if it's the shared_ptr copy constructor for String
			boolean isStringSharedPtrCopyCtor = fqn.equals("nebula.core.String") && ctor.getParameters().size() == 2 &&
					ctor.getParameters().get(0).getLexeme().equals("String") && // Assuming parameter type is 'String' keyword
					ctor.getParameters().get(1).getType() == TokenType.IDENTIFIER;

			if(!isStringDefaultCtor && !isStringRawStringCtor && !isStringSharedPtrCopyCtor)
			{
				appendHeaderLine(simpleName + "(" + formatCppParameters(ctor.getParameters()) + ");");
			}
		}


		// Methods
		for(MethodDeclaration method : declaration.getMethods())
		{
			boolean isStatic = method.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.STATIC);
			String modifiers = isStatic ? "static " : "";

			// Determine if a method is overriding a base class method
			boolean isOverride = false;
			if(!isStatic)
			{
				if(currentClassSymbol.getType().getSuperClassType() != null)
				{
					ClassType superClassType = (ClassType) currentClassSymbol.getType().getSuperClassType();
					if(superClassType.getClassSymbol() != null)
					{
						ClassSymbol superClass = (ClassSymbol) superClassType.getClassSymbol();
						Optional<MethodSymbol> overriddenMethod = superClass.methodsByName.values().stream()
								.flatMap(List::stream)
								.filter(m -> m.getName().equals(method.getName().getLexeme()) &&
										m.getParameterTypes().size() == method.getParameters().size() / 2) // Assuming MethodSymbol has isVirtual()
								.findFirst();
						if(overriddenMethod.isPresent())
						{
							isOverride = true;
						}
					}
				}
			}

			// Add virtual keyword if necessary
			if(!isStatic && (fqn.equals("nebula.core.Object") || isOverride))
			{
				modifiers += "virtual ";
			}

			String returnType = toCppType(resolveTypeFromToken(method.getReturnType()));
			String methodName = method.getName().getLexeme();
			if(method.getOperatorKeyword() != null)
				methodName = "operator" + methodName;

			String params = formatCppParameters(method.getParameters());

			boolean isConstMethod = !isStatic && List.of("length", "hashCode", "toString", "operator==").contains(methodName);
			String constQualifier = isConstMethod ? " const" : "";

			String overrideKeyword = isOverride ? " override" : "";

			if(fqn.equals("nebula.core.Object") && methodName.equals("operator=="))
			{
				appendHeaderLine(modifiers + "bool " + methodName + "(const std::shared_ptr<nebula::core::Object>& other)" + constQualifier + overrideKeyword + ";");
			}
			else if(fqn.equals("nebula.core.String") && methodName.equals("operator=="))
			{
				appendHeaderLine(modifiers + "bool " + methodName + "(const std::shared_ptr<nebula::core::Object>& other)" + constQualifier + overrideKeyword + ";");
				appendHeaderLine("bool " + methodName + "(const std::shared_ptr<nebula::core::String>& other)" + constQualifier + ";");
			}
			else
			{
				appendHeaderLine(modifiers + returnType + " " + methodName + "(" + params + ")" + constQualifier + overrideKeyword + ";");
			}
		}

		if(fqn.equals("nebula.core.String"))
		{
			appendHeaderLine("const std::string& raw() const;");
		}

		dedent();
		appendHeaderLine("}; // class " + simpleName);

		for(int i = namespaceParts.length - 1; i >= 0; i--)
		{
			if(!namespaceParts[i].isEmpty())
			{
				dedent();
				appendHeaderLine("} // namespace " + namespaceParts[i]);
			}
		}

		appendHeaderLine("\n#endif // " + headerGuard);
		generatedClassCodeMap.put(fqn.replace('.', '/') + ".h", currentHeaderCodeBuilder.toString());

		// --- Generate Source (.cpp) ---
		appendLine("// " + fqn.replace('.', '/') + ".cpp");
		appendLine("#include \"" + simpleName + ".h\""); // Current class header
		// Corrected includes for cpp files relative to their own location
		if(fqn.equals("nebula.core.Object"))
		{
			appendLine("#include \"String.h\"");
		}
		else if(fqn.equals("nebula.core.String"))
		{
			// String.cpp includes String.h (which now includes Object.h), no need for direct Object.h here
		}
		else if(fqn.equals("nebula.io.Console"))
		{
			appendLine("#include \"../core/String.h\"");
			appendLine("#include \"../core/Object.h\"");
		}
		appendLine("#include <iostream>");
		appendLine("#include <sstream>");
		appendLine("#include <functional>");
		appendLine("");

		for(String ns : namespaceParts)
		{
			if(!ns.isEmpty())
			{
				appendLine("namespace " + ns + " {");
				indent();
			}
		}

		// Use special generators for SDK classes
		if(fqn.equals("nebula.core.Object"))
			generateObjectCpp(declaration);
		else if(fqn.equals("nebula.core.String"))
			generateStringCpp(declaration);
		else if(fqn.equals("nebula.io.Console"))
			generateConsoleCpp(declaration);
		else
			generateGenericClassCpp(declaration);

		for(int i = namespaceParts.length - 1; i >= 0; i--)
		{
			if(!namespaceParts[i].isEmpty())
			{
				dedent();
				appendLine("} // namespace " + namespaceParts[i]);
			}
		}
		generatedClassCodeMap.put(fqn.replace('.', '/') + ".cpp", currentClassCodeBuilder.toString());

		this.currentClassSymbol = null; // Reset
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
		if(declaration.getInitializer() != null)
		{
			return declaration.getInitializer().accept(this);
		}
		return null;
	}

	@Override
	public String visitBlockStatement(BlockStatement statement)
	{
		for(Statement stmt : statement.getStatements())
		{
			String generatedCode = stmt.accept(this);
			if(generatedCode != null && !generatedCode.trim().isEmpty())
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
		statement.getThenBranch().accept(this);
		if(statement.getElseBranch() != null)
		{
			appendLine("} else {");
			statement.getElseBranch().accept(this);
		}
		appendLine("}");
		return null;
	}

	@Override
	public String visitWhileStatement(WhileStatement statement)
	{
		String conditionCode = statement.getCondition().accept(this);
		appendLine("while (" + conditionCode + ") {");
		statement.getBody().accept(this);
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

		// Ensure initializer has a semicolon if it's not a VariableDeclarationStatement
		if(statement.getInitializer() instanceof ExpressionStatement)
		{
			// ExpressionStatement already adds a semicolon
		}
		else if(statement.getInitializer() instanceof VariableDeclarationStatement)
		{
			// VariableDeclarationStatement already adds a semicolon
		}
		else if(!initializerCode.isEmpty() && !initializerCode.endsWith(";"))
		{
			initializerCode += ";";
		}


		appendLine("for (" + initializerCode + " " + conditionCode + "; " + incrementCode + ") {");
		statement.getBody().accept(this);
		appendLine("}");
		return null;
	}

	@Override
	public String visitReturnStatement(ReturnStatement statement)
	{
		if(statement.getValue() != null)
		{
			return "return " + statement.getValue().accept(this) + ";";
		}
		return "return;";
	}

	@Override
	public String visitVariableDeclarationStatement(VariableDeclarationStatement statement)
	{
		Type varType = resolveTypeFromToken(statement.getTypeToken());
		String cppType = statement.getTypeToken().getType() == TokenType.VAR ? "auto" : toCppType(varType);

		String declaration = cppType + " " + statement.getName().getLexeme();
		if(statement.getInitializer() != null)
		{
			declaration += " = " + statement.getInitializer().accept(this);
		}
		else if(varType instanceof ClassType)
		{
			// Initialize shared_ptr to nullptr if no explicit initializer
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
		for(SwitchCase sc : statement.getCases())
		{
			sc.accept(this);
		}
		if(statement.getDefaultBlock() != null)
		{
			appendLine("default:");
			statement.getDefaultBlock().accept(this);
		}
		dedent();
		appendLine("}");
		return null;
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
		for(MethodDeclaration method : declaration.getMethods())
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
			if(!method.getParameters().isEmpty())
			{
				Token paramNameToken = method.getParameters().get(1); // Parameter name is the second token
				Token paramTypeToken = method.getParameters().get(0); // Parameter type is the first token
				String paramName = paramNameToken.getLexeme();
				Type paramType = resolveTypeFromToken(paramTypeToken);


				if(paramType instanceof ClassType)
				{
					// Use the raw() method for printing Nebula String/Object content
					appendLine("if (" + paramName + ") { std::cout << " + paramName + "->toString()->raw(); } else { std::cout << \"null\"; }");
				}
				else if(paramType.equals(PrimitiveType.BOOL))
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


			if(methodName.startsWith("println"))
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
		String cppFqnPrefix = currentNamespacePrefix.isEmpty() ? "" : currentNamespacePrefix.replace(".", "::") + "::";


		// Generate default constructor if no constructors are defined in Nebula
		if(declaration.getConstructors().isEmpty())
		{
			appendLine(cppFqnPrefix + simpleName + "::" + simpleName + "() { } \n");
		}

		for(ConstructorDeclaration ctor : declaration.getConstructors())
		{
			// Ensure initializer list for base class constructor is included if 'super' is called.
			String ctorParams = formatCppParameters(ctor.getParameters());
			StringBuilder ctorSignature = new StringBuilder(cppFqnPrefix + simpleName + "::" + simpleName + "(" + ctorParams + ")");

//			// Check for constructor chaining (this/super)
//			if (ctor.getChainingCall() != null) { // This assumes getChainingCall() exists on ConstructorDeclaration
//				String chainingCallCode = ctor.getChainingCall().accept(this);
//				ctorSignature.append(" : ").append(chainingCallCode);
//			}

			ctorSignature.append(" {");
			appendLine(ctorSignature.toString());
			indent();
			if(ctor.getBody() != null)
				ctor.getBody().accept(this);
			dedent();
			appendLine("}\n");
		}

		for(MethodDeclaration method : declaration.getMethods())
		{
			this.currentMethodSymbol = method.getResolvedSymbol();

			String returnType = toCppType(resolveTypeFromToken(method.getReturnType()));
			String methodName = method.getName().getLexeme();
			if(method.getOperatorKeyword() != null)
				methodName = "operator" + methodName;

			String params = formatCppParameters(method.getParameters());

			boolean isStatic = method.getModifiers().stream().anyMatch(m -> m.getType() == TokenType.STATIC);
			String constQualifier = "";

			// Determine if a method is overriding a base class method for const qualifier
			boolean isOverride = false;
			if(!isStatic && currentClassSymbol.getType().getSuperClassType() != null)
			{
				ClassType superClassType = (ClassType) currentClassSymbol.getType().getSuperClassType();
				if(superClassType.getClassSymbol() != null)
				{
					ClassSymbol superClass = (ClassSymbol) superClassType.getClassSymbol();
					// Corrected: Use methodsByName map to get all methods from superclass
					Optional<MethodSymbol> overriddenMethod = superClass.methodsByName.values().stream()
							.flatMap(List::stream) // Flatten the list of lists of methods
							.filter(m -> m.getName().equals(method.getName().getLexeme()) &&
									m.getParameterTypes().size() / 2 == method.getParameters().size() / 2).findFirst();
					//&& m.isVirtual() Check if base method is virtual (assuming MethodSymbol has isVirtual)
					if(overriddenMethod.isPresent())
					{
						isOverride = true;
					}
				}
			}

			// Apply const qualifier only if it's not static and it's a known const method or an override of a const method
			// Removed isConst() call on MethodSymbol as it was not defined.
			if(!isStatic && (List.of("length", "hashCode", "toString", "operator==").contains(methodName) || isOverride))
			{
				constQualifier = " const";
			}

			appendLine(returnType + " " + cppFqnPrefix + simpleName + "::" + methodName + "(" + params + ")" + constQualifier + " {");
			indent();
			if(method.getBody() != null)
				method.getBody().accept(this);
			// For non-void functions, ensure a return statement is present if the body can exit without one.
			// This is a complex semantic check; for now, rely on Nebula's semantic analysis to ensure all paths return.
			// If not, a default return might be needed here, e.g., if (returnType != "void") appendLine("return {});");
			dedent();
			appendLine("}\n");

			this.currentMethodSymbol = null;
		}
	}

	@Override
	public String visitSwitchCase(SwitchCase switchCase)
	{
		String caseValueCode = switchCase.getValue().accept(this);
		appendLine("case " + caseValueCode + ":");
		indent();
		for(Statement stmt : switchCase.getBody())
		{
			String generatedCode = stmt.accept(this);
			if(generatedCode != null && !generatedCode.isEmpty())
			{
				appendLine(generatedCode);
			}
		}
		appendLine("break;"); // Add break statement for switch cases
		dedent();
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

		if(keyword.equals("this"))
		{
			// Call the current class's constructor
			// currentClassSymbol.getName() returns the simple name of the class
			call.append(currentClassSymbol.getName()).append("(");
		}
		else if(keyword.equals("super"))
		{
			// Call the super class's constructor
			String superCppName = "nebula::core::Object"; // Default
			if(currentClassSymbol != null && currentClassSymbol.getType().getSuperClassType() != null)
			{
				ClassType superClassType = (ClassType) currentClassSymbol.getType().getSuperClassType(); // Your cast
				superCppName = superClassType.getFqn().replace(".", "::");
			}
			// Get the simple name for the superclass constructor call if it's not the default Object
			if(superCppName.startsWith("nebula::core::"))
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
		if(op.equals("+") && leftType instanceof ClassType && ((ClassType) leftType).getFqn().equals("nebula.core.String"))
		{
			return "(*" + left + " + " + right + ")";
		}

		return "(" + left + " " + op + " " + right + ")";
	}

	@Override
	public String visitUnaryExpression(UnaryExpression expression)
	{
		String operandCode = expression.getRight().accept(this);
		String operator = expression.getOperator().getLexeme();

		// For prefix increment/decrement, the operator comes before the operand
		if(operator.equals("++") || operator.equals("--"))
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
		if(expression.getLiteralToken().getType() == TokenType.STRING_LITERAL)
		{
			// Ensure string literals are wrapped in std::make_shared<nebula::core::String>
			return "std::make_shared<nebula::core::String>(\"" + value.toString().replace("\"", "\\\"") + "\")";
		}
		if(expression.getLiteralToken().getType() == TokenType.BOOLEAN_LITERAL)
		{
			// Output 'true' or 'false' directly
			return value.toString().toLowerCase();
		}
		if("null".equals(expression.getLiteralToken().getLexeme()))
		{
			return "nullptr";
		}
		return value.toString();
	}

	@Override
	public String visitIdentifierExpression(IdentifierExpression expression)
	{
		Symbol symbol = expression.getResolvedSymbol();
		if(symbol instanceof VariableSymbol)
		{
			VariableSymbol varSym = (VariableSymbol) symbol;
			if(varSym.getOwnerClass() != null)
			{ // It's a field
				return "this->" + varSym.getName() + "_"; // Assume fields are suffixed
			}
			return varSym.getName();
		}
		if(symbol instanceof ClassSymbol)
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
		return target + " " + expression.getOperator().getLexeme() + " " + value;
	}

	@Override
	public String visitCallExpression(CallExpression expr)
	{
		String callee = expr.getCallee().accept(this);
		String args = expr.getArguments().stream()
				.map(arg -> arg.accept(this))
				.collect(Collectors.joining(", "));
		return callee + "(" + args + ")";
	}

	@Override
	public String visitDotExpression(DotExpression expr)
	{
		String left = expr.getLeft().accept(this);
		String memberName = expr.getMemberName().getLexeme();
		Symbol symbol = expr.getResolvedSymbol();

		if(symbol instanceof MethodSymbol && ((MethodSymbol) symbol).isStatic())
		{
			return left + "::" + memberName;
		}

		return left + "->" + memberName;
	}

	@Override
	public String visitThisExpression(ThisExpression expression)
	{
		// 'this' in Nebula maps to 'this' in C++ when accessing instance members
		// For fields, it will be `this->fieldName_`
		return "this";
	}

	@Override
	public String visitNewExpression(NewExpression expression)
	{
		Type classType = expression.getResolvedType();

		if(classType == null || !(classType instanceof ClassType))
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

	@Override
	public String visitArrayAccessExpression(ArrayAccessExpression expression)
	{
		String arrayCode = expression.getArray().accept(this);
		String indexCode = expression.getIndex().accept(this);

		Type arrayResolvedType = expression.getArray().getResolvedType();

		// Special handling for String character access (Nebula `s[0]` -> C++ `s->operator[](0)`)
		// and for ArrayType where elements are objects (i.e., shared_ptr), still use ->operator[]
		if(arrayResolvedType instanceof ClassType && ((ClassType) arrayResolvedType).getFqn().equals("nebula.core.String"))
		{
			// Access character from a Nebula String object (which is a shared_ptr to nebula::core::String)
			return arrayCode + "->operator[](" + indexCode + ")";
		}
		else if(arrayResolvedType instanceof ArrayType)
		{
			// For std::vector, use regular [] operator. If elements are objects, the vector will hold shared_ptr.
			return arrayCode + "[" + indexCode + "]";
		}
		// Default for other array-like types
		return arrayCode + "[" + indexCode + "]";
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
		if(targetType instanceof ClassType)
		{
			String targetFqn = ((ClassType) targetType).getFqn().replace(".", "::");
			return "(std::dynamic_pointer_cast<" + targetFqn + ">(" + leftCode + ") != nullptr)";
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
		if(expression instanceof IdentifierExpression)
		{
			return ((IdentifierExpression) expression).getName().getLexeme();
		}
		else if(expression instanceof DotExpression)
		{
			DotExpression dot = (DotExpression) expression;
			String leftPart = getQualifiedNameFromExpressionInternal(dot.getLeft());
			if(leftPart == null)
			{
				return null;
			}
			return leftPart + "." + dot.getMemberName().getLexeme();
		}
		return null;
	}
}
