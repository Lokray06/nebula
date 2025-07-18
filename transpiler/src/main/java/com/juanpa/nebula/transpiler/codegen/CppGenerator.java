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

	public CppGenerator(Map<String, ClassSymbol> declaredClasses, SemanticAnalyzer semanticAnalyzer)
	{
		this.declaredClasses = declaredClasses; // Initialize the new field
		this.currentClassCodeBuilder = null;
		this.currentHeaderCodeBuilder = null;
		this.currentClassSymbol = null;
		this.currentMethodSymbol = null;
		this.inStaticContext = false;
		this.currentNamespacePrefix = "";
		this.generatedClassCodeMap = new HashMap<>();

		this.semanticAnalyzer = semanticAnalyzer;
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

	/**
	 * *** NEW ***
	 * Resolves a type from a token and array rank, necessary because the generator
	 * doesn't have access to the fully resolved types on VariableDeclarationStatements.
	 */
	private Type resolveTypeFromToken(Token typeToken, int arrayRank)
	{
		Type baseType;
		switch(typeToken.getType())
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
				if(byFqn != null)
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

		if(baseType instanceof ErrorType)
			return baseType;

		Type currentType = baseType;
		for(int i = 0; i < arrayRank; i++)
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
			// *** MODIFIED ***: Correctly handle nested arrays.
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
			if(paramType instanceof ClassType || paramType instanceof ArrayType) // Also pass vectors by const ref
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
            appendHeaderLine("#include \"" + relativeUpPath + "nebula/core/Math.h\""); // Explicitly include Math for user classes
		}
		if(!fqn.startsWith("nebula."))
		{
			Set<String> deps = collectClassDependencies(declaration);
			int depth = currentNamespacePrefix.isEmpty() ? 0 : currentNamespacePrefix.split("\\.").length;
			for(String dep : deps)
			{
				String path = dep.replace('.', '/') + ".h";
				String rel = "../".repeat(depth) + path;
				appendHeaderLine("#include \"" + rel + "\"");
			}
			appendHeaderLine("");
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

		// FIX: Iterate through all field declarations to correctly handle static vs non-static
		for(FieldDeclaration field : declaration.getFields())
		{
			Symbol fieldSymbol = currentClassSymbol.getClassScope().resolve(field.getName().getLexeme());
			if(fieldSymbol instanceof VariableSymbol)
			{
				VariableSymbol varSymbol = (VariableSymbol) fieldSymbol;
				String modifiers = varSymbol.isStatic() ? "static " : "";
				String fieldCppType = toCppType(field.getResolvedType());
				String fieldName = field.getName().getLexeme();
				appendHeaderLine(modifiers + fieldCppType + " " + fieldName + ";");
			}
		}


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
			// --- START OF MODIFICATION ---

			// Get the resolved symbol from the semantic analysis pass. This is CRUCIAL.
			MethodSymbol resolvedMethodSymbol = method.getResolvedSymbol();
			if(resolvedMethodSymbol == null)
			{
				error(method.getName(), "Internal codegen error: Method symbol not resolved for '" + method.getName().getLexeme() + "'. Cannot generate header declaration.");
				continue;
			}

			boolean isStatic = resolvedMethodSymbol.isStatic();
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


			// Use the resolved type from the symbol for accuracy.
			String returnType = toCppType(resolvedMethodSymbol.getType());

			// *** THIS IS THE KEY FIX: Use the mangled name if it exists. ***
			String methodName = resolvedMethodSymbol.getMangledName() != null
					? resolvedMethodSymbol.getMangledName()
					: resolvedMethodSymbol.getName();

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
		indent();
		statement.getThenBranch().accept(this);
		dedent();
		if(statement.getElseBranch() != null)
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
		if(initializerCode.endsWith(";"))
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
		if(statement.getValue() != null)
		{
			return "return " + statement.getValue().accept(this) + ";";
		}
		return "return;";
	}

	@Override
	public String visitVariableDeclarationStatement(VariableDeclarationStatement statement)
	{
		// *** MODIFIED ***: Use the new helper to resolve the full type, including arrays.
		Type varType = resolveTypeFromToken(statement.getTypeToken(), statement.getArrayRank());
		String cppType = statement.getTypeToken().getType() == TokenType.VAR ? "auto" : toCppType(varType);

		String declaration = cppType + " " + statement.getName().getLexeme();
		if(statement.getInitializer() != null)
		{
			declaration += " = " + statement.getInitializer().accept(this);
		}
		else if(varType instanceof ClassType || varType instanceof ArrayType)
		{
			// Default-initialize shared_ptrs to nullptr and vectors to empty
			declaration += "{}";
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
		Type resolvedType = expression.getResolvedType();
		if(resolvedType == null || !(resolvedType instanceof ArrayType))
		{
			error(expression.getFirstToken(), "Internal codegen error: Array creation expression has no resolved type.");
			return "{}"; // Return empty C++ initializer list as an error fallback
		}

		ArrayType arrayType = (ArrayType) resolvedType;
		String elementCppType = toCppType(arrayType.getElementType());
		String sizeExpr = expression.getSizeExpression().accept(this);

		return "std::vector<" + elementCppType + ">(" + sizeExpr + ")";
	}

	/**
	 * Visits an array initializer expression (e.g., `{1, 2, 3}`) and generates
	 * the equivalent C++ initializer list `{1, 2, 3}`.
	 */
	@Override
	public String visitArrayInitializerExpression(ArrayInitializerExpression expression)
	{
		List<String> elementCodes = expression.getElements().stream()
				.map(element -> element.accept(this))
				.collect(Collectors.toList());

		return "{" + String.join(", ", elementCodes) + "}";
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

		// FIX: Define and initialize static fields
		for(FieldDeclaration field : declaration.getFields())
		{
			Symbol fieldSymbol = currentClassSymbol.getClassScope().resolve(field.getName().getLexeme());
			if(fieldSymbol instanceof VariableSymbol)
			{
				VariableSymbol varSymbol = (VariableSymbol) fieldSymbol;
				if(varSymbol.isStatic())
				{
					String fieldCppType = toCppType(field.getResolvedType());
					String fieldName = field.getName().getLexeme();
					String initializer = "{}"; // Default initializer
					if(field.getInitializer() != null)
					{
						initializer = field.getInitializer().accept(this);
					}
					appendLine(fieldCppType + " " + cppFqnPrefix + simpleName + "::" + fieldName + " = " + initializer + ";\n");
				}
			}
		}


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
			// Ensure the method declaration in the AST has its resolved symbol from semantic analysis
			if(method.getResolvedSymbol() == null)
			{
				// This indicates a problem in the semantic analysis setup.
				// For now, we'll try to look it up, but ideally, it should be pre-set.
				continue; // Skip methods that weren't resolved.
			}
			this.currentMethodSymbol = method.getResolvedSymbol();

			String returnType = toCppType(resolveTypeFromToken(method.getReturnType()));

			// NEW: Use mangled name if available
			String methodName = this.currentMethodSymbol.getMangledName() != null
					? this.currentMethodSymbol.getMangledName()
					: this.currentMethodSymbol.getName();

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
		appendLine("case " + caseValueCode + ": {");
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
		Type rightType = expression.getRight().getResolvedType();

		// If the left side is a class type, it's a shared_ptr.
		// Member operators require the object to be dereferenced.
		if(leftType instanceof ClassType)
		{
			// For any class type, dereference the left operand to access member operators.
			String leftOperand = "*" + left;

			// String concatenation requires special handling for the right operand,
			// which may need to be converted to a string.
			if(op.equals("+") && ((ClassType) leftType).getFqn().equals("nebula.core.String"))
			{
				String rightOperandForConcat = right;

				if(rightType instanceof PrimitiveType)
				{
					if(rightType.equals(PrimitiveType.INT) ||
							rightType.equals(PrimitiveType.FLOAT) ||
							rightType.equals(PrimitiveType.DOUBLE))
					{
						// For numeric primitives, use std::to_string
						rightOperandForConcat = "std::make_shared<nebula::core::String>(std::to_string(" + right + "))";
					}
					else if(rightType.equals(PrimitiveType.BOOL))
					{
						// For boolean, convert to "true" or "false" string
						rightOperandForConcat = "std::make_shared<nebula::core::String>(" + right + " ? \"true\" : \"false\")";
					}
					else if(rightType.equals(PrimitiveType.CHAR) ||
							rightType.equals(PrimitiveType.BYTE))
					{ // Assuming BYTE maps to char
						// For char/byte, convert to string containing single character
						rightOperandForConcat = "std::make_shared<nebula::core::String>(std::string(1, " + right + "))";
					}
				}
				else if(rightType instanceof ClassType)
				{
					// If the right operand is another Nebula class, call its toString() method.
					if(!((ClassType) rightType).getFqn().equals("nebula.core.String"))
					{
						rightOperandForConcat = right + "->toString()";
					}
				}
				// If rightType is NullType, 'right' will be "nullptr", which operator+ will handle.

				return "(" + leftOperand + " + " + rightOperandForConcat + ")";
			}

			// For all other class types (like Vector2) and operators, use the dereferenced
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
			if(varSym.getOwnerClass() != null && !varSym.isStatic())
			{ // It's an instance field
				return "this->" + varSym.getName();
			}
			else if(varSym.getOwnerClass() != null && varSym.isStatic())
			{
				// It's a static field
				return varSym.getOwnerClass().getFqn().replace(".", "::") + "::" + varSym.getName();
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
		String op = expression.getOperator().getLexeme();

		Type targetType = expression.getTarget().getResolvedType();

		// Check for a compound assignment operator (e.g., "+=", "-=") on a class type.
		if(targetType instanceof ClassType && op.length() > 1 && op.endsWith("="))
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
		if(!(resolvedSymbol instanceof MethodSymbol))
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

		if(expr.getCallee() instanceof IdentifierExpression)
		{
			if(resolvedMethod.isStatic())
			{
				calleeString = this.currentClassSymbol.getFqn().replace(".", "::") + "::" + methodName;
			}
			else
			{
				calleeString = "this->" + methodName;
			}
		}
		else if(expr.getCallee() instanceof DotExpression)
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

		// *** MODIFIED ***: Handle .length on arrays
		if(leftType instanceof ArrayType)
		{
			if(memberName.equals("length"))
			{
				return left + ".size()";
			}
		}

		if(symbol instanceof MethodSymbol && ((MethodSymbol) symbol).isStatic())
		{
			return left + "::" + memberName;
		}
		else if(symbol instanceof VariableSymbol && ((VariableSymbol) symbol).isStatic())
		{
			return left + "::" + memberName;
		}

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

	// In CppGenerator.java

	/**
	 * Visits an array access expression (e.g., `myArray[i]`) and generates
	 * the C++ equivalent `myArray[i]`.
	 */
	@Override
	public String visitArrayAccessExpression(ArrayAccessExpression expression)
	{
		String arrayCode = expression.getArray().accept(this);
		String indexCode = expression.getIndex().accept(this);

		// For std::vector, C++ uses the standard [] operator for access.
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
		else if(targetType instanceof ArrayType)
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

	/**
	 * REPLACEMENT for the original collectClassDependencies method.
	 * This version performs a full traversal of the class's AST to find all dependencies.
	 */
	private Set<String> collectClassDependencies(ClassDeclaration classDecl)
	{
		Set<String> dependencies = new HashSet<>();

		// 1. Superclass dependency
		if(currentClassSymbol != null && currentClassSymbol.getType().getSuperClassType() != null)
		{
			addTypeDependency(currentClassSymbol.getType().getSuperClassType(), dependencies);
		}

		// 2. Field dependencies (declaration type and initializer)
		for(FieldDeclaration field : classDecl.getFields())
		{
			addTypeDependency(field.getResolvedType(), dependencies);
			if(field.getInitializer() != null)
			{
				collectDependenciesFrom(field.getInitializer(), dependencies);
			}
		}

		// 3. Constructor dependencies (parameters and body)
		for(ConstructorDeclaration ctor : classDecl.getConstructors())
		{
			// Parameters
			for(int i = 0; i < ctor.getParameters().size(); i += 2)
			{
				Token typeToken = ctor.getParameters().get(i);
				Type paramType = resolveTypeFromToken(typeToken);
				addTypeDependency(paramType, dependencies);
			}
			// Body
			if(ctor.getBody() != null)
			{
				collectDependenciesFrom(ctor.getBody(), dependencies);
			}
			// Chaining call (this() or super())
			if(ctor.getChainingCall() != null)
			{
				collectDependenciesFrom(ctor.getChainingCall(), dependencies);
			}
		}

		// 4. Method dependencies (return type, parameters, and body)
		for(MethodDeclaration method : classDecl.getMethods())
		{
			MethodSymbol methodSymbol = method.getResolvedSymbol();
			if(methodSymbol == null)
				continue;

			// Return type
			addTypeDependency(methodSymbol.getType(), dependencies);

			// Parameter types (from the reliable resolved symbol)
			if(methodSymbol.getParameterTypes() != null)
			{
				for(Type paramType : methodSymbol.getParameterTypes())
				{
					addTypeDependency(paramType, dependencies);
				}
			}

			// Body
			if(method.getBody() != null)
			{
				collectDependenciesFrom(method.getBody(), dependencies);
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
	private void addTypeDependency(Type type, Set<String> dependencies)
	{
		if(type == null)
			return;

		// Unwrap array types to get the base element type.
		while(type instanceof ArrayType)
		{
			type = ((ArrayType) type).getElementType();
		}

		if(type instanceof ClassType)
		{
			String fqn = ((ClassType) type).getFqn();
			// Add dependency if it's not a core class and not the class we are currently processing.
			if(fqn != null && !isCoreClass(fqn) && !fqn.equals(currentClassSymbol.getFqn()))
			{
				dependencies.add(fqn);
			}
		}
	}

	/**
	 * Recursively traverses a Statement AST node to find all type dependencies.
	 *
	 * @param stmt         The statement to traverse.
	 * @param dependencies The set to add dependencies to.
	 */
	private void collectDependenciesFrom(Statement stmt, Set<String> dependencies)
	{
		if(stmt == null)
			return;

		if(stmt instanceof BlockStatement)
		{
			for(Statement s : ((BlockStatement) stmt).getStatements())
			{
				collectDependenciesFrom(s, dependencies);
			}
		}
		else if(stmt instanceof ExpressionStatement)
		{
			collectDependenciesFrom(((ExpressionStatement) stmt).getExpression(), dependencies);
		}
		else if(stmt instanceof IfStatement)
		{
			collectDependenciesFrom(((IfStatement) stmt).getCondition(), dependencies);
			collectDependenciesFrom(((IfStatement) stmt).getThenBranch(), dependencies);
			if(((IfStatement) stmt).getElseBranch() != null)
			{
				collectDependenciesFrom(((IfStatement) stmt).getElseBranch(), dependencies);
			}
		}
		else if(stmt instanceof WhileStatement)
		{
			collectDependenciesFrom(((WhileStatement) stmt).getCondition(), dependencies);
			collectDependenciesFrom(((WhileStatement) stmt).getBody(), dependencies);
		}
		else if(stmt instanceof ForStatement)
		{
			if(((ForStatement) stmt).getInitializer() != null)
			{
				// The initializer can be a declaration or an expression.
				if(((ForStatement) stmt).getInitializer() instanceof Statement)
				{
					collectDependenciesFrom((Statement) ((ForStatement) stmt).getInitializer(), dependencies);
				}
				else
				{
					collectDependenciesFrom((Expression) ((ForStatement) stmt).getInitializer(), dependencies);
				}
			}
			if(((ForStatement) stmt).getCondition() != null)
			{
				collectDependenciesFrom(((ForStatement) stmt).getCondition(), dependencies);
			}
			if(((ForStatement) stmt).getIncrement() != null)
			{
				collectDependenciesFrom(((ForStatement) stmt).getIncrement(), dependencies);
			}
			collectDependenciesFrom(((ForStatement) stmt).getBody(), dependencies);
		}
		else if(stmt instanceof ReturnStatement)
		{
			if(((ReturnStatement) stmt).getValue() != null)
			{
				collectDependenciesFrom(((ReturnStatement) stmt).getValue(), dependencies);
			}
		}
		else if(stmt instanceof VariableDeclarationStatement)
		{
			Type varType = resolveTypeFromToken(((VariableDeclarationStatement) stmt).getTypeToken(), ((VariableDeclarationStatement) stmt).getArrayRank());
			addTypeDependency(varType, dependencies);
			if(((VariableDeclarationStatement) stmt).getInitializer() != null)
			{
				collectDependenciesFrom(((VariableDeclarationStatement) stmt).getInitializer(), dependencies);
			}
		}
		else if(stmt instanceof SwitchStatement)
		{
			collectDependenciesFrom(((SwitchStatement) stmt).getSwitchExpression(), dependencies);
			for(SwitchCase sc : ((SwitchStatement) stmt).getCases())
			{
				collectDependenciesFrom(sc.getValue(), dependencies);
				for(Statement s : sc.getBody())
				{
					collectDependenciesFrom(s, dependencies);
				}
			}
			if(((SwitchStatement) stmt).getDefaultBlock() != null)
			{
				collectDependenciesFrom(((SwitchStatement) stmt).getDefaultBlock(), dependencies);
			}
		}
		else if(stmt instanceof ConstructorChainingCallStatement)
		{
			for(Expression arg : ((ConstructorChainingCallStatement) stmt).getArguments())
			{
				collectDependenciesFrom(arg, dependencies);
			}
		}
	}

	/**
	 * Recursively traverses an Expression AST node to find all type dependencies.
	 *
	 * @param expr         The expression to traverse.
	 * @param dependencies The set to add dependencies to.
	 */
	private void collectDependenciesFrom(Expression expr, Set<String> dependencies)
	{
		if(expr == null)
			return;

		// Every expression node should have its type resolved by the semantic analyzer.
		// This is the most reliable source for dependencies.
		addTypeDependency(expr.getResolvedType(), dependencies);

		// Recurse into sub-expressions.
		if(expr instanceof BinaryExpression)
		{
			collectDependenciesFrom(((BinaryExpression) expr).getLeft(), dependencies);
			collectDependenciesFrom(((BinaryExpression) expr).getRight(), dependencies);
		}
		else if(expr instanceof UnaryExpression)
		{
			collectDependenciesFrom(((UnaryExpression) expr).getRight(), dependencies);
		}
		else if(expr instanceof PostfixUnaryExpression)
		{
			collectDependenciesFrom(((PostfixUnaryExpression) expr).getOperand(), dependencies);
		}
		else if(expr instanceof CallExpression)
		{
			collectDependenciesFrom(((CallExpression) expr).getCallee(), dependencies);
			for(Expression arg : ((CallExpression) expr).getArguments())
			{
				collectDependenciesFrom(arg, dependencies);
			}
		}
		else if(expr instanceof DotExpression)
		{
			collectDependenciesFrom(((DotExpression) expr).getLeft(), dependencies);
		}
		else if(expr instanceof AssignmentExpression)
		{
			collectDependenciesFrom(((AssignmentExpression) expr).getTarget(), dependencies);
			collectDependenciesFrom(((AssignmentExpression) expr).getValue(), dependencies);
		}
		else if(expr instanceof NewExpression)
		{
			for(Expression arg : ((NewExpression) expr).getArguments())
			{
				collectDependenciesFrom(arg, dependencies);
			}
		}
		else if(expr instanceof ArrayCreationExpression)
		{
			collectDependenciesFrom(((ArrayCreationExpression) expr).getSizeExpression(), dependencies);
		}
		else if(expr instanceof ArrayInitializerExpression)
		{
			for(Expression element : ((ArrayInitializerExpression) expr).getElements())
			{
				collectDependenciesFrom(element, dependencies);
			}
		}
		else if(expr instanceof ArrayAccessExpression)
		{
			collectDependenciesFrom(((ArrayAccessExpression) expr).getArray(), dependencies);
			collectDependenciesFrom(((ArrayAccessExpression) expr).getIndex(), dependencies);
		}
		else if(expr instanceof GroupingExpression)
		{
			collectDependenciesFrom(((GroupingExpression) expr).getExpression(), dependencies);
		}
		else if(expr instanceof IsExpression)
		{
			collectDependenciesFrom(((IsExpression) expr).getLeft(), dependencies);
			Type targetType = resolveTypeFromToken(((IsExpression) expr).getTypeToken());
			addTypeDependency(targetType, dependencies);
		}
	}
}