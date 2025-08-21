// File: src/main/java/com/juanpa/nebula/transpiler/codegen/LLVMIRGenerator.java

package com.juanpa.nebula.transpiler.codegen;

import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.ast.declarations.*;
import com.juanpa.nebula.transpiler.ast.expressions.*;
import com.juanpa.nebula.transpiler.ast.statements.*;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.semantics.*;
import com.juanpa.nebula.transpiler.semantics.Type;
import com.juanpa.nebula.transpiler.util.Debug; // Import the debug utility
import org.bytedeco.javacpp.BytePointer;
import org.bytedeco.javacpp.Pointer;
import org.bytedeco.javacpp.PointerPointer;
import org.bytedeco.llvm.LLVM.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.stream.Collectors;

import static org.bytedeco.llvm.global.LLVM.*;

public class LLVMIRGenerator implements ASTVisitor<LLVMValueRef>
{

	private final SemanticAnalyzer semanticAnalyzer;
	private LLVMContextRef context;
	private LLVMModuleRef module;
	private LLVMBuilderRef builder;
	private LLVMTargetDataRef targetData;
	private LLVMTargetMachineRef targetMachine;

	// --- State Management ---
	private final Stack<ScopeContext> scopes = new Stack<>();
	private final Map<String, LLVMTypeRef> definedStructs = new HashMap<>();
	private LLVMValueRef currentFunction;
	private MethodSymbol currentMethodSymbol;
	private boolean isLValueContext = false;

	// --- External C Library Functions ---
	private LLVMValueRef mallocFunc;
	private LLVMValueRef freeFunc;
	private LLVMValueRef printfFunc;

	private LLVMValueRef currentAllocaForStackObject = null; // <-- ADD THIS LINE

	private static class ScopeContext
	{
		final Map<String, LLVMValueRef> variables = new HashMap<>();
		final List<LLVMValueRef> heapAllocations = new ArrayList<>();
	}

	public LLVMIRGenerator(SemanticAnalyzer semanticAnalyzer)
	{
		this.semanticAnalyzer = semanticAnalyzer;
	}

	public void generate(Program program, String outputFilename)
	{
		Debug.log("Starting LLVM IR Generation...");
		Debug.indent();

		Debug.log("Initializing LLVM components...");
		LLVMInitializeAllTargetInfos();
		LLVMInitializeAllTargets();
		LLVMInitializeAllTargetMCs();
		LLVMInitializeAllAsmParsers();
		LLVMInitializeAllAsmPrinters();

		context = LLVMContextCreate();
		module = LLVMModuleCreateWithNameInContext("nebula_module", context);
		builder = LLVMCreateBuilderInContext(context);

		BytePointer error = new BytePointer((Pointer) null);
		BytePointer triple = LLVMGetDefaultTargetTriple();
		LLVMTargetRef target = new LLVMTargetRef();
		if (LLVMGetTargetFromTriple(triple, target, error) != 0)
		{
			System.err.println("Error getting target from triple: " + error.getString());
			LLVMDisposeMessage(error);
			return;
		}

		targetMachine = LLVMCreateTargetMachine(target, triple.getString(), "", "", LLVMCodeGenLevelDefault, LLVMRelocDefault, LLVMCodeModelDefault);
		targetData = LLVMCreateTargetDataLayout(targetMachine);
		LLVMSetModuleDataLayout(module, targetData);
		LLVMSetTarget(module, triple);
		LLVMDisposeMessage(error);

		// --- FIX: The order of operations is critical. ---
		// 1. First, run the DeclarationVisitor to define all structs and declare all function signatures.
		Debug.log("Running Declaration Visitor Pre-pass...");
		DeclarationVisitor declarationVisitor = new DeclarationVisitor();
		declarationVisitor.declareAll(semanticAnalyzer.getDeclaredClasses());
		Debug.log("Declaration Visitor Pre-pass COMPLETE.");
		//dumpModuleState("After Declarations"); // <--- ADD THIS LINE


		// 2. NOW that all types are known (especially nebula.core.String), declare external functions that might use them.
		declareExternalFunctions();

		// 3. Finally, traverse the AST to generate the actual code for method bodies.
		enterScope();
		Debug.log("Starting main AST traversal for code generation...");
		program.accept(this);
		Debug.log("Main AST traversal COMPLETE.");
		exitScope();

		// --- FIX START: Generate the C-compatible 'main' entry point ---
		Debug.log("Generating LLVM 'main' entry point to call the Nebula program's main function...");

		// 1. Find the main function generated from the Nebula code (it's currently named "main").
		LLVMValueRef nebulaMainFunc = LLVMGetNamedFunction(module, "main");

		if (nebulaMainFunc != null && !nebulaMainFunc.isNull())
		{
			// 2. Rename it to avoid a name collision.
			LLVMSetValueName(nebulaMainFunc, "nebula_program_main");
			Debug.log("Renamed user's main function to 'nebula_program_main'.");

			// 3. Create the REAL C main function, which MUST be named "main" and return i32.
			LLVMTypeRef cMainFuncType = LLVMFunctionType(LLVMInt32TypeInContext(context), (PointerPointer) null, 0, 0);
			LLVMValueRef cMainFunc = LLVMAddFunction(module, "main", cMainFuncType);
			LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlockInContext(context, cMainFunc, "entry");
			LLVMPositionBuilderAtEnd(builder, entryBlock);

			// 4. Call the renamed Nebula main function from the C main.
			Debug.log("Adding call to 'nebula_program_main()' from the C 'main' function.");

			LLVMTypeRef nebulaMainFuncType = LLVMGlobalGetValueType(nebulaMainFunc);
			LLVMBuildCall2(builder, nebulaMainFuncType, nebulaMainFunc, (PointerPointer) null, 0, "");

			// 5. Add the standard C return code. "return 0;" means success.
			LLVMBuildRet(builder, LLVMConstInt(LLVMInt32TypeInContext(context), 0, 0));

		}
		else
		{
			Debug.log("WARNING: Could not find the Nebula 'main' function to call. The program will do nothing.");
			// Still create a dummy main so it can link
			LLVMTypeRef cMainFuncType = LLVMFunctionType(LLVMInt32TypeInContext(context), (PointerPointer) null, 0, 0);
			LLVMValueRef cMainFunc = LLVMAddFunction(module, "main", cMainFuncType);
			LLVMBasicBlockRef entryBlock = LLVMAppendBasicBlockInContext(context, cMainFunc, "entry");
			LLVMPositionBuilderAtEnd(builder, entryBlock);
			LLVMBuildRet(builder, LLVMConstInt(LLVMInt32TypeInContext(context), 1, 0)); // Return 1 for error
		}
		// --- FIX END ---

		BytePointer verificationError = new BytePointer((Pointer) null);
		if (LLVMVerifyModule(module, LLVMReturnStatusAction, verificationError) != 0)
		{
			System.err.println("LLVM Verify Error: " + verificationError.getString());
			Debug.log("LLVM Module verification FAILED: %s", verificationError.getString());
			LLVMDisposeMessage(verificationError);
		}
		else
		{
			Debug.log("LLVM Module verification PASSED.");
			Debug.log("Writing LLVM IR to file: %s", outputFilename);
			BytePointer fileWriteError = new BytePointer((Pointer) null);
			if (LLVMPrintModuleToFile(module, outputFilename, fileWriteError) != 0)
			{
				System.err.println("Error writing IR to file: " + fileWriteError.getString());
				Debug.log("Error writing IR to file: %s", fileWriteError.getString());
				LLVMDisposeMessage(fileWriteError);
			}
			else
			{
				System.out.println("LLVM IR generated successfully at: " + outputFilename);
				Debug.log("LLVM IR generation successful.");
			}
		}

		dumpModuleState("Dumping the final code generation");

		Debug.log("Disposing LLVM resources...");
		LLVMDisposeTargetData(targetData);
		LLVMDisposeTargetMachine(targetMachine);
		LLVMDisposeBuilder(builder);
		LLVMDisposeModule(module);
		LLVMContextDispose(context);
		Debug.log("LLVM resources disposed.");
		Debug.dedent();
		Debug.log("LLVM IR Generation Finished.");

	}

	private void declareExternalFunctions()
	{
		Debug.log("Declaring external C/C++ functions (malloc, free, printf, string_factory)...");
		Debug.indent();

		// Malloc: i8* @malloc(i64)
		LLVMTypeRef mallocReturnType = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
		LLVMTypeRef mallocParamType = LLVMInt64TypeInContext(context);
		LLVMTypeRef mallocFuncType = LLVMFunctionType(mallocReturnType, mallocParamType, 1, 0);
		mallocFunc = LLVMAddFunction(module, "malloc", mallocFuncType);
		Debug.log("Declared: i8* @malloc(i64)");

		// Free: void @free(i8*)
		LLVMTypeRef freeParamType = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
		LLVMTypeRef freeFuncType = LLVMFunctionType(LLVMVoidTypeInContext(context), freeParamType, 1, 0);
		freeFunc = LLVMAddFunction(module, "free", freeFuncType);
		Debug.log("Declared: void @free(i8*)");

		// Printf: i32 @printf(i8*, ...)
		LLVMTypeRef printfReturnType = LLVMInt32TypeInContext(context);
		LLVMTypeRef printfParamType = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
		LLVMTypeRef printfFuncType = LLVMFunctionType(printfReturnType, printfParamType, 1, 1);
		printfFunc = LLVMAddFunction(module, "printf", printfFuncType);
		Debug.log("Declared: i32 @printf(i8*, ...)");

		// C++ String Factory: %nebula.core.String* @from_c_string(i8*)
		LLVMTypeRef stringStructType = definedStructs.get("nebula.core.String");
		if (stringStructType == null)
		{
			Debug.log("-> FATAL ERROR: Cannot declare string factory because nebula.core.String struct is not defined.");
			Debug.dedent();
			// This should not happen with the new ordering, but it's a good safeguard.
			return;
		}
		LLVMTypeRef stringPtrType = LLVMPointerType(stringStructType, 0);
		LLVMTypeRef cStringParamType = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
		LLVMTypeRef stringFactoryFuncType = LLVMFunctionType(stringPtrType, cStringParamType, 1, 0);
		// The second argument is the C++ mangled name for: nebula::core::String::from_c_string(char const*)
		LLVMAddFunction(module, "_ZN6nebula4core6String15from_c_stringEPKc", stringFactoryFuncType);
		Debug.log("Declared: %nebula.core.String* @nebula::core::String::from_c_string(i8*)");

		Debug.dedent();
	}

	private LLVMTypeRef getLLVMType(Type nebulaType)
	{
		Debug.log("Mapping Nebula Type '%s' to LLVM Type", nebulaType != null ? nebulaType.getName() : "null");
		Debug.indent();

		if (nebulaType == null)
		{
			Debug.log("-> Nebula Type is null, returning null LLVM Type.");
			Debug.dedent();
			return null;
		}

		LLVMTypeRef result = null;
		if (nebulaType instanceof PrimitiveType)
		{
			if (nebulaType.equals(PrimitiveType.VOID))
			{
				result = LLVMVoidTypeInContext(context);
			}
			else if (nebulaType.equals(PrimitiveType.BOOL))
			{
				result = LLVMInt1TypeInContext(context);
			}
			else if (nebulaType.equals(PrimitiveType.INT) || nebulaType.equals(PrimitiveType.INT32))
			{
				result = LLVMInt32TypeInContext(context);
			}
			// --- ADD THIS BLOCK ---
			else if (nebulaType.equals(PrimitiveType.INT64) || nebulaType.equals(PrimitiveType.LONG))
			{
				result = LLVMInt64TypeInContext(context);
			}
			else if (nebulaType.equals(PrimitiveType.UINT64) || nebulaType.equals(PrimitiveType.ULONG))
			{
				result = LLVMInt64TypeInContext(context); // LLVM uses i64 for both signed and unsigned
			}
			// --- END OF FIX ---
			else if (nebulaType.equals(PrimitiveType.BYTE))
			{
				result = LLVMInt8TypeInContext(context);
			}
			else if (nebulaType.equals(PrimitiveType.DOUBLE))
			{
				result = LLVMDoubleTypeInContext(context);
			}
			else if (nebulaType.equals(PrimitiveType.FLOAT))
			{
				result = LLVMFloatTypeInContext(context);
			}
			else
			{
				result = LLVMInt32TypeInContext(context); // Default fallback
			}

		}
		else if (nebulaType instanceof ClassType)
		{
			String fqn = ((ClassType) nebulaType).getFqn();
			LLVMTypeRef structType = definedStructs.get(fqn);

			if (structType == null)
			{
				Debug.log("-> ERROR: Struct '%s' not found in definedStructs map.", fqn);
			}
			else if (semanticAnalyzer.getStackAllocatedClasses().contains(fqn))
			{
				Debug.log("-> Type '%s' is marked for stack allocation. Using value type.", fqn);
				result = structType; // Return the struct itself
			}
			else
			{
				// THIS IS THE CORRECT DEFAULT FOR ALL HEAP-ALLOCATED CLASSES, INCLUDING STRING
				Debug.log("-> Type '%s' is heap allocated. Using pointer type.", fqn);
				result = LLVMPointerType(structType, 0); // Return a pointer to the struct
			}

		}
		else if (nebulaType instanceof ArrayType)
		{
			Type elementType = ((ArrayType) nebulaType).getElementType();
			LLVMTypeRef elementLLVMType = getLLVMType(elementType);
			if (elementLLVMType != null)
			{
				result = LLVMPointerType(elementLLVMType, 0);
			}
		}

		Debug.log("-> Result: %s", result != null ? LLVMPrintTypeToString(result).getString() : "null");
		Debug.dedent();
		return result;
	}

	private void enterScope()
	{
		Debug.log("Entering new scope (level %d)", scopes.size() + 1);
		scopes.push(new ScopeContext());
	}

	private void exitScope()
	{
		Debug.log("Exiting scope (level %d)", scopes.size());
		ScopeContext currentScope = scopes.peek();
		if (!currentScope.heapAllocations.isEmpty())
		{
			Debug.indent();
			Debug.log("Cleaning up %d heap allocation(s) in this scope.", currentScope.heapAllocations.size());
			for (LLVMValueRef heapPtr : currentScope.heapAllocations)
			{
				Debug.log("-> Generating call to @free for a heap pointer.");
				LLVMValueRef ptrToFree = LLVMBuildBitCast(builder, heapPtr, LLVMPointerType(LLVMInt8TypeInContext(context), 0), "tmp_cast_free");
				LLVMTypeRef freeFuncType = LLVMGetElementType(LLVMTypeOf(freeFunc));
				PointerPointer<LLVMValueRef> args = new PointerPointer<>(ptrToFree);
				LLVMBuildCall2(builder, freeFuncType, freeFunc, args, 1, "");
			}
			Debug.dedent();
		}
		scopes.pop();
	}

	private LLVMValueRef findVariable(String name)
	{
		Debug.log("Searching for variable '%s' in scopes.", name);
		for (int i = scopes.size() - 1; i >= 0; i--)
		{
			if (scopes.get(i).variables.containsKey(name))
			{
				LLVMValueRef var = scopes.get(i).variables.get(name);
				Debug.log("-> Found '%s' in scope level %d.", name, i + 1);
				return var;
			}
		}
		Debug.log("-> Variable '%s' not found in any scope.", name);
		return null;
	}

	// --- AST Visitor Implementations ---
	// NOTE: The rest of your visitor methods remain unchanged as they were not part of the crash.
	// I am including them here for completeness.

	@Override
	public LLVMValueRef visitProgram(Program program)
	{
		Debug.log("Visiting Program");
		Debug.indent();
		for (NamespaceDeclaration ns : program.getNamespaceDeclarations())
		{
			ns.accept(this);
		}
		Debug.dedent();
		return null;
	}

	@Override
	public LLVMValueRef visitNamespaceDeclaration(NamespaceDeclaration declaration)
	{
		Debug.log("Visiting NamespaceDeclaration: %s", declaration.getNameExpression().toString());
		Debug.indent();
		for (ClassDeclaration classDecl : declaration.getClassDeclarations())
		{
			classDecl.accept(this);
		}
		Debug.dedent();
		return null;
	}

	@Override
	public LLVMValueRef visitClassDeclaration(ClassDeclaration declaration)
	{
		Debug.log("Visiting ClassDeclaration: %s", declaration.getName().getLexeme());
		Debug.indent();
		for (MethodDeclaration method : declaration.getMethods())
		{
			method.accept(this);
		}
		for (ConstructorDeclaration ctor : declaration.getConstructors())
		{
			ctor.accept(this);
		}

		// --- ADD THIS LOOP ---
		// This is the missing piece. Now the visitor will also traverse
		// each property, which will in turn call your new
		// visitPropertyDeclaration method.
		for (PropertyDeclaration prop : declaration.getProperties())
		{
			prop.accept(this);
		}
		// --- END OF FIX ---

		Debug.dedent();
		return null;
	}

	@Override
	public LLVMValueRef visitMethodDeclaration(MethodDeclaration declaration)
	{
		Debug.log("Visiting MethodDeclaration: %s", declaration.getName().getLexeme());
		Debug.indent();
		try
		{
			MethodSymbol methodSymbol = declaration.getResolvedSymbol();
			this.currentMethodSymbol = methodSymbol;
			if (methodSymbol == null)
			{
				Debug.log("-> No resolved symbol. Skipping.");
				return null;
			}
			if (methodSymbol.isWrapper())
			{
				Debug.log("-> Method is a wrapper. Skipping body generation.");
				return null;
			}

			String mangledName = methodSymbol.getMangledName();
			Debug.log("-> Mangled Name: %s", mangledName);
			LLVMValueRef function = LLVMGetNamedFunction(module, mangledName);
			if (function == null)
			{
				Debug.log("-> FATAL: Function '%s' was not declared in pre-pass.", mangledName);
				return null;
			}
			this.currentFunction = function;

			Debug.log("-> Creating 'entry' basic block.");
			LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, function, "entry");
			LLVMPositionBuilderAtEnd(builder, entry);

			enterScope();

			int paramIndex = 0;
			if (!methodSymbol.isStatic())
			{
				LLVMValueRef thisPtr = LLVMGetParam(function, 0);
				LLVMSetValueName(thisPtr, "this");
				paramIndex = 1;
			}

			for (int i = 0; i < methodSymbol.getParameterTypes().size(); i++)
			{
				LLVMValueRef param = LLVMGetParam(function, i + paramIndex);
				String paramName = declaration.getParameters().get(i * 2 + 1).getLexeme();
				LLVMSetValueName(param, paramName);
				Type paramType = methodSymbol.getParameterTypes().get(i);

				LLVMValueRef paramAlloca = LLVMBuildAlloca(builder, getLLVMType(paramType), paramName + "_addr");
				LLVMBuildStore(builder, param, paramAlloca);
				scopes.peek().variables.put(paramName, paramAlloca);
			}

			if (declaration.getBody() != null)
			{
				declaration.getBody().accept(this);
			}

			if (LLVMGetBasicBlockTerminator(LLVMGetLastBasicBlock(function)) == null)
			{
				if (methodSymbol.getType().equals(PrimitiveType.VOID))
				{
					Debug.log("-> No terminator found. Adding implicit 'ret void'.");
					LLVMBuildRetVoid(builder);
				}
				else
				{
					Debug.log("-> WARNING: Non-void function has no return statement. Adding 'unreachable'.");
					LLVMBuildUnreachable(builder);
				}
			}

			exitScope(); // Changed from scopes.pop() to maintain consistency
			return function;
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitBlockStatement(BlockStatement statement)
	{
		Debug.log("Visiting BlockStatement");
		Debug.indent();
		enterScope();
		for (Statement stmt : statement.getStatements())
		{
			stmt.accept(this);
		}
		exitScope();
		Debug.dedent();
		return null;
	}

	@Override
	public LLVMValueRef visitLiteralExpression(LiteralExpression expression)
	{
		Type type = expression.getResolvedType();
		Object value = expression.getValue();

		if (type.equals(PrimitiveType.INT) || type.equals(PrimitiveType.INT32))
		{
			return LLVMConstInt(LLVMInt32TypeInContext(context), ((Number) value).longValue(), 1);
		}
		if (type.equals(PrimitiveType.BOOL))
		{
			return LLVMConstInt(LLVMInt1TypeInContext(context), (Boolean) value ? 1 : 0, 0);
		}
		if (type.equals(PrimitiveType.DOUBLE))
		{
			return LLVMConstReal(LLVMDoubleTypeInContext(context), ((Number) value).doubleValue());
		}
		if (type.equals(PrimitiveType.FLOAT))
		{
			return LLVMConstReal(LLVMFloatTypeInContext(context), ((Number) value).doubleValue());
		}

		// --- START OF REVERTED CODE ---
		if (type instanceof ClassType && ((ClassType) type).getFqn().equals("nebula.core.String"))
		{
			String strValue = (String) value;

			// Create a C-string constant
			LLVMValueRef cString = LLVMBuildGlobalStringPtr(builder, strValue, ".str_literal");

			// Get the type for our Nebula String struct
			LLVMTypeRef stringStructType = definedStructs.get("nebula.core.String");

			// Allocate space for our struct on the stack. This is a temporary object.
			LLVMValueRef stringObjPtr = LLVMBuildAlloca(builder, stringStructType, "temp_string_obj");

			// Get a pointer to the first field (the i8* field) inside the struct.
			LLVMValueRef fieldPtr = LLVMBuildStructGEP2(builder, stringStructType, stringObjPtr, 0, "data_field_ptr");

			// Store the constant C-string pointer into our struct's field.
			LLVMBuildStore(builder, cString, fieldPtr);

			// Return the pointer to the temporary, stack-allocated struct.
			return stringObjPtr;
		}
		// --- END OF REVERTED CODE ---

		if (type.equals(NullType.INSTANCE))
		{
			LLVMTypeRef objectStructType = definedStructs.get("nebula.core.Object");
			if (objectStructType != null)
			{
				return LLVMConstPointerNull(LLVMPointerType(objectStructType, 0));
			}
			return LLVMConstPointerNull(LLVMPointerType(LLVMInt8TypeInContext(context), 0));
		}
		return null;
	}

	@Override
	public LLVMValueRef visitBinaryExpression(BinaryExpression expression)
	{
		String op = expression.getOperator().getLexeme();
		Debug.log("Visiting BinaryExpression: %s", op);
		Debug.indent();
		try
		{
			Type leftType = expression.getLeft().getResolvedType();
			Type rightType = expression.getRight().getResolvedType();

			// --- Handle String Concatenation ---
			// This block checks for the '+' operator where at least one operand is a String.
			ClassSymbol stringClass = semanticAnalyzer.getDeclaredClasses().get("nebula.core.String");
			if (op.equals("+") && (leftType.equals(stringClass.getType()) || rightType.equals(stringClass.getType())))
			{
				Debug.log("-> Detected string concatenation.");

				LLVMValueRef leftVal = expression.getLeft().accept(this);
				LLVMValueRef rightVal = expression.getRight().accept(this);

				// Convert non-string operands to strings
				if (!leftType.equals(stringClass.getType()))
				{
					leftVal = callToString(leftVal, leftType);
				}
				if (!rightType.equals(stringClass.getType()))
				{
					rightVal = callToString(rightVal, rightType);
				}

				if (leftVal == null || rightVal == null)
				{
					return null;
				}

				// Call the C++ runtime helper for concatenation
				LLVMValueRef concatFunc = LLVMGetNamedFunction(module, "string_concat");
				LLVMTypeRef concatFuncType = LLVMGlobalGetValueType(concatFunc);
				PointerPointer<LLVMValueRef> args = new PointerPointer<>(leftVal, rightVal);

				return LLVMBuildCall2(builder, concatFuncType, concatFunc, args, 2, "concat_str");
			}

			// --- LOGICAL OPERATORs ---
			// Handles short-circuiting for '&&' and '||' operators.
			if (op.equals("&&") || op.equals("||"))
			{
				Debug.log("-> Operator: '%s', isFloat: false", op);

				// Get the current function and basic block to build the branching logic.
				LLVMValueRef function = currentFunction;
				LLVMBasicBlockRef startBlock = LLVMGetInsertBlock(builder);

				// Create new basic blocks for the short-circuit logic.
				LLVMBasicBlockRef rhsBlock = LLVMAppendBasicBlockInContext(context, function, "shortcircuit_rhs");
				LLVMBasicBlockRef endBlock = LLVMAppendBasicBlockInContext(context, function, "shortcircuit_end");

				// 1. Evaluate the left-hand side.
				LLVMValueRef leftVal = expression.getLeft().accept(this);

				// 2. Create the conditional branch.
				if (op.equals("&&"))
				{
					// If left is false, jump directly to the end. Otherwise, evaluate the right side.
					LLVMBuildCondBr(builder, leftVal, rhsBlock, endBlock);
				}
				else
				{ // op is "||"
					// If left is true, jump directly to the end. Otherwise, evaluate the right side.
					LLVMBuildCondBr(builder, leftVal, endBlock, rhsBlock);
				}

				// 3. Generate code for the right-hand side block.
				LLVMPositionBuilderAtEnd(builder, rhsBlock);
				LLVMValueRef rightVal = expression.getRight().accept(this);
				LLVMBasicBlockRef rhsFinalBlock = LLVMGetInsertBlock(builder); // The block where RHS evaluation finished.
				LLVMBuildBr(builder, endBlock); // After evaluating RHS, jump to the end.

				// 4. Generate the end block with a PHI node to combine the results.
				LLVMPositionBuilderAtEnd(builder, endBlock);
				LLVMValueRef phi = LLVMBuildPhi(builder, LLVMInt1TypeInContext(context), "logic_result");

				// Define the incoming values for the PHI node.
				LLVMValueRef[] incomingValues = new LLVMValueRef[2];
				LLVMBasicBlockRef[] incomingBlocks = new LLVMBasicBlockRef[2];

				if (op.equals("&&"))
				{
					// If we came from startBlock, the result is false (0).
					incomingValues[0] = LLVMConstInt(LLVMInt1TypeInContext(context), 0, 0);
					incomingBlocks[0] = startBlock;
					// If we came from rhsBlock, the result is whatever the RHS evaluated to.
					incomingValues[1] = rightVal;
					incomingBlocks[1] = rhsFinalBlock;
				}
				else
				{ // op is "||"
					// If we came from startBlock, the result is true (1).
					incomingValues[0] = LLVMConstInt(LLVMInt1TypeInContext(context), 1, 0);
					incomingBlocks[0] = startBlock;
					// If we came from rhsBlock, the result is whatever the RHS evaluated to.
					incomingValues[1] = rightVal;
					incomingBlocks[1] = rhsFinalBlock;
				}

				PointerPointer<LLVMValueRef> phiValues = new PointerPointer<>(incomingValues);
				PointerPointer<LLVMBasicBlockRef> phiBlocks = new PointerPointer<>(incomingBlocks);

				LLVMAddIncoming(phi, phiValues, phiBlocks, 2);
				return phi;
			}
			// --- END: LOGICAL OPERATOR FIX ---

			// --- POWER OPERATOR LOGIC ---
			// Handles the '**' operator using the llvm.powi intrinsic.
			if (op.equals("**"))
			{
				String powiFunctionName = "llvm.powi.f64.i32";
				LLVMValueRef powiFunc = LLVMGetNamedFunction(module, powiFunctionName);
				LLVMTypeRef powiFuncType;

				if (powiFunc == null)
				{
					LLVMTypeRef doubleType = LLVMDoubleTypeInContext(context);
					LLVMTypeRef i32Type = LLVMInt32TypeInContext(context);
					PointerPointer<LLVMTypeRef> paramTypes = new PointerPointer<>(doubleType, i32Type);
					powiFuncType = LLVMFunctionType(doubleType, paramTypes, 2, 0);
					powiFunc = LLVMAddFunction(module, powiFunctionName, powiFuncType);
				}
				else
				{
					powiFuncType = LLVMGlobalGetValueType(powiFunc);
				}

				LLVMValueRef left = expression.getLeft().accept(this);
				LLVMValueRef right = expression.getRight().accept(this);

				if (left == null || right == null)
				{
					return null;
				}

				LLVMValueRef leftDouble = LLVMBuildSIToFP(builder, left, LLVMDoubleTypeInContext(context), "base_to_double");
				PointerPointer<LLVMValueRef> args = new PointerPointer<>(leftDouble, right);
				LLVMValueRef resultDouble = LLVMBuildCall2(builder, powiFuncType, powiFunc, args, 2, "powitmp");
				return LLVMBuildFPToSI(builder, resultDouble, LLVMInt32TypeInContext(context), "powi_to_int");
			}

			// --- REGULAR BINARY OPERATOR LOGIC ---
			// Handles all other standard binary operators.
			LLVMValueRef left = expression.getLeft().accept(this);
			LLVMValueRef right = expression.getRight().accept(this);

			if (left == null || right == null)
			{
				Debug.log("-> One or both operands are null. Aborting.");
				return null;
			}

			// --- START OF FIX: NUMERIC PROMOTION ---
			LLVMTypeRef leftLLVMType = LLVMTypeOf(left);
			LLVMTypeRef rightLLVMType = LLVMTypeOf(right);

			// Check if we are dealing with two different integer types.
			if (LLVMGetTypeKind(leftLLVMType) == LLVMIntegerTypeKind && LLVMGetTypeKind(rightLLVMType) == LLVMIntegerTypeKind &&  !leftLLVMType.equals(rightLLVMType))
			{

				Debug.log("-> Performing integer promotion for binary operation.");
				int leftWidth = LLVMGetIntTypeWidth(leftLLVMType);
				int rightWidth = LLVMGetIntTypeWidth(rightLLVMType);

				if (leftWidth > rightWidth)
				{
					// Promote the right operand to match the left's type.
					Debug.log("--> Promoting right operand from i%d to i%d.", rightWidth, leftWidth);
					right = LLVMBuildSExt(builder, right, leftLLVMType, "promoted_rhs");
				}
				else
				{
					// Promote the left operand to match the right's type.
					Debug.log("--> Promoting left operand from i%d to i%d.", leftWidth, rightWidth);
					left = LLVMBuildSExt(builder, left, rightLLVMType, "promoted_lhs");
				}
			}
			// --- END OF FIX ---

			boolean isFloat = leftType.equals(PrimitiveType.DOUBLE) || leftType.equals(PrimitiveType.FLOAT);
			Debug.log("-> Operator: '%s', isFloat: %b", op, isFloat);
			LLVMValueRef result;

			switch (op)
			{
				case "+":
					result = isFloat ? LLVMBuildFAdd(builder, left, right, "faddtmp") : LLVMBuildAdd(builder, left, right, "addtmp");
					break;
				case "-":
					result = isFloat ? LLVMBuildFSub(builder, left, right, "fsubtmp") : LLVMBuildSub(builder, left, right, "subtmp");
					break;
				case "*":
					result = isFloat ? LLVMBuildFMul(builder, left, right, "fmultmp") : LLVMBuildMul(builder, left, right, "multmp");
					break;
				case "/":
					result = isFloat ? LLVMBuildFDiv(builder, left, right, "fdivtmp") : LLVMBuildSDiv(builder, left, right, "sdivtmp");
					break;
				case "%":
					result = LLVMBuildSRem(builder, left, right, "sremtmp");
					break;
				case "==":
					result = isFloat ? LLVMBuildFCmp(builder, LLVMRealOEQ, left, right, "feqtmp") : LLVMBuildICmp(builder, LLVMIntEQ, left, right, "ieqtmp");
					break;
				case "!=":
					result = isFloat ? LLVMBuildFCmp(builder, LLVMRealONE, left, right, "fnetmp") : LLVMBuildICmp(builder, LLVMIntNE, left, right, "inetmp");
					break;
				case "<":
					result = isFloat ? LLVMBuildFCmp(builder, LLVMRealOLT, left, right, "flttmp") : LLVMBuildICmp(builder, LLVMIntSLT, left, right, "ilttmp");
					break;
				case "<=":
					result = isFloat ? LLVMBuildFCmp(builder, LLVMRealOLE, left, right, "fletmp") : LLVMBuildICmp(builder, LLVMIntSLE, left, right, "iletmp");
					break;
				case ">":
					result = isFloat ? LLVMBuildFCmp(builder, LLVMRealOGT, left, right, "fgttmp") : LLVMBuildICmp(builder, LLVMIntSGT, left, right, "igttmp");
					break;
				case ">=":
					result = isFloat ? LLVMBuildFCmp(builder, LLVMRealOGE, left, right, "fgetmp") : LLVMBuildICmp(builder, LLVMIntSGE, left, right, "igetmp");
					break;
				case "&":
					result = LLVMBuildAnd(builder, left, right, "andtmp");
					break;
				case "|":
					result = LLVMBuildOr(builder, left, right, "ortmp");
					break;
				case "^":
					result = LLVMBuildXor(builder, left, right, "xortmp");
					break;
				case "<<":
					result = LLVMBuildShl(builder, left, right, "shltmp");
					break;
				case ">>":
					result = LLVMBuildAShr(builder, left, right, "ashrtmp");
					break;
				default:
					Debug.log("-> Unhandled binary operator: %s", op);
					return null;
			}
			Debug.log("-> Generated instruction for '%s'", op);
			return result;
		}
		finally
		{
			Debug.dedent();
		}
	}


	@Override
	public LLVMValueRef visitUnaryExpression(UnaryExpression expression)
	{
		String op = expression.getOperator().getLexeme();
		Debug.log("Visiting UnaryExpression: %s", op);
		Debug.indent();
		try
		{
			if (op.equals("++") || op.equals("--"))
			{
				this.isLValueContext = true;
				Type targetPtrNebType = expression.getRight().getResolvedType();
				LLVMValueRef targetPtr = expression.getRight().accept(this);
				this.isLValueContext = false;

				if (targetPtr == null)
				{
					Debug.log("-> Target pointer is null. Aborting.");
					return null;
				}

				LLVMTypeRef targetPtrType = getLLVMType(targetPtrNebType);
				LLVMTypeRef valueType = LLVMGetElementType(targetPtrType);

				LLVMValueRef currentValue = LLVMBuildLoad2(builder, valueType, targetPtr, "loadtmp");
				LLVMValueRef result;

				if (expression.getResolvedType().isReferenceType())
				{
					Debug.log("-> Performing GEP for pointer arithmetic.");
					LLVMValueRef index = op.equals("++") ? LLVMConstInt(LLVMInt64TypeInContext(context), 1, 1) : LLVMConstInt(LLVMInt64TypeInContext(context), -1, 1);
					result = LLVMBuildGEP2(builder, LLVMGetElementType(valueType), currentValue, new PointerPointer<>(index), 1, "pre_gep_ptr");
				}
				else if (expression.getRight().getResolvedType().isFloatingPoint())
				{
					LLVMValueRef one = LLVMConstReal(valueType, 1.0);
					result = op.equals("++") ? LLVMBuildFAdd(builder, currentValue, one, "f_incrtmp") : LLVMBuildFSub(builder, currentValue, one, "f_decrtmp");
					Debug.log("-> Generated floating point instruction.");
				}
				else
				{
					LLVMValueRef one = LLVMConstInt(valueType, 1, 1);
					result = op.equals("++") ? LLVMBuildAdd(builder, currentValue, one, "incrtmp") : LLVMBuildSub(builder, currentValue, one, "decrtmp");
					Debug.log("-> Generated integer instruction.");
				}

				LLVMBuildStore(builder, result, targetPtr);
				return result;
			}

			LLVMValueRef operand = expression.getRight().accept(this);
			if (operand == null)
			{
				Debug.log("-> Operand is null. Aborting.");
				return null;
			}

			Type operandType = expression.getRight().getResolvedType();
			boolean isFloat = operandType.isFloatingPoint();
			Debug.log("-> Operator: '%s', isFloat: %b", op, isFloat);

			switch (op)
			{
				case "-":
					return isFloat ? LLVMBuildFNeg(builder, operand, "fnegtmp") : LLVMBuildNeg(builder, operand, "negtmp");
				case "!":
					LLVMValueRef zero = LLVMConstInt(LLVMInt1TypeInContext(context), 0, 0);
					return LLVMBuildICmp(builder, LLVMIntEQ, operand, zero, "nottmp");
				case "~":
					return LLVMBuildNot(builder, operand, "bitnottmp");
			}
			return null;
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitAssignmentExpression(AssignmentExpression expression)
	{
		String op = expression.getOperator().getLexeme();
		Debug.log("Visiting AssignmentExpression: %s", op);
		Debug.indent();

		try
		{
			Symbol resolvedSymbol = expression.getResolvedSymbol();

			// --- FIX START: Handle Property Setters ---
			if (op.equals("=") && resolvedSymbol instanceof MethodSymbol)
			{
				MethodSymbol setterSymbol = (MethodSymbol) resolvedSymbol;
				Debug.log("-> Detected property assignment. Generating setter call to @%s.", setterSymbol.getMangledName());

				if (!(expression.getTarget() instanceof DotExpression))
				{
					Debug.log("-> INTERNAL ERROR: Setter call target is not a DotExpression.");
					return null;
				}
				DotExpression targetDotExpr = (DotExpression) expression.getTarget();

				// Get the object instance (e.g., 'p' in 'p.Health = value')
				LLVMValueRef objectPtr = targetDotExpr.getLeft().accept(this);
				// Get the value to assign
				LLVMValueRef valueToAssign = expression.getValue().accept(this);

				if (objectPtr == null || valueToAssign == null)
				{
					return null;
				}

				LLVMValueRef setterFunc = LLVMGetNamedFunction(module, setterSymbol.getMangledName());
				if (setterFunc.isNull())
				{
					Debug.log("-> FATAL: Could not find LLVM function for setter '%s'.", setterSymbol.getMangledName());
					return null;
				}
				Type setterFuncNebType = setterSymbol.getType();
				LLVMTypeRef setterFuncType = getLLVMType(setterFuncNebType);

				// Build the argument list: (this, value)
				PointerPointer<LLVMValueRef> args = new PointerPointer<>(objectPtr, valueToAssign);

				LLVMBuildCall2(builder, setterFuncType, setterFunc, args, 2, "");
				return valueToAssign;
			}
			// --- FIX END ---

			// CASE 2: This is a standard variable/field assignment.
			Debug.log("-> Performing standard variable/field assignment.");
			this.isLValueContext = true;
			LLVMValueRef targetPtr = expression.getTarget().accept(this);
			this.isLValueContext = false;

			if (targetPtr == null)
			{
				return null;
			}

			LLVMValueRef valueToAssign = expression.getValue().accept(this);
			if (valueToAssign == null)
			{
				return null;
			}

			LLVMValueRef finalValue;

			if (op.equals("="))
			{
				finalValue = valueToAssign;
			}
			else
			{
				// Logic for compound assignments (+=, -=, etc.)
				// --- FIX START ---
				// The targetPtr is the address of the variable (e.g., i32*).
				// The valueType is the type stored at that address (e.g., i32).
				LLVMTypeRef valueType = getLLVMType(expression.getTarget().getResolvedType());
				if (expression.getResolvedType().isReferenceType())
				{
					// For pointers (like strings), we should be loading the pointer value.
					valueType = LLVMGetElementType(LLVMTypeOf(targetPtr));
				}
				// --- FIX END ---

				LLVMValueRef currentValue = LLVMBuildLoad2(builder, valueType, targetPtr, "loadtmp");
				String baseOp = op.substring(0, op.length() - 1);
				boolean isFloat = expression.getTarget().getResolvedType().isFloatingPoint();

				// --- START OF FIX: NUMERIC PROMOTION ---
				LLVMTypeRef leftLLVMType = LLVMTypeOf(currentValue);
				LLVMTypeRef rightLLVMType = LLVMTypeOf(valueToAssign);

				if (LLVMGetTypeKind(leftLLVMType) == LLVMIntegerTypeKind &&
						LLVMGetTypeKind(rightLLVMType) == LLVMIntegerTypeKind &&
						!leftLLVMType.equals(rightLLVMType)) {

					Debug.log("-> Performing integer promotion for compound assignment.");
					int leftWidth = LLVMGetIntTypeWidth(leftLLVMType);
					int rightWidth = LLVMGetIntTypeWidth(rightLLVMType);

					if (leftWidth > rightWidth) {
						Debug.log("--> Promoting right operand from i%d to i%d.", rightWidth, leftWidth);
						valueToAssign = LLVMBuildSExt(builder, valueToAssign, leftLLVMType, "promoted_rhs");
					} else {
						Debug.log("--> Promoting left operand from i%d to i%d.", leftWidth, rightWidth);
						currentValue = LLVMBuildSExt(builder, currentValue, rightLLVMType, "promoted_lhs");
					}
				}
				// --- END OF FIX ---

				finalValue = switch (baseOp)
				{
					case "+" ->
							isFloat ? LLVMBuildFAdd(builder, currentValue, valueToAssign, "faddtmp") : LLVMBuildAdd(builder, currentValue, valueToAssign, "addtmp");
					case "-" ->
							isFloat ? LLVMBuildFSub(builder, currentValue, valueToAssign, "fsubtmp") : LLVMBuildSub(builder, currentValue, valueToAssign, "subtmp");
					case "*" ->
							isFloat ? LLVMBuildFMul(builder, currentValue, valueToAssign, "fmultmp") : LLVMBuildMul(builder, currentValue, valueToAssign, "multmp");
					case "/" ->
							isFloat ? LLVMBuildFDiv(builder, currentValue, valueToAssign, "fdivtmp") : LLVMBuildSDiv(builder, currentValue, valueToAssign, "sdivtmp");
					case "%" -> LLVMBuildSRem(builder, currentValue, valueToAssign, "sremtmp");
					case "&" -> LLVMBuildAnd(builder, currentValue, valueToAssign, "andtmp");
					case "|" -> LLVMBuildOr(builder, currentValue, valueToAssign, "ortmp");
					case "^" -> LLVMBuildXor(builder, currentValue, valueToAssign, "xortmp");
					case "<<" -> LLVMBuildShl(builder, currentValue, valueToAssign, "shltmp");
					case ">>" -> LLVMBuildAShr(builder, currentValue, valueToAssign, "ashrtmp");
					// Power operator (**) needs special handling if you support it for compound assignment
					default -> null;
				};
			}

			if (finalValue != null)
			{
				LLVMBuildStore(builder, finalValue, targetPtr);
			}

			return finalValue;
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitIdentifierExpression(IdentifierExpression expression)
	{
		String name = expression.getName().getLexeme();
		Debug.log("Visiting IdentifierExpression: %s", name);
		Debug.indent();
		try
		{
			Symbol symbol = expression.getResolvedSymbol();
			LLVMValueRef varLocation = findVariable(name);

			// --- START OF FIX ---
			// If the variable is not in local scopes, check if it's a member field.
			if (varLocation == null && currentMethodSymbol != null && !currentMethodSymbol.isStatic())
			{
				Debug.log("-> Not a local variable. Checking for implicit 'this' member access...");
				ClassSymbol ownerClass = currentMethodSymbol.getOwnerClass();
				int fieldIndex = ownerClass.getFieldIndex(name);

				if (fieldIndex != -1)
				{
					// It's a field! Generate a GEP from the 'this' pointer.
					LLVMValueRef thisPtr = LLVMGetParam(currentFunction, 0);
					LLVMTypeRef structType = definedStructs.get(ownerClass.getFqn());
					varLocation = LLVMBuildStructGEP2(builder, structType, thisPtr, fieldIndex, name + "_ptr");
				}
			}
			// --- END OF FIX ---

			if (varLocation == null)
			{
				Debug.log("-> Could not find LLVM value for variable '%s'.", name);
				return null;
			}

			Type nebulaType = expression.getResolvedType();

			// --- START OF FIX ---
			// If the variable is a stack-allocated class instance, we almost always want its pointer,
			// not its value (unless it's being assigned, which is handled by the caller).
			// Returning the pointer directly prevents loading the whole struct into a register.
			boolean isStackObject = nebulaType instanceof ClassType &&
					semanticAnalyzer.getStackAllocatedClasses().contains(((ClassType) nebulaType).getFqn());

			if (isLValueContext || isStackObject)
			{
				// Return the POINTER for l-values OR if it's a stack object being read (r-value).
				Debug.log("-> In l-value context or stack object context. Returning pointer.");
				return varLocation;
			}
			else
			{
				// This path is now only for primitives and heap pointers.
				Debug.log("-> In r-value context for primitive/heap pointer. Loading value.");
				LLVMTypeRef loadType = getLLVMType(nebulaType);
				return LLVMBuildLoad2(builder, loadType, varLocation, name + "_val");
			}
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitPostfixUnaryExpression(PostfixUnaryExpression expression)
	{
		String op = expression.getOperator().getLexeme();
		Debug.log("Visiting PostfixUnaryExpression: %s", op);
		Debug.indent();
		try
		{
			this.isLValueContext = true;
			LLVMValueRef targetPtr = expression.getOperand().accept(this);
			this.isLValueContext = false;

			if (targetPtr == null)
			{
				Debug.log("-> Target pointer is null. Aborting.");
				return null;
			}

			LLVMTypeRef valueType = getLLVMType(expression.getResolvedType());

			LLVMValueRef originalValue = LLVMBuildLoad2(builder, valueType, targetPtr, "postfix_orig");
			LLVMValueRef newValue;

			if (expression.getResolvedType().isFloatingPoint())
			{
				LLVMValueRef one = LLVMConstReal(valueType, 1.0);
				newValue = op.equals("++")
						? LLVMBuildFAdd(builder, originalValue, one, "f_incrtmp")
						: LLVMBuildFSub(builder, originalValue, one, "f_decrtmp");
			}
			else
			{ // guaranteed numeric int by analyzer
				LLVMValueRef one = LLVMConstInt(valueType, 1, 0);
				newValue = op.equals("++")
						? LLVMBuildAdd(builder, originalValue, one, "incrtmp")
						: LLVMBuildSub(builder, originalValue, one, "decrtmp");
			}

			LLVMBuildStore(builder, newValue, targetPtr);

			return originalValue; // postfix returns old value
		}
		finally
		{
			Debug.dedent();
		}
	}


	@Override
	public LLVMValueRef visitThisExpression(ThisExpression expression)
	{
		Debug.log("Visiting ThisExpression");
		Debug.indent();
		LLVMValueRef thisPtr = LLVMGetParam(currentFunction, 0);
		Debug.log("-> Resolved 'this' to the first function parameter.");
		Debug.dedent();
		return thisPtr;
	}

	@Override
	public LLVMValueRef visitNewExpression(NewExpression expression)
	{
		Debug.log("Visiting NewExpression for type: %s", expression.getClassName().toString());
		Debug.indent();
		try
		{
			ClassType classType = (ClassType) expression.getResolvedType();
			LLVMTypeRef structType = definedStructs.get(classType.getFqn());
			if (structType == null)
			{
				Debug.log("-> ERROR: Struct type for '%s' not found.", classType.getFqn());
				return null;
			}

			LLVMValueRef typedPtr; // This will hold the pointer to the new object's memory.

			// --- FIX START ---
			// Check if we were given a stack address to use.
			if (this.currentAllocaForStackObject != null)
			{
				// Yes: the object lives on the stack. The pointer is the address we were given.
				Debug.log("-> Using pre-allocated stack memory for the object.");
				typedPtr = this.currentAllocaForStackObject;
			}
			else
			{
				// No: the object lives on the heap. Fall back to malloc.
				long sizeInBytes = LLVMSizeOfTypeInBits(targetData, structType) / 8;
				Debug.log("-> Allocating %d bytes on the heap for the object.", sizeInBytes);
				LLVMValueRef sizeVal = LLVMConstInt(LLVMInt64TypeInContext(context), sizeInBytes, 0);

				PointerPointer<LLVMValueRef> mallocArgs = new PointerPointer<>(1);
				mallocArgs.put(0, sizeVal);
				LLVMTypeRef mallocFuncType = LLVMGlobalGetValueType(mallocFunc);
				LLVMValueRef rawPtr = LLVMBuildCall2(builder, mallocFuncType, mallocFunc, mallocArgs, 1, "raw_ptr");

				LLVMTypeRef structPtrType = LLVMPointerType(structType, 0);
				typedPtr = LLVMBuildBitCast(builder, rawPtr, structPtrType, "typed_ptr");
				Debug.log("-> Casted malloc pointer to %s", LLVMPrintTypeToString(structPtrType).getString());
			}
			// --- END OF NEW LOGIC ---

			MethodSymbol constructorSymbol = expression.getResolvedConstructor();
			if (constructorSymbol != null)
			{
				Debug.log("-> Calling constructor: %s", constructorSymbol.getMangledName());
				LLVMValueRef constructorFunc = LLVMGetNamedFunction(module, constructorSymbol.getMangledName());
				if (constructorFunc != null)
				{
					List<LLVMValueRef> args = new ArrayList<>();
					args.add(typedPtr); // Pass the object pointer (which is now either stack or heap)
					for (Expression argExpr : expression.getArguments())
					{
						args.add(argExpr.accept(this));
					}

					LLVMTypeRef constructorFuncType = LLVMGlobalGetValueType(constructorFunc);

					// Handle zero-argument constructor calls correctly
					if (args.isEmpty())
					{
						LLVMBuildCall2(builder, constructorFuncType, constructorFunc, null, 0, "");
					}
					else
					{
						PointerPointer<LLVMValueRef> llvmArgs = new PointerPointer<>(args.size());
						for (int i = 0; i < args.size(); i++)
						{
							llvmArgs.put(i, args.get(i));
						}
						LLVMBuildCall2(builder, constructorFuncType, constructorFunc, llvmArgs, args.size(), "");
					}
					Debug.log("-> Generated call to constructor.");

				}
				else
				{
					Debug.log("-> WARNING: Could not find LLVM function for constructor '%s'.", constructorSymbol.getMangledName());
				}
			}

			return typedPtr;
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitDotExpression(DotExpression expression)
	{
		String memberName = expression.getMemberName().getLexeme();
		Debug.log("Visiting DotExpression, accessing member: %s", memberName);
		Debug.indent();
		try
		{
			// Step 1: Get a pointer to the object on the left of the '.'
			LLVMValueRef objectRef = expression.getLeft().accept(this);
			if (objectRef == null)
			{
				return null;
			}

			LLVMValueRef objectPtr;
			if (LLVMGetTypeKind(LLVMTypeOf(objectRef)) == LLVMPointerTypeKind)
			{
				objectPtr = objectRef;
			}
			else
			{
				objectPtr = LLVMBuildAlloca(builder, LLVMTypeOf(objectRef), "dot_lhs_temp");
				LLVMBuildStore(builder, objectRef, objectPtr);
			}

			Symbol resolvedSymbol = expression.getResolvedSymbol();
			ClassType classType = (ClassType) expression.getLeft().getResolvedType();
			ClassSymbol classSymbol = classType.classSymbol;
			LLVMTypeRef structType = definedStructs.get(classSymbol.getFqn());

			// Step 2: Choose logic based on L-Value (write) or R-Value (read) context
			if (isLValueContext)
			{
				Debug.log("-> In l-value context, returning pointer to member.");
				int fieldIndex = -1;
				if (resolvedSymbol instanceof PropertySymbol)
				{
					fieldIndex = classSymbol.getBackingFieldIndexForProperty(memberName);
				}
				else if (resolvedSymbol instanceof VariableSymbol)
				{
					fieldIndex = classSymbol.getFieldIndex(memberName);
				}
				if (fieldIndex == -1)
				{
					return null;
				}
				return LLVMBuildStructGEP2(builder, structType, objectPtr, fieldIndex, memberName + "_ptr");
			}
			else
			{
				// R-VALUE CONTEXT: We are reading a value.
				Debug.log("-> In r-value context, generating a read operation.");
				Symbol memberSymbol = expression.getResolvedSymbol();
				ClassType ownerType = (ClassType) expression.getLeft().getResolvedType();

				// --- START OF FIX ---

				// CASE 1: The member is a method, like .length
				if (memberSymbol instanceof MethodSymbol method)
				{
					String mangledName = method.getMangledName();

					// Subcase 1.1: Special handling for string.length
					if (mangledName.equals("length") && ownerType.getFqn().equals("nebula.core.String"))
					{
						Debug.log("--> Special case: redirecting to runtime function @string_length");
						LLVMValueRef func = LLVMGetNamedFunction(module, "string_length");
						if (func == null || func.isNull())
						{
							Debug.log("--> FATAL: @string_length runtime function not found!");
							return null;
						}

						LLVMTypeRef funcType = LLVMGlobalGetValueType(func);
						// Use the safe, two-step PointerPointer creation
						PointerPointer<LLVMValueRef> args = new PointerPointer<>(1);
						args.put(0, objectPtr);

						return LLVMBuildCall2(builder, funcType, func, args, 1, "length_val");
					}

					// Subcase 1.2: A regular method is being accessed for a later call (e.g., in `obj.myMethod()`)
					// We don't generate a call here. The CallExpression visitor will handle it.
					// We just need to return the object pointer ('this') for that future call.
					Debug.log("--> Member is a standard method. Returning object pointer for call context.");
					return objectPtr;
				}

				// CASE 2: The member is a property with a getter
				if (memberSymbol instanceof PropertySymbol property)
				{
					Debug.log("--> Member is a property: %s", property.getName());
					MethodSymbol getter = property.getGetter();
					if (getter != null)
					{
						String mangledName = getter.getMangledName();
						Debug.log("--> Generating getter call to @%s", mangledName);
						LLVMValueRef func = LLVMGetNamedFunction(module, mangledName);
						if (func == null || func.isNull())
						{
							Debug.log("--> FATAL: getter function @%s not found!", mangledName);
							return null;
						}

						LLVMTypeRef funcType = LLVMGlobalGetValueType(func);
						// Use the safe, two-step PointerPointer creation
						PointerPointer<LLVMValueRef> args = new PointerPointer<>(1);
						args.put(0, objectPtr);

						return LLVMBuildCall2(builder, funcType, func, args, 1, property.getName() + "_val");
					}
				}

				// CASE 3: The member is a simple field
				if (memberSymbol instanceof VariableSymbol)
				{
					Debug.log("--> Member is a field: %s", memberName);
					classSymbol = ownerType.classSymbol;
					int fieldIndex = classSymbol.getFieldIndex(memberName);
					if (fieldIndex == -1)
					{
						return null;
					}
					LLVMValueRef fieldPtr = LLVMBuildStructGEP2(builder, structType, objectPtr, fieldIndex, memberName + "_ptr");
					LLVMTypeRef fieldLLVMType = getLLVMType(memberSymbol.getType());
					return LLVMBuildLoad2(builder, fieldLLVMType, fieldPtr, memberName + "_val");
				}

				Debug.log("-> Unhandled r-value member access for '%s'. Returning object pointer.", memberName);
				return objectPtr;

				// --- END OF FIX ---
			}
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitCallExpression(CallExpression expression)
	{
		Debug.log("Visiting CallExpression");
		Debug.indent();
		try
		{
			Symbol symbol = expression.getResolvedSymbol();
			if (!(symbol instanceof MethodSymbol))
			{
				Debug.log("-> ERROR: Resolved symbol for call is not a MethodSymbol.");
				return null;
			}
			MethodSymbol methodSymbol = (MethodSymbol) symbol;

			// --- NEW: Handle wrapper methods directly ---
			if (methodSymbol.isWrapper())
			{
				Debug.log("-> Call to WRAPPER method: %s", methodSymbol.getMangledName());

				// Specific handler for Console.println overloads
				if (methodSymbol.getOwnerClass().getFqn().equals("nebula.io.Console") && methodSymbol.getName().equals("println"))
				{
					if (expression.getArguments().size() != 1)
					{
						return null; // Should be caught by analyzer
					}

					Expression argExpr = expression.getArguments().get(0);
					LLVMValueRef argValue = argExpr.accept(this);
					Type argType = argExpr.getResolvedType();

					LLVMValueRef valueToPrint;
					String formatString = "";

					if (argType instanceof ClassType && ((ClassType) argType).getFqn().equals("nebula.core.String"))
					{
						formatString = "%s\n";
						// argValue is now a POINTER to the String struct.
						// We need to get the 'c_str' field from inside it.
						LLVMTypeRef stringStructType = definedStructs.get("nebula.core.String");
						LLVMValueRef fieldPtr = LLVMBuildStructGEP2(builder, stringStructType, argValue, 0, "data_ptr");

						// The type of the field itself is a pointer to char (i8*).
						LLVMTypeRef i8PtrType = LLVMPointerType(LLVMInt8TypeInContext(context), 0);
						valueToPrint = LLVMBuildLoad2(builder, i8PtrType, fieldPtr, "raw_c_str");
					}
					else if (argType.equals(PrimitiveType.INT32))
					{
						formatString = "%d\n";
						valueToPrint = argValue;
					}
					else if (argType.equals(PrimitiveType.DOUBLE))
					{
						formatString = "%f\n";
						valueToPrint = argValue;
					}
					else if (argType.equals(PrimitiveType.BOOL))
					{
						formatString = "%s\n";
						LLVMValueRef trueStr = LLVMBuildGlobalStringPtr(builder, "true", ".true");
						LLVMValueRef falseStr = LLVMBuildGlobalStringPtr(builder, "false", ".false");
						valueToPrint = LLVMBuildSelect(builder, argValue, trueStr, falseStr, "bool_str");
					}
					else
					{
						Debug.log("-> ERROR: Unhandled wrapper type for println: %s", argType.getName());
						return null;
					}

					LLVMValueRef formatStrRef = LLVMBuildGlobalStringPtr(builder, formatString, ".fmt");

					// --- FIX START ---
// When building the call, ensure you create the PointerPointer correctly.
					PointerPointer<LLVMValueRef> printfArgs = new PointerPointer<>(formatStrRef, valueToPrint);
// --- FIX END ---

					LLVMTypeRef printfFuncType = LLVMGlobalGetValueType(printfFunc);

					// --- START: ADD THIS DEBUGGING CODE ---
					Debug.log("--- Preparing to call @printf ---");
					Debug.log("  -> Function Type:  %s", LLVMPrintTypeToString(printfFuncType).getString());
					Debug.log("  -> Function Value: %s", LLVMPrintValueToString(printfFunc).getString());
					Debug.log("  -> Arg 0 (format): %s", LLVMPrintValueToString(formatStrRef).getString());
					Debug.log("  -> Arg 1 (value):  %s", LLVMPrintValueToString(valueToPrint).getString());
					Debug.log("---------------------------------");
// --- END: ADD THIS DEBUGGING CODE ---

					return LLVMBuildCall2(builder, printfFuncType, printfFunc, printfArgs, 2, "");
				}

				Debug.log("-> ERROR: Unhandled wrapper method call.");
				return null;
			}

			// --- Existing logic for non-wrapper calls ---
			String mangledName = methodSymbol.getMangledName();
			LLVMValueRef function = LLVMGetNamedFunction(module, mangledName);
			if (function == null)
			{
				Debug.log("-> ERROR: Could not find declared LLVM function for '%s'.", mangledName);
				return null;
			}
			Debug.log("-> Found function: @%s", mangledName);

			List<LLVMValueRef> args = new ArrayList<>();
			if (!methodSymbol.isStatic())
			{
				// Correctly get the object instance pointer.
				// For a call like 'obj.method()', the callee is a DotExpression. We need to visit its left side.
				if (expression.getCallee() instanceof DotExpression)
				{
					DotExpression dotCallee = (DotExpression) expression.getCallee();
					LLVMValueRef objectInstancePtr = dotCallee.getLeft().accept(this);
					args.add(objectInstancePtr); // Add the 'this' pointer as the first argument
				}
				else
				{
					// This handles implicit 'this' calls (e.g., 'myMethod()' inside another method)
					LLVMValueRef thisPtr = LLVMGetParam(currentFunction, 0);
					args.add(thisPtr);
				}
			}

			// Now, add the rest of the arguments
			for (Expression argExpr : expression.getArguments())
			{
				args.add(argExpr.accept(this));
			}

			LLVMTypeRef functionType = LLVMGlobalGetValueType(function);
			String returnName = methodSymbol.getType().equals(PrimitiveType.VOID) ? "" : "calltmp";

			if (args.isEmpty())
			{
				// For functions with no arguments, pass a null PointerPointer.
				return LLVMBuildCall2(builder, functionType, function, null, 0, returnName);
			}
			else
			{
				// For functions with arguments, create and populate the PointerPointer.
				PointerPointer<LLVMValueRef> llvmArgs = new PointerPointer<>(args.size());
				for (int i = 0; i < args.size(); i++)
				{
					llvmArgs.put(i, args.get(i));
				}
				return LLVMBuildCall2(builder, functionType, function, llvmArgs, args.size(), returnName);
			}

		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitArrayAccessExpression(ArrayAccessExpression expression)
	{
		Debug.log("Visiting ArrayAccessExpression");
		Debug.indent();
		try
		{
			// 1. Get the pointer to the stack variable that holds the array pointer (e.g., address of 'resourcePool').
			// This is a pointer-to-a-pointer, like %SharedResource***.
			this.isLValueContext = true; // Ensure we get the pointer to the variable itself
			LLVMValueRef arrayAlloca = expression.getArray().accept(this);
			this.isLValueContext = false; // Reset context
			if (arrayAlloca == null)
			{
				Debug.log("-> ERROR: Array allocation pointer is null.");
				return null;
			}

			// 2. Load the actual array pointer from the stack variable.
			// This gives us the pointer to the array data on the heap (e.g., %SharedResource**).
			LLVMTypeRef arrayPtrType = LLVMGetElementType(LLVMTypeOf(arrayAlloca));
			LLVMValueRef arrayPtr = LLVMBuildLoad2(builder, arrayPtrType, arrayAlloca, "array_ptr");

			// 3. Get the index value.
			LLVMValueRef indexVal = expression.getIndex().accept(this);
			if (indexVal == null)
			{
				Debug.log("-> ERROR: Index value is null.");
				return null;
			}

			// 4. Create the GEP instruction.
			// Explicitly create the PointerPointer for the GEP indices to avoid ambiguity.
			PointerPointer<LLVMValueRef> indices = new PointerPointer<>(1);
			indices.put(0, indexVal);

			// --- START OF FIX ---
			// Robustly determine the element type from the Nebula AST's resolved type information
			// instead of relying on LLVMTypeOf on an intermediate value.
			ArrayType arrayNebulaType = (ArrayType) expression.getArray().getResolvedType();
			Type elementNebulaType = arrayNebulaType.getElementType();

			// getLLVMType will correctly return the LLVM type for an element in the array.
			// For an array of heap-allocated objects like SharedResource[], this will correctly
			// resolve to a pointer type (%SharedResource*). For an array of primitives like int[],
			// it will resolve to a value type (i32).
			LLVMTypeRef elementLLVMType = getLLVMType(elementNebulaType);
			// --- END OF FIX ---

			// Now, build the GEP with the reliably-determined element type.
			LLVMValueRef elementPtr = LLVMBuildGEP2(builder, elementLLVMType, arrayPtr, indices, 1, "elem_ptr");
			Debug.log("-> Generated GEP instruction to find element pointer.");


			// 5. Return either the pointer (for assignments) or the loaded value.
			if (isLValueContext)
			{
				Debug.log("-> In l-value context. Returning pointer to array element.");
				return elementPtr;
			}
			else
			{
				Debug.log("-> In r-value context. Loading value from array element pointer.");
				// Note: The type of the value we load is the same as the element type of the array.
				return LLVMBuildLoad2(builder, elementLLVMType, elementPtr, "elem_val");
			}
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitGroupingExpression(GroupingExpression expression)
	{
		Debug.log("Visiting GroupingExpression");
		Debug.indent();
		LLVMValueRef result = expression.getExpression().accept(this);
		Debug.dedent();
		return result;
	}

	@Override
	public LLVMValueRef visitCastExpression(CastExpression expression)
	{
		Debug.log("Visiting CastExpression");
		Debug.indent();
		try
		{
			LLVMValueRef operand = expression.getExpression().accept(this);
			if (operand == null)
			{
				return null;
			}

			Type targetNebulaType = expression.getResolvedType();
			Type sourceNebulaType = expression.getExpression().getResolvedType();

			LLVMTypeRef targetLLVMType = getLLVMType(targetNebulaType);
			LLVMTypeRef sourceLLVMType = getLLVMType(sourceNebulaType);

			if (targetLLVMType == null || sourceLLVMType == null)
			{
				return null;
			}

			Debug.log("-> Casting from %s to %s", sourceNebulaType.getName(), targetNebulaType.getName());

			boolean isSourceFloat = sourceNebulaType.equals(PrimitiveType.FLOAT) || sourceNebulaType.equals(PrimitiveType.DOUBLE);
			boolean isTargetFloat = targetNebulaType.equals(PrimitiveType.FLOAT) || targetNebulaType.equals(PrimitiveType.DOUBLE);

			if (isSourceFloat && !isTargetFloat)
			{
				Debug.log("-> Generating 'fptosi' instruction (float to signed int).");
				return LLVMBuildFPToSI(builder, operand, targetLLVMType, "fp_to_si_cast");
			}
			else if (!isSourceFloat && isTargetFloat)
			{
				Debug.log("-> Generating 'sitofp' instruction (signed int to float).");
				return LLVMBuildSIToFP(builder, operand, targetLLVMType, "si_to_fp_cast");
			}
			else if (!isSourceFloat && !isTargetFloat)
			{
				long targetSize = LLVMSizeOfTypeInBits(targetData, targetLLVMType);
				long sourceSize = LLVMSizeOfTypeInBits(targetData, sourceLLVMType);

				if (targetSize < sourceSize)
				{
					Debug.log("-> Generating 'trunc' instruction (narrowing int cast).");
					return LLVMBuildTrunc(builder, operand, targetLLVMType, "trunc_cast");
				}
				else if (targetSize > sourceSize)
				{
					Debug.log("-> Generating 'sext' instruction (sign-extending int cast).");
					return LLVMBuildSExt(builder, operand, targetLLVMType, "sext_cast");
				}
				else
				{
					return operand;
				}
			}
			else
			{
				return operand;
			}
		}
		finally
		{
			Debug.dedent();
		}
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitTernaryExpression(TernaryExpression expression)
	{
		return null;
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitIsExpression(IsExpression expression)
	{
		return null;
	}

	@Override
	public LLVMValueRef visitConstructorDeclaration(ConstructorDeclaration declaration)
	{
		Debug.log("Visiting ConstructorDeclaration: %s", declaration.getName().getLexeme());
		Debug.indent();
		try
		{
			MethodSymbol constructorSymbol = (MethodSymbol) declaration.getResolvedSymbol();
			this.currentMethodSymbol = (MethodSymbol) constructorSymbol;
			if (constructorSymbol == null)
			{
				return null;
			}

			String mangledName = constructorSymbol.getMangledName();
			LLVMValueRef function = LLVMGetNamedFunction(module, mangledName);
			if (function == null)
			{
				return null;
			}

			this.currentFunction = function;
			LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, function, "entry");
			LLVMPositionBuilderAtEnd(builder, entry);

			enterScope();

			LLVMValueRef thisPtr = LLVMGetParam(function, 0);
			LLVMSetValueName(thisPtr, "this");

			for (int i = 0; i < constructorSymbol.getParameterTypes().size(); i++)
			{
				LLVMValueRef param = LLVMGetParam(function, i + 1);
				String paramName = declaration.getParameters().get(i * 2 + 1).getLexeme();
				LLVMSetValueName(param, paramName);

				LLVMValueRef paramAlloca = LLVMBuildAlloca(builder, LLVMTypeOf(param), paramName + "_addr");
				LLVMBuildStore(builder, param, paramAlloca);
				scopes.peek().variables.put(paramName, paramAlloca);
			}

			if (declaration.getBody() != null)
			{
				declaration.getBody().accept(this);
			}

			if (LLVMGetBasicBlockTerminator(LLVMGetLastBasicBlock(function)) == null)
			{
				LLVMBuildRetVoid(builder);
			}

			exitScope();
			return function;
		}
		finally
		{
			Debug.dedent();
		}
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitFieldDeclaration(FieldDeclaration declaration)
	{
		return null;
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitPropertyDeclaration(PropertyDeclaration declaration)
	{
		PropertySymbol propSymbol = declaration.getResolvedSymbol();
		Debug.log("Visiting property declaration: %s.%s", declaration.getClass().getName(), propSymbol.getName());

		if (propSymbol != null)
		{
			// A property can have a getter, a setter, or both.
			// The semantic analyzer has already created the MethodSymbols for them.
			// Here, we just generate the actual code bodies for them.
			generateGetterBody(propSymbol);
			generateSetterBody(propSymbol);
		}
		return null;
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitIfStatement(IfStatement statement)
	{
		return null;
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitWhileStatement(WhileStatement statement)
	{
		return null;
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitForStatement(ForStatement statement)
	{
		Debug.log("Generating LLVM IR for 'for' loop...");
		Debug.indent();

		enterScope();

		LLVMBasicBlockRef loopEntryBlock = LLVMAppendBasicBlockInContext(context, currentFunction, "loop_entry");
		LLVMBasicBlockRef loopConditionBlock = LLVMAppendBasicBlockInContext(context, currentFunction, "loop_condition");
		LLVMBasicBlockRef loopBodyBlock = LLVMAppendBasicBlockInContext(context, currentFunction, "loop_body");
		LLVMBasicBlockRef loopAfterBlock = LLVMAppendBasicBlockInContext(context, currentFunction, "loop_after");

		// Branch from the current block to the loop entry block
		LLVMBuildBr(builder, loopEntryBlock);

		// --- Loop Initialization ---
		LLVMPositionBuilderAtEnd(builder, loopEntryBlock);
		if (statement.getInitializer() != null)
		{
			statement.getInitializer().accept(this);
		}
		// Now, jump to the condition check
		LLVMBuildBr(builder, loopConditionBlock);

		// --- Loop Condition Check ---
		LLVMPositionBuilderAtEnd(builder, loopConditionBlock);
		LLVMValueRef conditionValue = statement.getCondition().accept(this);
		// The condition must be a 1-bit integer (boolean)
		LLVMValueRef loopCondition = LLVMBuildICmp(builder, LLVMIntNE, conditionValue, LLVMConstInt(LLVMTypeOf(conditionValue), 0, 0), "loop_cond_bool");
		LLVMBuildCondBr(builder, loopCondition, loopBodyBlock, loopAfterBlock);

		// --- Loop Body and Increment ---
		LLVMPositionBuilderAtEnd(builder, loopBodyBlock);
		statement.getBody().accept(this);
		if (statement.getIncrement() != null)
		{
			statement.getIncrement().accept(this);
		}
		// After the body and increment, branch back to the condition check
		LLVMBuildBr(builder, loopConditionBlock);

		// --- Loop Exit ---
		LLVMPositionBuilderAtEnd(builder, loopAfterBlock);

		exitScope();
		Debug.dedent();
		Debug.log("LLVM IR for 'for' loop generated.");
		return null; // For a statement, we don't return a value
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitForEachStatement(ForEachStatement statement)
	{
		return null;
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitSwitchStatement(SwitchStatement statement)
	{
		return null;
	}

	@Override
	public LLVMValueRef visitArrayCreationExpression(ArrayCreationExpression expression)
	{
		Debug.log("Visiting ArrayCreationExpression");
		Debug.indent();
		try
		{
			ArrayType arrayType = (ArrayType) expression.getResolvedType();
			Type nebulaElementType = arrayType.getElementType();
			LLVMTypeRef llvmElementType;

			// Determine the type of elements the array will hold.
			// For objects, the array holds POINTERS. For primitives, it holds VALUES.
			if (nebulaElementType instanceof ClassType)
			{
				LLVMTypeRef structType = definedStructs.get(((ClassType) nebulaElementType).getFqn());
				llvmElementType = LLVMPointerType(structType, 0);
			}
			else
			{
				llvmElementType = getLLVMType(nebulaElementType);
			}

			LLVMValueRef sizeValue = expression.getSizeExpression().accept(this);

			// --- START OF FIX ---
			// 1. Get the size of a single element using the correct LLVMSizeOf function.
			// It takes only the type and returns an LLVM constant value (an i64).
			LLVMValueRef elementSizeVal = LLVMSizeOf(llvmElementType);

			// 2. Multiply the element size by the number of elements to get the total allocation size.
			// We may need to cast the sizeValue (which is likely i32) to i64 to match the malloc parameter.
			LLVMValueRef sizeValue64 = LLVMBuildSExt(builder, sizeValue, LLVMInt64TypeInContext(context), "size_to_i64");
			LLVMValueRef totalSize = LLVMBuildMul(builder, sizeValue64, elementSizeVal, "total_array_size");
			// --- END OF FIX ---

			// 3. Allocate memory on the heap
			PointerPointer<LLVMValueRef> mallocArgs = new PointerPointer<>(1);
			mallocArgs.put(0, totalSize);

			LLVMTypeRef mallocFuncType = LLVMGlobalGetValueType(mallocFunc);
			LLVMValueRef rawPtr = LLVMBuildCall2(builder, mallocFuncType, mallocFunc, mallocArgs, 1, "raw_array_ptr");

			// 4. Cast the raw pointer to the correct typed pointer (e.g., ptr to i32, ptr to %Person*, etc.)
			LLVMTypeRef arrayPtrType = LLVMPointerType(llvmElementType, 0);
			return LLVMBuildBitCast(builder, rawPtr, arrayPtrType, "typed_array_ptr");
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitArrayInitializerExpression(ArrayInitializerExpression expression)
	{
		Debug.log("Visiting ArrayInitializerExpression");
		Debug.indent();
		try
		{
			ArrayType arrayType = (ArrayType) expression.getResolvedType();
			if (arrayType == null)
			{
				Debug.log("-> ERROR: Array initializer has no resolved type.");
				return null;
			}

			Type nebulaElementType = arrayType.getElementType();
			LLVMTypeRef llvmElementType;
			boolean elementsAreStackAllocated = false;

			if (nebulaElementType instanceof ClassType)
			{
				String fqn = ((ClassType) nebulaElementType).getFqn();
				LLVMTypeRef structType = definedStructs.get(fqn);
				if (semanticAnalyzer.getStackAllocatedClasses().contains(fqn))
				{
					llvmElementType = structType; // Array holds structs: %Person
					elementsAreStackAllocated = true;
				}
				else
				{
					llvmElementType = LLVMPointerType(structType, 0); // Array holds pointers: %OtherClass*
				}
			}
			else
			{
				llvmElementType = getLLVMType(nebulaElementType); // Array of primitives
			}

			int numElements = expression.getElements().size();
			long elementSize = LLVMSizeOfTypeInBits(targetData, llvmElementType) / 8;
			long totalSize = elementSize * numElements;
			LLVMValueRef sizeVal = LLVMConstInt(LLVMInt64TypeInContext(context), totalSize, 0);

			PointerPointer<LLVMValueRef> mallocArgs = new PointerPointer<>(1);
			mallocArgs.put(0, sizeVal);

			LLVMTypeRef mallocFuncType = LLVMGlobalGetValueType(mallocFunc);
			LLVMValueRef rawPtr = LLVMBuildCall2(builder, mallocFuncType, mallocFunc, mallocArgs, 1, "raw_array_ptr");

			LLVMTypeRef arrayPtrType = LLVMPointerType(llvmElementType, 0);
			LLVMValueRef typedArrayPtr = LLVMBuildBitCast(builder, rawPtr, arrayPtrType, "typed_array_ptr");

			for (int i = 0; i < numElements; i++)
			{
				LLVMValueRef index = LLVMConstInt(LLVMInt32TypeInContext(context), i, 0);

				// --- START OF FIX ---
				// Explicitly create the PointerPointer for the GEP indices to avoid ambiguity.
				PointerPointer<LLVMValueRef> indices = new PointerPointer<>(1);
				indices.put(0, index);

				// The second argument to GEP2 must be the element type of the array,
				// which is what the pointer `typedArrayPtr` points to.

				LLVMTypeRef pointeeType = LLVMGetElementType(getLLVMType(nebulaElementType));
				LLVMValueRef elementPtr = LLVMBuildGEP2(builder, pointeeType, typedArrayPtr, indices, 1, "elem_ptr");
				// --- END OF FIX ---

				if (elementsAreStackAllocated)
				{
					// For stack types, we construct the object directly into the array slot.
					this.currentAllocaForStackObject = elementPtr;
					expression.getElements().get(i).accept(this); // Constructor is called inside
					this.currentAllocaForStackObject = null; // Reset flag
				}
				else
				{
					// For heap types, we get the pointer from 'new' and store it in the array.
					LLVMValueRef elementValue = expression.getElements().get(i).accept(this);
					LLVMBuildStore(builder, elementValue, elementPtr);
				}
			}

			return typedArrayPtr;
		}
		finally
		{
			Debug.dedent();
		}
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitSwitchCase(SwitchCase switchCase)
	{
		return null;
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitConstructorChainingCallStatement(ConstructorChainingCallStatement statement)
	{
		return null;
	}

	@Override
	public LLVMValueRef visitVariableDeclarationStatement(VariableDeclarationStatement statement)
	{
		Debug.log("Visiting VariableDeclarationStatement: %s", statement.getName().getLexeme());
		Debug.indent();
		try
		{
			VariableSymbol varSymbol = statement.getResolvedSymbol();
			if (varSymbol == null)
			{
				Debug.log("-> ERROR: No resolved symbol for this variable. Skipping.");
				return null;
			}

			Type nebulaType = varSymbol.getType();
			LLVMTypeRef llvmType;

			// --- FIX START ---
// Check if the type is a class marked for stack allocation.
			if (nebulaType instanceof ClassType && semanticAnalyzer.getStackAllocatedClasses().contains(((ClassType) nebulaType).getFqn()))
			{
				// If so, the variable's type is the struct itself, not a pointer to it.
				llvmType = definedStructs.get(((ClassType) nebulaType).getFqn());
			}
			else
			{
				// Otherwise, use the existing logic (for primitives or heap pointers).
				llvmType = getLLVMType(nebulaType);
			}
// --- FIX END ---

			if (llvmType == null)
			{
				Debug.log("-> ERROR: Could not resolve LLVM type for Nebula type '%s'.", nebulaType.getName());
				return null;
			}

			String varName = varSymbol.getName();
			LLVMValueRef alloca = LLVMBuildAlloca(builder, llvmType, varSymbol.getName());
			Debug.log("-> Allocated stack space (alloca) for '%s' of type %s", varName, LLVMPrintTypeToString(llvmType).getString());
			scopes.peek().variables.put(varSymbol.getName(), alloca);

			Debug.log("-> Stored variable pointer in the current scope.");

			if (statement.getInitializer() != null)
			{
				Debug.log("-> Generating code for initializer.");

				// --- FIX START ---
				// If it's a stack object, we need to tell 'new' where to construct it.
				if (nebulaType instanceof ClassType && semanticAnalyzer.getStackAllocatedClasses().contains(((ClassType) nebulaType).getFqn()))
				{
					// Use a temporary field to pass the stack address to the NewExpression visitor.
					this.currentAllocaForStackObject = alloca;
				}
				// --- FIX END ---

				LLVMValueRef initializerValue = statement.getInitializer().accept(this);

				// --- FIX START ---
				// Reset the temporary field immediately after.
				this.currentAllocaForStackObject = null;

				// For stack objects, 'new' already initialized the memory. For all other types, we must store the result.
				if (!(nebulaType instanceof ClassType && semanticAnalyzer.getStackAllocatedClasses().contains(((ClassType) nebulaType).getFqn())))
				{
					if (initializerValue != null)
					{
						LLVMBuildStore(builder, initializerValue, alloca);
					}
					else
					{
						Debug.log("-> WARNING: Initializer expression yielded a null value.");
					}
				}
				// --- FIX END ---
			}

			return alloca;
		}
		finally
		{
			Debug.dedent();
		}
	}

	@Override
	public LLVMValueRef visitExpressionStatement(ExpressionStatement statement)
	{
		Debug.log("Visiting ExpressionStatement");
		Debug.indent();
		LLVMValueRef result = statement.getExpression().accept(this);
		Debug.dedent();
		return result;
	}

	@Override
	public LLVMValueRef visitReturnStatement(ReturnStatement statement)
	{
		Debug.log("Visiting ReturnStatement");
		Debug.indent();
		try
		{
			if (statement.getValue() != null)
			{
				LLVMValueRef returnValue = statement.getValue().accept(this);
				if (returnValue != null)
				{
					Debug.log("-> Generating 'ret' with a value.");
					return LLVMBuildRet(builder, returnValue);
				}
				else
				{
					Debug.log("-> ERROR: Return expression generated a null value.");
					return null;
				}
			}
			else
			{
				Debug.log("-> Generating 'ret void'.");
				return LLVMBuildRetVoid(builder);
			}
		}
		finally
		{
			Debug.dedent();
		}
	}

	//TODO: Implement
	@Override
	public LLVMValueRef visitImportDirective(ImportDirective directive)
	{
		return null;
	}


	private class DeclarationVisitor implements ASTVisitor<Void>
	{

		public void declareAll(Map<String, ClassSymbol> allKnownClasses)
		{
			Debug.log("DeclarationVisitor: Beginning declaration passes...");
			Debug.indent();

			Debug.log("Pass 1.1: Creating opaque struct types...");
			Debug.indent();
			for (ClassSymbol classSymbol : allKnownClasses.values())
			{
				if (classSymbol != null && !definedStructs.containsKey(classSymbol.getFqn()))
				{
					Debug.log("-> Creating opaque struct: %%%s", classSymbol.getFqn());
					LLVMTypeRef structType = LLVMStructCreateNamed(context, classSymbol.getFqn());
					definedStructs.put(classSymbol.getFqn(), structType);
				}
			}
			Debug.dedent();
			Debug.log("Pass 1.1 COMPLETE.");

			Debug.log("Pass 1.2: Defining struct bodies...");
			Debug.indent();
			Debug.log("Pass 1.2: Defining struct bodies...");
			for (ClassSymbol classSymbol : allKnownClasses.values())
			{
				if (classSymbol == null)
				{
					continue;
				}

				LLVMTypeRef structType = definedStructs.get(classSymbol.getFqn());

				if (classSymbol.getFqn().equals("nebula.core.String"))
				{
					Debug.log("-> Defining special struct body for nebula.core.String.");
					LLVMTypeRef i8PtrType = LLVMPointerType(LLVMInt8TypeInContext(context), 0);

					// --- START OF FIX ---
					// Use the reliable two-step process to create the PointerPointer.
					// 1. Allocate a PointerPointer of size 1.
					PointerPointer<LLVMTypeRef> fieldTypes = new PointerPointer<>(1);
					// 2. Put the desired LLVM type into the first slot.
					fieldTypes.put(0, i8PtrType);
					// --- END OF FIX ---

					LLVMStructSetBody(structType, fieldTypes, 1, 0);
					continue;
				}

				List<VariableSymbol> fields = classSymbol.getClassScope().getSymbols().values().stream()
						.filter(s -> s instanceof VariableSymbol && !s.isStatic())
						.map(s -> (VariableSymbol) s)
						.toList();

				PointerPointer<LLVMTypeRef> llvmFieldTypes = new PointerPointer<>(fields.size());
				for (int i = 0; i < fields.size(); i++)
				{
					LLVMTypeRef fieldType = getLLVMType(fields.get(i).getType());
					if (fieldType != null)
					{
						llvmFieldTypes.put(i, fieldType);
					}
					else
					{
						llvmFieldTypes.put(i, LLVMInt8TypeInContext(context)); // Fallback
					}
				}
				LLVMStructSetBody(structType, llvmFieldTypes, fields.size(), 0);
			}
			Debug.log("Pass 1.2 COMPLETE.");
			Debug.dedent();
			Debug.log("Pass 1.2 COMPLETE.");

			Debug.log("Pass 1.3: Declaring function signatures...");
			Debug.indent();

			// Manually declare ALL runtime helper functions first.
			declareRuntimeFunctions();

			// Now declare the functions from the user's Nebula code
			for (ClassSymbol classSymbol : allKnownClasses.values())
			{
				for (List<MethodSymbol> overloads : classSymbol.methodsByName.values())
				{
					for (MethodSymbol methodSymbol : overloads)
					{
						declareFunctionSignature(methodSymbol);
					}
				}
			}
			Debug.dedent();
			Debug.log("Pass 1.3 COMPLETE.");

			Debug.dedent();
			Debug.log("DeclarationVisitor: All declaration passes finished.");
		}

		private void declareRuntimeFunctions()
		{
			Debug.log("Declaring C++ runtime helper functions...");
			LLVMTypeRef stringPtrType = LLVMPointerType(definedStructs.get("nebula.core.String"), 0);

			// Define the correct parameter types
			LLVMTypeRef i32 = LLVMInt32TypeInContext(context);
			LLVMTypeRef i1 = LLVMInt1TypeInContext(context);
			LLVMTypeRef f64 = LLVMDoubleTypeInContext(context);
			LLVMTypeRef i64 = LLVMInt64TypeInContext(context);
			LLVMTypeRef objPtr = LLVMPointerType(LLVMInt8TypeInContext(context), 0);

			// --- START OF DEFINITIVE FIX ---
			// Create the PointerPointer first, then call put() on the next line.

			PointerPointer<LLVMTypeRef> i32Param = new PointerPointer<>(1);
			i32Param.put(0, i32);
			LLVMAddFunction(module, "int32_toString", LLVMFunctionType(stringPtrType, i32Param, 1, 0));
			LLVMAddFunction(module, "char_toString", LLVMFunctionType(stringPtrType, i32Param, 1, 0)); // Re-use for char

			PointerPointer<LLVMTypeRef> i64Param = new PointerPointer<>(1);
			i64Param.put(0, LLVMInt64TypeInContext(context));
			LLVMAddFunction(module, "int64_toString", LLVMFunctionType(stringPtrType, i64Param, 1, 0));

			PointerPointer<LLVMTypeRef> f64Param = new PointerPointer<>(1);
			f64Param.put(0, f64);
			LLVMAddFunction(module, "double_toString", LLVMFunctionType(stringPtrType, f64Param, 1, 0));

			PointerPointer<LLVMTypeRef> i1Param = new PointerPointer<>(1);
			i1Param.put(0, i1);
			LLVMAddFunction(module, "bool_toString", LLVMFunctionType(stringPtrType, i1Param, 1, 0));

			PointerPointer<LLVMTypeRef> objPtrParam = new PointerPointer<>(1);
			objPtrParam.put(0, objPtr);
			LLVMAddFunction(module, "object_toString", LLVMFunctionType(stringPtrType, objPtrParam, 1, 0));

			PointerPointer<LLVMTypeRef> concatParams = new PointerPointer<>(2);
			concatParams.put(0, stringPtrType);
			concatParams.put(1, stringPtrType);
			LLVMAddFunction(module, "string_concat", LLVMFunctionType(stringPtrType, concatParams, 2, 0));

			// --- ADD THIS DECLARATION ---
			// Declare i32 @string_length(ptr)
			PointerPointer<LLVMTypeRef> stringLengthParams = new PointerPointer<>(1);
			stringLengthParams.put(0, stringPtrType);
			LLVMAddFunction(module, "string_length", LLVMFunctionType(i32, stringLengthParams, 1, 0));
			// --- END OF ADDITION ---

			Debug.log("-> Runtime functions declared.");
		}

		private void declareFunctionSignature(MethodSymbol methodSymbol)
		{
			String mangledName = methodSymbol.getMangledName();
			Debug.log("Declaring function signature for: @%s", mangledName);
			Debug.indent();

			if (LLVMGetNamedFunction(module, mangledName) != null)
			{
				Debug.log("-> Function already declared. Skipping.");
				Debug.dedent();
				return;
			}

			LLVMTypeRef returnType = getLLVMType(methodSymbol.getType());
			if (returnType == null)
			{
				System.err.println("FATAL: Could not resolve return type for function: " + mangledName);
				Debug.log("-> FATAL: Unresolved return type. Aborting declaration.");
				Debug.dedent();
				return;
			}
			Debug.log("-> Return Type: %s", LLVMPrintTypeToString(returnType).getString());

			List<Type> nebulaParamTypes = methodSymbol.getParameterTypes();
			int totalParams = nebulaParamTypes.size() + (methodSymbol.isStatic() ? 0 : 1);
			PointerPointer<LLVMTypeRef> llvmParamTypes = new PointerPointer<>(totalParams);
			int llvmParamIndex = 0;

// --- START OF FIX ---
// The 'this' parameter must ALWAYS be a pointer, regardless of stack/heap allocation.
			if (!methodSymbol.isStatic())
			{
				String fqn = methodSymbol.getOwnerClass().getFqn();
				Debug.log("-> Adding 'this' parameter (type: %%%s*)", fqn);
				LLVMTypeRef structType = definedStructs.get(fqn);
				if (structType == null)
				{
					System.err.println("FATAL: Could not find struct for 'this' parameter: " + fqn);
					Debug.log("-> FATAL: Could not resolve 'this' parameter struct. Aborting declaration.");
					Debug.dedent();
					return;
				}
				// This is the key change: always create a pointer type for 'this'.
				LLVMTypeRef thisPtrType = LLVMPointerType(structType, 0);
				llvmParamTypes.put(llvmParamIndex++, thisPtrType);
			}
// --- END OF FIX ---

			Debug.log("-> Processing %d regular parameter(s)...", nebulaParamTypes.size());
			Debug.indent();
			for (Type paramNebulaType : nebulaParamTypes)
			{
				Debug.log("-> Param %d: Nebula type is '%s'", llvmParamIndex, paramNebulaType.getName());
				if (paramNebulaType.equals(PrimitiveType.VOID))
				{
					System.err.println("FATAL: Method '" + mangledName + "' cannot have a parameter of type 'void'.");
					Debug.log("-> FATAL: 'void' parameter found. Aborting.");
					Debug.dedent();
					Debug.dedent();
					return;
				}
				LLVMTypeRef paramLLVMType = getLLVMType(paramNebulaType); // getLLVMType is correct for non-'this' params
				if (paramLLVMType == null)
				{
					System.err.println("FATAL: Could not resolve LLVM type for parameter of function: " + mangledName);
					Debug.log("-> FATAL: Unresolved LLVM type for parameter. Aborting.");
					Debug.dedent();
					Debug.dedent();
					return;
				}
				llvmParamTypes.put(llvmParamIndex++, paramLLVMType);
			}
			Debug.dedent();

			LLVMTypeRef functionType = LLVMFunctionType(returnType, llvmParamTypes, totalParams, 0);
			if (functionType == null)
			{
				System.err.println("FATAL: Failed to create function type for: " + mangledName);
				Debug.log("-> FATAL: LLVMFunctionType returned null.");
				Debug.dedent();
				return;
			}
			LLVMAddFunction(module, mangledName, functionType);
			Debug.log("-> Successfully added function declaration to module.");
			Debug.dedent();
		}

		// Stub implementations for the rest of the visitor methods
		@Override
		public Void visitProgram(Program program)
		{
			return null;
		}

		@Override
		public Void visitNamespaceDeclaration(NamespaceDeclaration declaration)
		{
			return null;
		}

		@Override
		public Void visitClassDeclaration(ClassDeclaration declaration)
		{
			return null;
		}

		@Override
		public Void visitImportDirective(ImportDirective directive)
		{
			return null;
		}

		@Override
		public Void visitMethodDeclaration(MethodDeclaration declaration)
		{
			Debug.log("Visiting method declaration: " + declaration.toString());
			return null;
		}

		@Override
		public Void visitConstructorDeclaration(ConstructorDeclaration declaration)
		{
			return null;
		}

		@Override
		public Void visitFieldDeclaration(FieldDeclaration declaration)
		{
			return null;
		}

		@Override
		public Void visitPropertyDeclaration(PropertyDeclaration declaration)
		{
			return null;
		}

		@Override
		public Void visitBlockStatement(BlockStatement statement)
		{
			return null;
		}

		@Override
		public Void visitExpressionStatement(ExpressionStatement statement)
		{
			return null;
		}

		@Override
		public Void visitIfStatement(IfStatement statement)
		{
			return null;
		}

		@Override
		public Void visitWhileStatement(WhileStatement statement)
		{
			return null;
		}

		@Override
		public Void visitForStatement(ForStatement statement)
		{
			return null;
		}

		@Override
		public Void visitForEachStatement(ForEachStatement statement)
		{
			return null;
		}

		@Override
		public Void visitReturnStatement(ReturnStatement statement)
		{
			return null;
		}

		@Override
		public Void visitVariableDeclarationStatement(VariableDeclarationStatement statement)
		{
			return null;
		}

		@Override
		public Void visitSwitchStatement(SwitchStatement statement)
		{
			return null;
		}

		@Override
		public Void visitArrayCreationExpression(ArrayCreationExpression expression)
		{
			return null;
		}

		@Override
		public Void visitArrayInitializerExpression(ArrayInitializerExpression expression)
		{
			return null;
		}

		@Override
		public Void visitSwitchCase(SwitchCase switchCase)
		{
			return null;
		}

		@Override
		public Void visitConstructorChainingCallStatement(ConstructorChainingCallStatement statement)
		{
			return null;
		}

		@Override
		public Void visitBinaryExpression(BinaryExpression expression)
		{
			return null;
		}

		@Override
		public Void visitUnaryExpression(UnaryExpression expression)
		{
			return null;
		}

		@Override
		public Void visitLiteralExpression(LiteralExpression expression)
		{
			return null;
		}

		@Override
		public Void visitIdentifierExpression(IdentifierExpression expression)
		{
			return null;
		}

		@Override
		public Void visitAssignmentExpression(AssignmentExpression expression)
		{
			return null;
		}

		@Override
		public Void visitCallExpression(CallExpression expression)
		{
			return null;
		}

		@Override
		public Void visitDotExpression(DotExpression expression)
		{
			return null;
		}

		@Override
		public Void visitThisExpression(ThisExpression expression)
		{
			return null;
		}

		@Override
		public Void visitNewExpression(NewExpression expression)
		{
			return null;
		}

		@Override
		public Void visitPostfixUnaryExpression(PostfixUnaryExpression expression)
		{
			return null;
		}

		@Override
		public Void visitArrayAccessExpression(ArrayAccessExpression expression)
		{
			return null;
		}

		@Override
		public Void visitGroupingExpression(GroupingExpression expression)
		{
			return null;
		}

		@Override
		public Void visitCastExpression(CastExpression expression)
		{
			return null;
		}

		@Override
		public Void visitTernaryExpression(TernaryExpression expression)
		{
			return null;
		}

		@Override
		public Void visitIsExpression(IsExpression expression)
		{
			return null;
		}
	}

	// Add this helper method to LLVMIRGenerator.java
	private void dumpModuleState(String stage)
	{
		System.out.println("\n\n--- LLVM IR DUMP (" + stage + ") ---\n");
		BytePointer irString = LLVMPrintModuleToString(module);
		System.out.println(irString.getString());
		LLVMDisposeMessage(irString);
		System.out.println("\n--- END DUMP ---\n\n");
	}

	/**
	 * Helper method to generate a call to the appropriate toString() method for a given type.
	 * Add this helper method to your LLVMIRGenerator class.
	 */
	private LLVMValueRef callToString(LLVMValueRef value, Type type)
	{
		Debug.log("-> Generating call to toString() for type %s", type.getName());

		// Promote smaller integers to i32 before calling the C++ helper
		if (type.isInteger() && LLVMSizeOfTypeInBits(targetData, getLLVMType(type)) < 32)
		{
			value = LLVMBuildSExt(builder, value, LLVMInt32TypeInContext(context), "promoted_to_i32");
			type = PrimitiveType.INT32; // Treat it as an int32 from now on
		}

		// --- ADD THIS BLOCK ---
		// Promote float to double to use the existing double_toString helper.
		if (type.equals(PrimitiveType.FLOAT))
		{
			value = LLVMBuildFPExt(builder, value, LLVMDoubleTypeInContext(context), "promoted_to_double");
			type = PrimitiveType.DOUBLE; // Treat it as a double from now on
		}
		// --- END OF FIX ---

		String funcName = type.getName().replace('.', '_') + "_toString";
		LLVMValueRef toStringFunc = LLVMGetNamedFunction(module, funcName);

		if (toStringFunc == null || toStringFunc.isNull())
		{
			Debug.log("-> WARNING: Could not find specific toString '%s'. Falling back to generic object_toString.", funcName);
			toStringFunc = LLVMGetNamedFunction(module, "object_toString");

			// --- FIX START: Correctly handle pointers for fallback ---
			LLVMTypeRef valueType = getLLVMType(type);
			if (LLVMGetTypeKind(valueType) != LLVMPointerTypeKind)
			{
				// It's a stack-allocated struct value. We need its address.
				LLVMValueRef tempAlloca = LLVMBuildAlloca(builder, valueType, "temp_obj_for_tostring");
				LLVMBuildStore(builder, value, tempAlloca);
				value = tempAlloca; // 'value' is now the pointer to the struct
			}
			// Cast the object pointer to a generic void* (represented as i8* in LLVM)
			value = LLVMBuildBitCast(builder, value, LLVMPointerType(LLVMInt8TypeInContext(context), 0), "obj_as_void_ptr");
			// --- FIX END ---
		}

		LLVMTypeRef funcType = LLVMGlobalGetValueType(toStringFunc);
		PointerPointer<LLVMValueRef> args = new PointerPointer<>(1);
		args.put(0, value);

		return LLVMBuildCall2(builder, funcType, toStringFunc, args, 1, "toString_val");
	}

	/**
	 * Generates the LLVM IR for a property's getter method body.
	 * A getter loads the value from the property's backing field and returns it.
	 */
	private void generateGetterBody(PropertySymbol propertySymbol)
	{
		MethodSymbol getter = propertySymbol.getGetter();
		if (getter == null)
		{
			return;
		}

		Debug.log("Generating getter body for: %s.%s", propertySymbol.getOwnerClass().getName(), propertySymbol.getName());
		Debug.indent();

		// 1. Find the already-declared function signature.
		LLVMValueRef function = LLVMGetNamedFunction(module, getter.getMangledName());
		this.currentFunction = function;

		// 2. Create the entry block and position the builder.
		LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, function, "entry");
		LLVMPositionBuilderAtEnd(builder, entry);

		// 3. Get the 'this' pointer (the instance of the object).
		LLVMValueRef thisPtr = LLVMGetParam(function, 0);

		// 4. Find the backing field for the property.
		ClassSymbol ownerClass = propertySymbol.getOwnerClass();
		int fieldIndex = ownerClass.getBackingFieldIndexForProperty(propertySymbol.getName());
		LLVMTypeRef structType = definedStructs.get(ownerClass.getFqn());

		// 5. Get a pointer to the backing field inside the struct.
		LLVMValueRef fieldPtr = LLVMBuildStructGEP2(builder, structType, thisPtr, fieldIndex, propertySymbol.getName() + "_fieldptr");

		// 6. Load the value from the field's pointer.
		LLVMTypeRef fieldLLVMType = getLLVMType(propertySymbol.getType());
		LLVMValueRef fieldValue = LLVMBuildLoad2(builder, fieldLLVMType, fieldPtr, propertySymbol.getName() + "_value");

		// 7. Return the loaded value.
		LLVMBuildRet(builder, fieldValue);

		Debug.dedent();
	}

	/**
	 * Generates the LLVM IR for a property's setter method body.
	 * A setter stores the incoming value into the property's backing field.
	 */
	private void generateSetterBody(PropertySymbol propertySymbol)
	{
		MethodSymbol setter = propertySymbol.getSetter();
		if (setter == null)
		{
			return;
		}

		Debug.log("Generating setter body for: %s.%s", propertySymbol.getOwnerClass().getName(), propertySymbol.getName());
		Debug.indent();

		// 1. Find the already-declared function signature.
		LLVMValueRef function = LLVMGetNamedFunction(module, setter.getMangledName());
		this.currentFunction = function;

		// 2. Create the entry block and position the builder.
		LLVMBasicBlockRef entry = LLVMAppendBasicBlockInContext(context, function, "entry");
		LLVMPositionBuilderAtEnd(builder, entry);

		// 3. Get parameters: 'this' (the object instance) and 'value' (the value to set).
		LLVMValueRef thisPtr = LLVMGetParam(function, 0);
		LLVMValueRef valueToSet = LLVMGetParam(function, 1);

		// 4. Find the backing field for the property.
		ClassSymbol ownerClass = propertySymbol.getOwnerClass();
		int fieldIndex = ownerClass.getBackingFieldIndexForProperty(propertySymbol.getName());
		LLVMTypeRef structType = definedStructs.get(ownerClass.getFqn());

		// 5. Get a pointer to the backing field inside the struct.
		LLVMValueRef fieldPtr = LLVMBuildStructGEP2(builder, structType, thisPtr, fieldIndex, propertySymbol.getName() + "_fieldptr");

		// 6. Store the new value into the field's pointer.
		LLVMBuildStore(builder, valueToSet, fieldPtr);

		// 7. Return void.
		LLVMBuildRetVoid(builder);

		Debug.dedent();
	}
}
