// File: src/main/java/com/juanpa/nebula/transpiler/Main.java

package com.juanpa.nebula.compiler;

import com.juanpa.nebula.compiler.ast.Program;
import com.juanpa.nebula.compiler.codegen.CppGenerator;
import com.juanpa.nebula.compiler.codegen.LLVMIRGenerator;
import com.juanpa.nebula.compiler.lexer.Lexer;
import com.juanpa.nebula.compiler.lexer.Token;
import com.juanpa.nebula.compiler.parser.NebulaParser;
import com.juanpa.nebula.compiler.semantics.ClassSymbol;
import com.juanpa.nebula.compiler.semantics.SemanticAnalyzer;
import com.juanpa.nebula.compiler.serialization.SymbolSerializer;
import com.juanpa.nebula.compiler.util.CompilerConfig;
import com.juanpa.nebula.compiler.util.Debug;
import com.juanpa.nebula.compiler.util.ErrorReporter;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Entry point for the Nebula Transpiler.
 * This Main class orchestrates the entire compilation process.
 */
public class Main
{

	// File: src/main/java/com/juanpa/nebula/transpiler/Main.java

	public static void main(String[] args)
	{
		// 1. Load Compiler Configuration
		CompilerConfig config = loadConfiguration();

		// 2. Argument parsing for flags
		boolean useLLVM = false;
		List<String> mainArgs = new ArrayList<>();
		for (String arg : args)
		{
			if (arg.equals("--use-llvm"))
			{
				useLLVM = true;
			}
			else
			{
				mainArgs.add(arg);
			}
		}

		// 3. Handle distinct compiler modes
		if (mainArgs.isEmpty())
		{
			System.err.println("Usage: nbcc [--use-llvm] <file.neb | project.nebproj>");
			System.err.println("   or: nbcc --compile-ndk --ndk-source <ndk_src_dir> --output-symbols <file.nsf> --output-cpp <dir>");
			return;
		}

		String command = mainArgs.get(0);
		if (command.equals("--compile-ndk"))
		{
			// NDK compilation mode
			if (mainArgs.size() < 7)
			{
				System.err.println("Usage: nbcc --compile-ndk --ndk-source <ndk_src_dir> --output-symbols <file.nsf> --output-cpp <dir>");
				return;
			}
			Path ndkSourceDir = Paths.get(mainArgs.get(2));
			Path nsfFile = Paths.get(mainArgs.get(4));
			Path cppDir = Paths.get(mainArgs.get(6));
			compileNdk(ndkSourceDir, nsfFile, cppDir, config);
			return;
		}

		// --- Standard Compilation Flow (Project or Single-File) ---
		Path inputPath = Paths.get(command);
		if (!Files.exists(inputPath))
		{
			System.err.println("Error: Input file or project not found: " + inputPath);
			return;
		}

		ErrorReporter errorReporter = new ErrorReporter();
		SemanticAnalyzer semanticAnalyzer = new SemanticAnalyzer(errorReporter, config);
		Program fullProgramAST = null;
		String mainClassFqn = null;
		Path projectRoot; // The base directory for placing the 'out' folder

		// --- MODE SELECTION ---
		try
		{
			if (command.endsWith(".nebproj"))
			{
				// --- PROJECT MODE ---
				System.out.println("--- Compiling in Project Mode ---");
				Path projectFile = inputPath;
				projectRoot = projectFile.getParent() != null ? projectFile.getParent() : Paths.get(".");

				Properties projectProps = new Properties();
				projectProps.load(new FileInputStream(projectFile.toFile()));

				mainClassFqn = projectProps.getProperty("main_class");
				if (mainClassFqn == null || mainClassFqn.isBlank())
				{
					errorReporter.report(0, 0, "Project file must specify a 'main_class'.");
					return;
				}

				String sourceRootsStr = projectProps.getProperty("source_roots", ".");
				List<Path> sourceRoots = Arrays.stream(sourceRootsStr.split(","))
						.map(String::trim)
						.map(projectRoot::resolve)
						.collect(Collectors.toList());

				ProjectLoader loader = new ProjectLoader(sourceRoots, errorReporter, loadNdkSymbols(semanticAnalyzer));
				fullProgramAST = loader.loadProjectFromSourceRoots();

			}
			else if (command.endsWith(".neb"))
			{
				// --- SINGLE-FILE MODE ---
				System.out.println("--- Compiling in Single-File Mode ---");
				Path singleFile = inputPath;
				projectRoot = singleFile.getParent() != null ? singleFile.getParent() : Paths.get(".");

				// In single-file mode, we don't need the project loader's discovery.
				// We just parse the one file provided.
				String sourceCode = Files.readString(singleFile, StandardCharsets.UTF_8);
				Lexer lexer = new Lexer(sourceCode, errorReporter);
				List<Token> tokens = lexer.scanTokens();
				NebulaParser parser = new NebulaParser(tokens, errorReporter);
				fullProgramAST = parser.parse();

			}
			else
			{
				System.err.println("Error: Invalid input. Must be a .neb file or a .nebproj project file.");
				return;
			}

			if (fullProgramAST == null || errorReporter.hasErrors())
			{
				System.err.println("Build failed during parsing phase.");
				return;
			}

		}
		catch (IOException e)
		{
			System.err.println("An I/O error occurred during project loading: " + e.getMessage());
			return;
		}

		// --- ANALYSIS & CODEGEN (COMMON TO BOTH MODES) ---
		semanticAnalyzer.preloadSymbols(loadNdkSymbols(semanticAnalyzer)); // Load NDK symbols before analysis
		Debug.log("\n--- Semantic Analysis Phase ---");
		semanticAnalyzer.analyze(fullProgramAST);

		if (errorReporter.hasErrors())
		{
			Debug.log("Build failed due to semantic errors.");
			return;
		}
		Debug.log("Semantic analysis completed successfully.");

		Debug.log("\n--- Code Generation Phase ---");
		Path outputDirPath = projectRoot.resolve("out");
		try
		{
			Files.createDirectories(outputDirPath);
		}
		catch (IOException e)
		{
			System.err.println("Error creating output directory: " + e.getMessage());
			return;
		}

		if (useLLVM)
		{
			LLVMIRGenerator llvmGenerator = new LLVMIRGenerator(semanticAnalyzer);
			String outputName = inputPath.getFileName().toString().replaceAll("\\.neb(proj)?$", ".ll");
			Path llvmIrFile = outputDirPath.resolve(outputName);

			// Pass the main class FQN if available (from project mode), otherwise null
			llvmGenerator.generate(fullProgramAST, llvmIrFile.toString(), mainClassFqn);

			if (!errorReporter.hasErrors())
			{
				System.out.println("\n--- Compiling and Running Generated LLVM IR ---");
				compileAndRunLLVM(outputDirPath, llvmIrFile, config);
			}
		}
		else
		{
			System.err.println("C++ backend is deprecated. Please use the --use-llvm flag.");
		}

		System.out.println("\nTranspilation process finished.");
	}

	// --- NEW HELPER METHOD TO AVOID CODE DUPLICATION ---
	private static Map<String, ClassSymbol> loadNdkSymbols(SemanticAnalyzer semanticAnalyzer)
	{
		Path symbolCacheFile = Paths.get("ndk.nsf");
		if (Files.exists(symbolCacheFile))
		{
			Debug.log("--- Found NDK symbol cache (ndk.nsf). Loading... ---");
			try (InputStream is = new BufferedInputStream(new FileInputStream(symbolCacheFile.toFile())))
			{
				SymbolSerializer serializer = new SymbolSerializer();
				Map<String, ClassSymbol> preloadedSymbols = serializer.readSymbols(is);
				Debug.log("--- NDK symbols loaded successfully. ---");
				return preloadedSymbols;
			}
			catch (IOException e)
			{
				System.err.println("Warning: Could not read symbol cache file 'ndk.nsf'. Error: " + e.getMessage());
			}
		}
		return new HashMap<>();
	}

	private static CompilerConfig loadConfiguration()
	{
		Properties props = new Properties();
		Path configPath = Paths.get(System.getProperty("user.home"), ".config", "nebula", "nebula.conf");

		if (Files.exists(configPath))
		{
			try (InputStream input = new FileInputStream(configPath.toFile()))
			{
				props.load(input);
				System.out.println("--- Loaded configuration from: " + configPath + " ---");
			}
			catch (IOException e)
			{
				System.err.println("Warning: Could not read config file at " + configPath + ". Using default settings.");
			}
		}
		else
		{
			System.out.println("--- No config file found at ~/.config/nebula/nebula.conf. Using default settings. ---");
			System.out.println("--- You can create this file to customize compiler paths. Example: ---");
			System.out.println("# ~/.config/nebula/nebula.conf");
			System.out.println("compiler.cpp_path = /usr/bin/g++");
			System.out.println("ndk.header_path = /path/to/your/nebula/ndk_cpp");
			System.out.println("ndk.library_path = /path/to/your/nebula/libs/libndk.so");
			System.out.println("--------------------------------------------------------------------");
		}
		return new CompilerConfig(props);
	}

	private static void compileNdk(Path ndkSourceDir, Path nsfOutputFile, Path cppOutputDir, CompilerConfig config)
	{
		System.out.println("--- Compiling NDK symbols to: " + nsfOutputFile.toAbsolutePath() + " ---");
		System.out.println("--- Generating NDK C++ code to: " + cppOutputDir.toAbsolutePath() + " ---");
		ErrorReporter errorReporter = new ErrorReporter();

		// Use the provided ndkSourceDir to load files
		List<Path> sourceRoots = List.of(ndkSourceDir);
		ProjectLoader loader = new ProjectLoader(sourceRoots, errorReporter, null);
		Program ndkAst;
		try
		{
			ndkAst = loader.loadProjectFromSourceRoots(); // Use the new unified method
			if (ndkAst == null || errorReporter.hasErrors())
			{
				System.err.println("Failed to parse NDK source files.");
				return;
			}
		}
		catch (IOException e)
		{
			System.err.println("An I/O error occurred: " + e.getMessage());
			return;
		}

		SemanticAnalyzer semanticAnalyzer = new SemanticAnalyzer(errorReporter, config);
		semanticAnalyzer.analyze(ndkAst);
		if (errorReporter.hasErrors())
		{
			System.err.println("Semantic errors found in NDK.");
			return;
		}

		try (FileOutputStream fos = new FileOutputStream(nsfOutputFile.toFile()))
		{
			new SymbolSerializer().writeSymbols(semanticAnalyzer.getDeclaredClasses(), fos);
			System.out.println("--- NDK symbols successfully saved. ---");
		}
		catch (IOException e)
		{
			System.err.println("Error writing symbol cache file: " + e.getMessage());
		}

		System.out.println("\n--- Generating NDK C++ Code ---");
		Map<String, String> generatedCode = new CppGenerator(semanticAnalyzer.getDeclaredClasses(), semanticAnalyzer).generate(ndkAst);
		try
		{
			Files.createDirectories(cppOutputDir);
			for (Map.Entry<String, String> entry : generatedCode.entrySet())
			{
				saveToFile(cppOutputDir.resolve(entry.getKey()), entry.getValue());
			}
			System.out.println("--- NDK C++ code generated successfully. ---");
		}
		catch (IOException e)
		{
			System.err.println("Error saving generated NDK C++ files: " + e.getMessage());
			return;
		}

		System.out.println("\n--- Building NDK Shared Library ---");
		try
		{
			List<String> objectFiles = new ArrayList<>();
			for (String fileName : generatedCode.keySet())
			{
				if (fileName.endsWith(".cpp"))
				{
					Path sourceFile = cppOutputDir.resolve(fileName);
					String objectFile = fileName.replace(".cpp", ".o");
					objectFiles.add(objectFile);

					List<String> cmd = List.of(
							config.getCppCompilerPath(),
							"-c", "-fPIC",
							"-I" + cppOutputDir.toAbsolutePath(),
							sourceFile.toAbsolutePath().toString(),
							"-o", cppOutputDir.resolve(objectFile).toAbsolutePath().toString()
					);

					Process p = new ProcessBuilder(cmd).inheritIO().start();
					if (p.waitFor() != 0)
					{
						System.err.println("Failed to compile " + fileName);
						return;
					}
				}
			}

			String libraryName = "libndk.so";
			Path libraryPath = cppOutputDir.resolve(libraryName);

			List<String> linkCmd = new ArrayList<>();
			linkCmd.add(config.getCppCompilerPath());
			linkCmd.add("-shared");
			linkCmd.add("-o");
			linkCmd.add(libraryPath.toAbsolutePath().toString());
			objectFiles.stream()
					.map(obj -> cppOutputDir.resolve(obj).toAbsolutePath().toString())
					.forEach(linkCmd::add);

			Process p = new ProcessBuilder(linkCmd).inheritIO().start();
			if (p.waitFor() == 0)
			{
				System.out.println("--- NDK Shared Library '" + libraryName + "' built successfully. ---");
			}
			else
			{
				System.err.println("Failed to link NDK shared library.");
			}

		}
		catch (IOException | InterruptedException e)
		{
			System.err.println("An error occurred while building the NDK library: " + e.getMessage());
			Thread.currentThread().interrupt();
		}
	}


	// --- HELPER METHODS ---

	private static void saveToFile(Path filePath, String content)
	{
		try
		{
			Files.createDirectories(filePath.getParent());
			Files.write(filePath, content.getBytes(StandardCharsets.UTF_8));
			System.out.println("Saved: " + filePath.getFileName());
		}
		catch (IOException e)
		{
			System.err.println("Error saving generated code to file '" + filePath + "': " + e.getMessage());
		}
	}

	private static void compileAndRunCpp(Path outputDirPath, Map<String, String> generatedCode, CompilerConfig config)
	{
		// --- 1. Parse Library Path Information ---
		Path fullLibraryPath = Paths.get(config.getNdkLibraryPath());
		Path libraryDir = fullLibraryPath.getParent();
		String libraryName = fullLibraryPath.getFileName().toString();
		if (libraryName.startsWith("lib"))
		{
			libraryName = libraryName.substring(3);
		}
		if (libraryName.endsWith(".so") || libraryName.endsWith(".dylib"))
		{
			libraryName = libraryName.substring(0, libraryName.lastIndexOf('.'));
		}

		// --- 2. Build the Compile Command ---
		List<String> compileCommand = new ArrayList<>();
		compileCommand.add(config.getCppCompilerPath());

		// Add the optimization flag here
		compileCommand.add("-O3");

		// Include paths for user code and NDK headers
		compileCommand.add("-I" + outputDirPath.toAbsolutePath());
		compileCommand.add("-I" + config.getNdkHeaderPath());

		// Add user project .cpp files
		generatedCode.keySet().stream()
				.filter(fileName -> fileName.endsWith(".cpp"))
				.map(fileName -> outputDirPath.resolve(fileName).toAbsolutePath().toString())
				.forEach(compileCommand::add);

		// Link against the NDK library
		compileCommand.add("-L" + libraryDir.toAbsolutePath());
		compileCommand.add("-l" + libraryName);

		// Embed the runtime path to the library directory in the executable
		compileCommand.add("-Wl,-rpath," + libraryDir.toAbsolutePath());

		// Output executable
		compileCommand.add("-o");
		compileCommand.add(outputDirPath.resolve("main").toAbsolutePath().toString());

		System.out.println("Compiling with command: " + String.join(" ", compileCommand));

		// --- 3. Execute the Command ---
		try
		{
			ProcessBuilder compileProcessBuilder = new ProcessBuilder(compileCommand);
			compileProcessBuilder.directory(outputDirPath.toFile());
			Process compileProcess = compileProcessBuilder.start();

			new Thread(new StreamGobbler(compileProcess.getInputStream(), "[C++ stdout]: ")).start();
			new Thread(new StreamGobbler(compileProcess.getErrorStream(), "[C++ stderr]: ")).start();

			int compileExitCode = compileProcess.waitFor();

			if (compileExitCode == 0)
			{
				System.out.println("C++ compilation successful.");
				System.out.println("Running executable...");
				ProcessBuilder runProcessBuilder = new ProcessBuilder(outputDirPath.resolve("main").toAbsolutePath().toString());
				runProcessBuilder.directory(outputDirPath.toFile());
				Process runProcess = runProcessBuilder.start();

				new Thread(new StreamGobbler(runProcess.getInputStream(), "[App stdout]: ")).start();
				new Thread(new StreamGobbler(runProcess.getErrorStream(), "[App stderr]: ")).start();

				runProcess.waitFor();
			}
			else
			{
				System.err.println("C++ compilation failed with exit code: " + compileExitCode);
			}
		}
		catch (IOException | InterruptedException e)
		{
			System.err.println("Error during C++ compilation or execution: " + e.getMessage());
			Thread.currentThread().interrupt();
		}
	}

	/**
	 * Compiles the generated LLVM IR file into an executable using Link-Time Optimization (LTO) and runs it.
	 *
	 * @param outputDirPath The directory where the executable will be placed.
	 * @param llvmIrFile    The path to the generated .ll file.
	 * @param config        The compiler configuration.
	 */
	private static void compileAndRunLLVM(Path outputDirPath, Path llvmIrFile, CompilerConfig config)
	{
		// 1. Define all necessary file paths
		Path runtimeCppFile = Paths.get("compiler/src/main/cpp/nebula_runtime.cpp");
		if (!Files.exists(runtimeCppFile))
		{
			System.err.println("FATAL: C++ runtime file not found at: " + runtimeCppFile.toAbsolutePath());
			return;
		}
		Path runtimeBitcodeFile = outputDirPath.resolve("nebula_runtime.bc");
		Path mainBitcodeFile = outputDirPath.resolve(llvmIrFile.getFileName().toString().replace(".ll", ".bc"));
		Path executableFile = outputDirPath.resolve("main");

		// 2. Compile C++ runtime to LLVM bitcode using clang++
		System.out.println("\n--- Compiling C++ Runtime to LLVM Bitcode ---");
		try
		{
			// *** FIX 1: Explicitly use clang++ from the LLVM compiler path for consistency ***
			List<String> runtimeCompileCommand = List.of(
					config.getLlvmCompilerPath() + "++", // Use clang++
					"-flto",
					"-O3",
					"-emit-llvm",
					"-c",
					runtimeCppFile.toAbsolutePath().toString(),
					"-o",
					runtimeBitcodeFile.toAbsolutePath().toString()
			);

			System.out.println("Executing: " + String.join(" ", runtimeCompileCommand));
			Process p = new ProcessBuilder(runtimeCompileCommand).inheritIO().start();
			if (p.waitFor() != 0)
			{
				System.err.println("Failed to compile C++ runtime to bitcode. Aborting.");
				return;
			}
			System.out.println("C++ runtime compiled to bitcode successfully.");
		}
		catch (IOException | InterruptedException e)
		{
			System.err.println("Error compiling C++ runtime: " + e.getMessage());
			Thread.currentThread().interrupt();
			return;
		}

		// 3. Compile generated LLVM IR (.ll) to bitcode (.bc) using clang
		System.out.println("\n--- Compiling Generated LLVM IR to Bitcode ---");
		try
		{
			List<String> llvmCompileCommand = List.of(
					config.getLlvmCompilerPath(), // Use clang
					"-emit-llvm",
					"-c",
					llvmIrFile.toAbsolutePath().toString(),
					"-o",
					mainBitcodeFile.toAbsolutePath().toString()
			);

			System.out.println("Executing: " + String.join(" ", llvmCompileCommand));
			Process p = new ProcessBuilder(llvmCompileCommand).inheritIO().start();
			if (p.waitFor() != 0)
			{
				System.err.println("Failed to compile LLVM IR to bitcode. Aborting.");
				return;
			}
			System.out.println("LLVM IR compiled to bitcode successfully.");
		}
		catch (IOException | InterruptedException e)
		{
			System.err.println("Error compiling LLVM IR: " + e.getMessage());
			Thread.currentThread().interrupt();
			return;
		}

		// 4. Link everything together using LTO with clang++
		System.out.println("\n--- Linking Executable with LTO ---");
		Path fullLibraryPath = Paths.get(config.getNdkLibraryPath());
		Path libraryDir = fullLibraryPath.getParent();
		String libraryName = fullLibraryPath.getFileName().toString().replaceAll("^lib|\\.(so|dylib)$", "");

		try
		{
			List<String> linkCommand = new ArrayList<>();
			// *** FIX 2: Use clang++ for linking to correctly handle LLVM bitcode and LTO ***
			linkCommand.add(config.getLlvmCompilerPath() + "++"); // CRITICAL: Use clang++ here, not c++
			linkCommand.add("-flto");
			linkCommand.add("-O3");
			linkCommand.add(mainBitcodeFile.toAbsolutePath().toString());
			linkCommand.add(runtimeBitcodeFile.toAbsolutePath().toString());
			linkCommand.add("-L" + libraryDir.toAbsolutePath());
			linkCommand.add("-l" + libraryName);
			linkCommand.add("-Wl,-rpath," + libraryDir.toAbsolutePath());
			linkCommand.add("-o");
			linkCommand.add(executableFile.toAbsolutePath().toString());

			System.out.println("Executing: " + String.join(" ", linkCommand));
			Process p = new ProcessBuilder(linkCommand).inheritIO().start();
			if (p.waitFor() == 0)
			{
				System.out.println("Linking successful. Executable created.");

				// 5. Run the final program
				System.out.println("\n--- Running Program ---");
				Process runProcess = new ProcessBuilder(executableFile.toAbsolutePath().toString()).inheritIO().start();
				runProcess.waitFor();
				System.out.println("\n--- Program Finished ---");
			}
			else
			{
				System.err.println("Linking failed.");
			}
		}
		catch (IOException | InterruptedException e)
		{
			System.err.println("Error during linking or execution: " + e.getMessage());
			Thread.currentThread().interrupt();
		}
	}

	private static class StreamGobbler implements Runnable
	{
		private final InputStream inputStream;
		private final String prefix;

		public StreamGobbler(InputStream inputStream, String prefix)
		{
			this.inputStream = inputStream;
			this.prefix = prefix;
		}

		@Override
		public void run()
		{
			try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream)))
			{
				String line;
				while ((line = reader.readLine()) != null)
				{
					System.out.println(prefix + line);
				}
			}
			catch (IOException e)
			{
				System.err.println("Error reading process stream: " + e.getMessage());
			}
		}
	}

	private static void copyCppSdkFiles(Path sourceDir, Path destDir)
	{
		System.out.println("\n--- Copying C++ SDK files from: " + sourceDir.toAbsolutePath() + " to " + destDir.toAbsolutePath() + " ---");
		try (Stream<Path> stream = Files.walk(sourceDir))
		{
			stream.forEach(sourcePath ->
			{
				try
				{
					Path relativePath = sourceDir.relativize(sourcePath);
					Path destinationPath = destDir.resolve(relativePath);

					if (Files.isDirectory(sourcePath))
					{
						Files.createDirectories(destinationPath);
					}
					else
					{
						Files.copy(sourcePath, destinationPath, StandardCopyOption.REPLACE_EXISTING);
						System.out.println("Copied: " + sourcePath.getFileName());
					}
				}
				catch (IOException e)
				{
					System.err.println("Error copying file '" + sourcePath + "': " + e.getMessage());
				}
			});
		}
		catch (IOException e)
		{
			System.err.println("Error walking C++ SDK directory '" + sourceDir + "': " + e.getMessage());
		}
		System.out.println("---------------------------------------------------------------------------\n");
	}
}