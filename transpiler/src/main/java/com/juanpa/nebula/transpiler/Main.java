// File: src/main/java/com/juanpa/nebula/transpiler/Main.java

package com.juanpa.nebula.transpiler;

import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.codegen.CppGenerator;
import com.juanpa.nebula.transpiler.semantics.ClassSymbol;
import com.juanpa.nebula.transpiler.semantics.SemanticAnalyzer;
import com.juanpa.nebula.transpiler.serialization.SymbolSerializer;
import com.juanpa.nebula.transpiler.util.CompilerConfig;
import com.juanpa.nebula.transpiler.util.ErrorReporter;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.*;
import java.util.stream.Stream;

/**
 * Entry point for the Nebula Transpiler.
 * This Main class orchestrates the entire compilation process.
 */
public class Main
{

	public static void main(String[] args)
	{
		// 1. Load Compiler Configuration
		CompilerConfig config = loadConfiguration();

		// --- Argument parsing for different compiler modes ---
		if (args.length > 0)
		{
			if (args[0].equals("--compile-ndk"))
			{
				// Update the usage message to include the new argument
				if (args.length < 7 || !args[1].equals("--ndk-source") || !args[3].equals("--output-symbols") || !args[5].equals("--output-cpp"))
				{
					System.err.println("Usage: nbcc --compile-ndk --ndk-source <ndk_src_dir> --output-symbols <file.nsf> --output-cpp <dir>");
					return;
				}
				Path ndkSourceDir = Paths.get(args[2]);
				Path nsfFile = Paths.get(args[4]);
				Path cppDir = Paths.get(args[6]);

				// Pass the new NDK source path to the compilation method
				compileNdk(ndkSourceDir, nsfFile, cppDir, config);
				return;
			}
		}

		if (args.length == 0)
		{
			System.err.println("Usage: nbcc <entrypoint.neb>");
			System.err.println("   or: nbcc --compile-ndk --ndk-source <ndk_src_dir> --output-symbols <file.nsf> --output-cpp <dir>");
			return;
		}

		// --- Standard Compilation Flow ---
		Path entryPointFile = Paths.get(args[0]);
		if (!Files.exists(entryPointFile))
		{
			System.err.println("Error: Entry point file not found: " + entryPointFile);
			return;
		}

		ErrorReporter errorReporter = new ErrorReporter();
		SemanticAnalyzer semanticAnalyzer = new SemanticAnalyzer(errorReporter, config);

		Map<String, ClassSymbol> preloadedSymbols = new HashMap<>();
		Path symbolCacheFile = Paths.get("ndk.nsf");
		if (Files.exists(symbolCacheFile))
		{
			System.out.println("--- Found NDK symbol cache (ndk.nsf). Loading... ---");
			try (InputStream is = new BufferedInputStream(new FileInputStream(symbolCacheFile.toFile())))
			{
				SymbolSerializer serializer = new SymbolSerializer();
				preloadedSymbols = serializer.readSymbols(is);
				semanticAnalyzer.preloadSymbols(preloadedSymbols);
				System.out.println("--- NDK symbols loaded successfully. ---");
			}
			catch (IOException e)
			{
				System.err.println("Warning: Could not read symbol cache file 'ndk.nsf'. Error: " + e.getMessage());
			}
		}

		List<Path> sourceRoots = new ArrayList<>();
		sourceRoots.add(entryPointFile.getParent());
		sourceRoots.add(Paths.get("./sdk"));

		Program fullProgramAST;
		try
		{
			System.out.println("--- Loading project from entry point: " + entryPointFile.getFileName() + " ---");
			ProjectLoader loader = new ProjectLoader(sourceRoots, errorReporter, preloadedSymbols);
			fullProgramAST = loader.loadProjectFromEntryPoint(entryPointFile);

			if (fullProgramAST == null || errorReporter.hasErrors())
			{
				System.err.println("Build failed due to parsing errors.");
				return;
			}

		}
		catch (IOException e)
		{
			System.err.println("An I/O error occurred during project loading: " + e.getMessage());
			return;
		}

		System.out.println("\n--- Semantic Analysis Phase (Entire Project) ---");
		semanticAnalyzer.analyze(fullProgramAST);

		if (errorReporter.hasErrors())
		{
			System.out.println("Semantic analysis finished with errors.");
			return;
		}
		System.out.println("Semantic analysis completed successfully.");

		System.out.println("\n--- Code Generation Phase ---");
		Path projectRoot = entryPointFile.getParent();
		CppGenerator cppGenerator = new CppGenerator(semanticAnalyzer.getDeclaredClasses(), semanticAnalyzer);
		Map<String, String> generatedCode = cppGenerator.generate(fullProgramAST);

		if (errorReporter.hasErrors())
		{
			System.out.println("Code generation finished with errors.");
		}
		else
		{
			System.out.println("Code generation completed successfully.");
		}

		Path outputDirPath = projectRoot.resolve("out");
		try
		{
			Files.createDirectories(outputDirPath);
			System.out.println("\n--- Saving Generated C++ Files to: " + outputDirPath.toAbsolutePath() + " ---");

			for (Map.Entry<String, String> entry : generatedCode.entrySet())
			{
				saveToFile(outputDirPath.resolve(entry.getKey()), entry.getValue());
			}

			if (!generatedCode.isEmpty() && !errorReporter.hasErrors())
			{
				System.out.println("\n--- Compiling and Running Generated C++ Code ---");
				compileAndRunCpp(outputDirPath, generatedCode, config);
			}
		}
		catch (IOException e)
		{
			System.err.println("Error creating output directory or saving files: " + e.getMessage());
		}

		System.out.println("\nTranspilation process finished.");
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
			ndkAst = loader.loadAllFilesFromRoot(ndkSourceDir);
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