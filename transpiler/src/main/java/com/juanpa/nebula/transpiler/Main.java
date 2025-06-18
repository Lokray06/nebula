// File: src/main/java/com/juanpa.nebula.transpiler/Main.java

package com.juanpa.nebula.transpiler;

import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.ast.declarations.ImportDirective;
import com.juanpa.nebula.transpiler.ast.declarations.NamespaceDeclaration;
import com.juanpa.nebula.transpiler.codegen.CppGenerator;
import com.juanpa.nebula.transpiler.lexer.Lexer;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.parser.NebulaParser;
import com.juanpa.nebula.transpiler.semantics.SemanticAnalyzer;
import com.juanpa.nebula.transpiler.util.ErrorReporter;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/**
 * Entry point for the Nebula Transpiler.
 * This Main class orchestrates the lexical, syntactic, and semantic analysis phases,
 * and now handles C++ code generation and saving, as well as compilation and execution.
 */
public class Main
{
	public static void main(String[] args)
	{
		Path projectPath;
		if(args.length > 0)
		{
			projectPath = Paths.get(args[0]);
		}
		else
		{
			projectPath = Paths.get("."); // Default to current working directory
			System.out.println("No project path provided. Using default: current working directory ('.').");
		}

		ErrorReporter errorReporter = new ErrorReporter();

		// Determine the base path for Nebula SDK files
		Path nebulaSdkBasePath = projectPath.resolve("sdk");

		List<Path> nebulaFiles = new ArrayList<>();
		try(Stream<Path> stream = Files.walk(nebulaSdkBasePath))
		{
			stream.filter(Files::isRegularFile)
					.filter(p -> p.toString().endsWith(".neb"))
					.forEach(nebulaFiles::add);
		}
		catch(IOException e)
		{
			System.err.println("Error reading Nebula SDK files: " + e.getMessage());
			return;
		}

		if(nebulaFiles.isEmpty())
		{
			System.out.println("Found 0 Nebula files in the SDK path: " + nebulaSdkBasePath);
			return;
		}
		System.out.println("--- Loading Nebula Project from: " + nebulaSdkBasePath + " ---");
		System.out.println("Found " + nebulaFiles.size() + " Nebula files. Processing...");

		// Create a single aggregated Program AST node
		Program fullProgramAST = new Program();

		// First Pass: Lex and Parse all Nebula files to build the ASTs
		// And aggregate them into a single fullProgramAST
		for(Path filePath : nebulaFiles)
		{
			System.out.println("\n--- Processing file: " + filePath.getFileName() + " ---");
			try(InputStream inputStream = Files.newInputStream(filePath))
			{
				String sourceCode = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);

				// Lexical Analysis
				Lexer lexer = new Lexer(sourceCode, errorReporter);
				List<Token> tokens = lexer.scanTokens();
				if(errorReporter.hasErrors())
				{
					System.out.println("Lexing failed for " + filePath.getFileName() + ".");
					return;
				}
				System.out.println("Lexing completed successfully for " + filePath.getFileName() + ".");

				// Syntactic Analysis
				NebulaParser parser = new NebulaParser(tokens, errorReporter);
				Program currentFileProgram = parser.parse(); // Parse the current file
				if(errorReporter.hasErrors())
				{
					System.out.println("Parsing failed for " + filePath.getFileName() + ".");
					return;
				}
				System.out.println("Parsing completed successfully for " + filePath.getFileName() + ".");

				// Aggregate namespaces and import directives from current file into the full program AST
				for(ImportDirective directive : currentFileProgram.getImportDirectives())
				{
					fullProgramAST.addImportDirective(directive);
				}
				for(NamespaceDeclaration namespaceDecl : currentFileProgram.getNamespaceDeclarations())
				{
					fullProgramAST.addNamespace(namespaceDecl);
				}

			}
			catch(IOException e)
			{
				System.err.println("Error reading file " + filePath + ": " + e.getMessage());
				return;
			}
		}

		// Semantic Analysis Phase
		System.out.println("\n--- Semantic Analysis Phase (Entire Project) ---");
		SemanticAnalyzer semanticAnalyzer = new SemanticAnalyzer(errorReporter);

		// Analyze the aggregated program AST once
		semanticAnalyzer.analyze(fullProgramAST);

		if(errorReporter.hasErrors())
		{
			System.out.println("Semantic analysis finished with errors for the project. Cannot proceed to code generation.");
			return; // Exit if semantic analysis failed
		}
		System.out.println("Semantic analysis completed successfully for the project.");


		// Code Generation Phase
		System.out.println("\n--- Code Generation Phase ---");
		CppGenerator cppGenerator = new CppGenerator(semanticAnalyzer.getDeclaredClasses());
		Map<String, String> generatedCode = cppGenerator.generate(fullProgramAST);

		if(errorReporter.hasErrors())
		{
			System.out.println("Code generation finished with errors. Generated code might be incomplete or incorrect.");
			// Do not return here, allow saving partial code for debugging
		}
		else
		{
			System.out.println("Code generation completed successfully.");
		}


		// Save Generated C++ Files
		Path outputDirPath = projectPath.resolve("out");
		try
		{
			Files.createDirectories(outputDirPath); // Ensure the output directory exists
			System.out.println("\n--- Saving Generated C++ Files to: " + outputDirPath.toAbsolutePath() + " ---");

			for(Map.Entry<String, String> entry : generatedCode.entrySet())
			{
				String fileName = entry.getKey();
				String code = entry.getValue();
				saveToFile(outputDirPath.resolve(fileName), code);
			}

			// Automatically compile and run the generated C++ code
			if(!generatedCode.isEmpty() && !errorReporter.hasErrors())
			{
				System.out.println("\n--- Compiling and Running Generated C++ Code ---");
				compileAndRunCpp(outputDirPath, generatedCode);
			}
			else if(errorReporter.hasErrors())
			{
				System.out.println("\nSkipping C++ compilation and run due to previous errors.");
			}
			else
			{
				System.out.println("\nNo C++ code generated. Skipping compilation and run.");
			}

		}
		catch(IOException e)
		{
			System.err.println("Error creating output directory or saving files: " + e.getMessage());
		}

		System.out.println("\nTranspilation process finished.");
	}

	private static void saveToFile(Path filePath, String content)
	{
		try
		{
			Files.createDirectories(filePath.getParent()); // Ensure parent directories exist
			Files.write(filePath, content.getBytes(StandardCharsets.UTF_8));
			System.out.println("Saved: " + filePath.getFileName());
		}
		catch(IOException e)
		{
			System.err.println("Error saving generated code to file '" + filePath + "': " + e.getMessage());
			System.err.println(e);
		}
	}

	/**
	 * Compiles the generated C++ files and runs the executable.
	 *
	 * @param outputDirPath The directory where the generated C++ files are located.
	 * @param generatedCode A map of generated C++ filenames to their content.
	 */
	private static void compileAndRunCpp(Path outputDirPath, Map<String, String> generatedCode)
	{
		List<String> compileCommand = new ArrayList<>();
		compileCommand.add("c++"); // Use c++ (for Arch Linux, as requested)

		// Add all .cpp files to the compile command
		generatedCode.keySet().stream()
				.filter(fileName -> fileName.endsWith(".cpp"))
				.map(fileName -> outputDirPath.resolve(fileName).toAbsolutePath().toString())
				.forEach(compileCommand::add);

		compileCommand.add("-o");
		compileCommand.add(outputDirPath.resolve("main").toAbsolutePath().toString()); // Output executable name

		System.out.println("Compiling with command: " + String.join(" ", compileCommand));

		try
		{
			ProcessBuilder compileProcessBuilder = new ProcessBuilder(compileCommand);
			compileProcessBuilder.directory(outputDirPath.toFile()); // Set working directory for compilation
			Process compileProcess = compileProcessBuilder.start();

			// Read compile process output concurrently
			StreamGobbler compileStdoutGobbler = new StreamGobbler(compileProcess.getInputStream(), "[C++ stdout]: ");
			StreamGobbler compileStderrGobbler = new StreamGobbler(compileProcess.getErrorStream(), "[C++ stderr]: ");
			new Thread(compileStdoutGobbler).start();
			new Thread(compileStderrGobbler).start();

			int compileExitCode = compileProcess.waitFor();

			if(compileExitCode == 0)
			{
				System.out.println("C++ compilation successful.");
				// Run the compiled executable
				System.out.println("Running executable: " + outputDirPath.resolve("main").toAbsolutePath().toString());
				ProcessBuilder runProcessBuilder = new ProcessBuilder(outputDirPath.resolve("main").toAbsolutePath().toString());
				runProcessBuilder.directory(outputDirPath.toFile()); // Set working directory for execution
				Process runProcess = runProcessBuilder.start();

				// Read run process output concurrently
				StreamGobbler runStdoutGobbler = new StreamGobbler(runProcess.getInputStream(), "[App stdout]: ");
				StreamGobbler runStderrGobbler = new StreamGobbler(runProcess.getErrorStream(), "[App stderr]: ");
				new Thread(runStdoutGobbler).start();
				new Thread(runStderrGobbler).start();

				runProcess.waitFor(); // Wait for the application to finish
			}
			else
			{
				System.err.println("C++ compilation failed with exit code: " + compileExitCode);
			}
		}
		catch(IOException |
			  InterruptedException e)
		{
			System.err.println("Error during C++ compilation or execution: " + e.getMessage());
			Thread.currentThread().interrupt(); // Restore interrupt status
		}
	}

	/**
	 * Helper class to consume a process's input stream (stdout or stderr) in a separate thread.
	 */
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
			try(BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream)))
			{
				String line;
				while((line = reader.readLine()) != null)
				{
					System.out.println(prefix + line);
				}
			}
			catch(IOException e)
			{
				System.err.println("Error reading process stream: " + e.getMessage());
			}
		}
	}

	/**
	 * Copies all C++ SDK files from the source directory to the destination directory.
	 *
	 * @param sourceDir The path to the C++ SDK source directory (e.g., "./sdk/cpp").
	 * @param destDir   The path to the destination directory (e.g., "./out").
	 */
	private static void copyCppSdkFiles(Path sourceDir, Path destDir)
	{
		System.out.println("\n--- Copying C++ SDK files from: " + sourceDir.toAbsolutePath() + " to " + destDir.toAbsolutePath() + " ---");
		try(Stream<Path> stream = Files.walk(sourceDir))
		{
			stream.forEach(sourcePath ->
			{
				try
				{
					Path relativePath = sourceDir.relativize(sourcePath);
					Path destinationPath = destDir.resolve(relativePath);

					if(Files.isDirectory(sourcePath))
					{
						Files.createDirectories(destinationPath); // Ensure subdirectories are created
					}
					else
					{
						// Copy file, replacing existing one if it exists
						Files.copy(sourcePath, destinationPath, StandardCopyOption.REPLACE_EXISTING);
						System.out.println("Copied: " + sourcePath.getFileName());
					}
				}
				catch(IOException e)
				{
					System.err.println("Error copying file '" + sourcePath + "': " + e.getMessage());
				}
			});
		}
		catch(IOException e)
		{
			System.err.println("Error walking C++ SDK directory '" + sourceDir + "': " + e.getMessage());
		}
		System.out.println("---------------------------------------------------------------------------\n");
	}
}
