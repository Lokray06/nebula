// File: src/main/java/com/juanpa/nebula/transpiler/ProjectLoader.java
package com.juanpa.nebula.compiler;

import com.juanpa.nebula.compiler.ast.Program;
import com.juanpa.nebula.compiler.lexer.Lexer;
import com.juanpa.nebula.compiler.lexer.Token;
import com.juanpa.nebula.compiler.parser.NebulaParser;
import com.juanpa.nebula.compiler.semantics.ClassSymbol;
import com.juanpa.nebula.compiler.util.Debug;
import com.juanpa.nebula.compiler.util.ErrorReporter;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

/**
 * Manages the loading of a Nebula project by discovering and parsing all
 * source files from a given set of source directories.
 */
public class ProjectLoader
{
	private final List<Path> sourceRoots;
	private final ErrorReporter errorReporter;
	private final Queue<Path> filesToParse = new LinkedList<>();
	private final Set<Path> parsedFiles = new HashSet<>();
	private final Program combinedAst = new Program();
	private final Map<String, ClassSymbol> preloadedSymbols;

	public ProjectLoader(List<Path> sourceRoots, ErrorReporter errorReporter, Map<String, ClassSymbol> preloadedSymbols)
	{
		this.sourceRoots = sourceRoots;
		this.errorReporter = errorReporter;
		this.preloadedSymbols = (preloadedSymbols != null) ? preloadedSymbols : new HashMap<>();
	}

	/**
	 * Scans all configured source roots for .neb files, parses them,
	 * and merges them into a single Program AST. This is the primary method
	 * for both regular project and NDK compilation.
	 */
	public Program loadProjectFromSourceRoots() throws IOException
	{
		Debug.log("Discovering source files from all source roots...");
		for (Path rootDir : this.sourceRoots)
		{
			if (!Files.exists(rootDir) || !Files.isDirectory(rootDir))
			{
				System.out.println("Warning: Source root not found or is not a directory: " + rootDir);
				continue;
			}
			try (Stream<Path> stream = Files.walk(rootDir))
			{
				stream.filter(path -> !Files.isDirectory(path) && path.toString().endsWith(".neb")).forEach(filesToParse::add);
			}
		}

		if (filesToParse.isEmpty())
		{
			System.err.println("Error: No source (.neb) files found in the specified source roots.");
			return null;
		}

		// Parse every file that was discovered.
		while (!filesToParse.isEmpty())
		{
			Path currentFile = filesToParse.poll();
			Path canonicalPath = currentFile.toRealPath();
			if (parsedFiles.contains(canonicalPath))
			{
				continue; // Avoid parsing the same file twice
			}

			Debug.log("  -> Parsing: " + currentFile.getFileName());
			Program fileAst = parseFile(canonicalPath);

			if (errorReporter.hasErrors())
			{
				System.err.println("Stopping build due to parsing errors in " + currentFile.getFileName());
				return null;
			}
			parsedFiles.add(canonicalPath);

			// Merge this file's AST into the main one
			fileAst.getImportDirectives().forEach(combinedAst::addImportDirective);
			fileAst.getNamespaceDeclarations().forEach(combinedAst::addNamespace);
		}

		return combinedAst;
	}

	/**
	 * Parses a single Nebula source file into an AST.
	 */
	private Program parseFile(Path filePath) throws IOException
	{
		String sourceCode = new String(Files.readAllBytes(filePath), StandardCharsets.UTF_8);

		Lexer lexer = new Lexer(sourceCode, errorReporter);
		List<Token> tokens = lexer.scanTokens();
		if (errorReporter.hasErrors())
		{
			return null;
		}

		// Note: When you implement the file name check, you'll pass the filename here.
		// String expectedClassName = filePath.getFileName().toString().replaceAll("\\.neb$", "");
		// NebulaParser parser = new NebulaParser(tokens, errorReporter, expectedClassName);
		NebulaParser parser = new NebulaParser(tokens, errorReporter);
		return parser.parse();
	}
}