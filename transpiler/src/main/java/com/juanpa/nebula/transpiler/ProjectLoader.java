// File: src/main/java/com/juanpa/nebula/transpiler/ProjectLoader.java
package com.juanpa.nebula.transpiler;

import com.juanpa.nebula.transpiler.ast.Program;
import com.juanpa.nebula.transpiler.ast.declarations.ImportDirective;
import com.juanpa.nebula.transpiler.ast.declarations.NamespaceDeclaration;
import com.juanpa.nebula.transpiler.lexer.Lexer;
import com.juanpa.nebula.transpiler.lexer.Token;
import com.juanpa.nebula.transpiler.parser.NebulaParser;
import com.juanpa.nebula.transpiler.util.ErrorReporter;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/**
 * Manages the loading of a Nebula project by discovering and parsing all
 * necessary source files starting from a single entry point.
 */
public class ProjectLoader
{
	private final List<Path> sourceRoots;
	private final ErrorReporter errorReporter;
	private final Queue<Path> filesToParse = new LinkedList<>();
	private final Set<Path> parsedFiles = new HashSet<>();
	private final Program combinedAst = new Program();

	public ProjectLoader(List<Path> sourceRoots, ErrorReporter errorReporter)
	{
		this.sourceRoots = sourceRoots;
		this.errorReporter = errorReporter;
	}

	/**
	 * Loads the entire project, starting from the entry point file, by recursively
	 * finding and parsing all dependencies.
	 *
	 * @param entryPoint The main file of the project.
	 * @return The combined Program AST of the entire project.
	 */
	public Program loadProjectFromEntryPoint(Path entryPoint) throws IOException
	{
		// Add the initial file to our work queue
		filesToParse.add(entryPoint.toAbsolutePath());

		while(!filesToParse.isEmpty())
		{
			Path currentFile = filesToParse.poll();

			// Use a canonical path to avoid parsing the same file via different relative paths
			Path canonicalPath = currentFile.toRealPath();
			if(parsedFiles.contains(canonicalPath))
			{
				continue; // Already processed this file, handles circular dependencies
			}

			System.out.println("Discovering and parsing: " + canonicalPath.getFileName());

			Program fileAst = parseFile(canonicalPath);
			if(errorReporter.hasErrors())
			{
				System.err.println("Stopping build due to parsing errors in " + canonicalPath.getFileName());
				return null; // Stop if there's an error
			}

			// Mark as parsed
			parsedFiles.add(canonicalPath);

			// Merge the AST of the current file into the main project AST
			fileAst.getImportDirectives().forEach(combinedAst::addImportDirective);
			fileAst.getNamespaceDeclarations().forEach(combinedAst::addNamespace);

			// Discover new dependencies from the current file's imports
			discoverDependencies(fileAst);
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
		if(errorReporter.hasErrors())
			return null;

		NebulaParser parser = new NebulaParser(tokens, errorReporter);
		return parser.parse();
	}

	/**
	 * Examines the import directives of a given AST to find new files to parse.
	 */
	private void discoverDependencies(Program ast)
	{
		for(ImportDirective directive : ast.getImportDirectives())
		{
			String fqn = getQualifiedNameFromExpression(directive.getQualifiedName());
			if(fqn == null)
				continue;

			// Try to find the corresponding .neb file in our source roots
			findAndQueueFileForFqn(fqn);
		}
	}

	/**
	 * Converts a fully qualified name (e.g., "Program.Vector2") to a relative file path
	 * ("Program/Vector2.neb") and searches for it in the source roots.
	 */
	private void findAndQueueFileForFqn(String fqn)
	{
		// Don't search for core library files, they are handled by the pre-built SDK metadata
		if(fqn.startsWith("nebula.core") || fqn.startsWith("nebula.io"))
		{
			return;
		}

		String relativePathString = fqn.replace('.', '/') + ".neb";

		for(Path root : sourceRoots)
		{
			Path potentialFile = root.resolve(relativePathString);
			if(Files.exists(potentialFile))
			{
				// Found a potential match, add it to the queue to be parsed
				filesToParse.add(potentialFile.toAbsolutePath());
				return; // Found it, no need to check other roots
			}
		}
	}

	// Helper to get the FQN string from an import directive's expression
	private String getQualifiedNameFromExpression(com.juanpa.nebula.transpiler.ast.expressions.Expression expression)
	{
		if(expression instanceof com.juanpa.nebula.transpiler.ast.expressions.IdentifierExpression)
		{
			return ((com.juanpa.nebula.transpiler.ast.expressions.IdentifierExpression) expression).getName().getLexeme();
		}
		else if(expression instanceof com.juanpa.nebula.transpiler.ast.expressions.DotExpression)
		{
			com.juanpa.nebula.transpiler.ast.expressions.DotExpression dot = (com.juanpa.nebula.transpiler.ast.expressions.DotExpression) expression;
			String leftPart = getQualifiedNameFromExpression(dot.getLeft());
			return leftPart != null ? leftPart + "." + dot.getMemberName().getLexeme() : null;
		}
		return null;
	}
}