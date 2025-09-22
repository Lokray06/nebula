package com.juanpa.nebula.compiler.util;

import java.util.Properties;

/**
 * Holds configuration settings for the Nebula compiler, loaded from a properties file.
 * Provides sensible defaults if settings are not specified.
 */
public class CompilerConfig
{
	private final String cppCompilerPath;
	private final String llvmCompilerPath;
	private final String ndkHeaderPath;
	private final String ndkLibraryPath;
	private final boolean ownershipOptimizationEnabled; // --- NEW ---

	public CompilerConfig(Properties props)
	{
		this.cppCompilerPath = props.getProperty("compiler.cpp_path", "c++");
		this.llvmCompilerPath = props.getProperty("compiler.llvm_path", "clang");
		this.ndkHeaderPath = props.getProperty("ndk.header_path", "ndk_cpp");
		this.ndkLibraryPath = props.getProperty("ndk.library_path", "libs/libndk.so");
		// --- NEW: Load the optimization flag, defaulting to true ---
		this.ownershipOptimizationEnabled = Boolean.parseBoolean(props.getProperty("compiler.ownership_optimization", "true"));
	}

	public String getCppCompilerPath()
	{
		return cppCompilerPath;
	}
	public String getLlvmCompilerPath()
	{
		return llvmCompilerPath;
	}

	public String getNdkHeaderPath()
	{
		return ndkHeaderPath;
	}

	public String getNdkLibraryPath()
	{
		return ndkLibraryPath;
	}

	// --- NEW: Getter for the flag ---
	public boolean isOwnershipOptimizationEnabled()
	{
		return ownershipOptimizationEnabled;
	}
}