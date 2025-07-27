package com.juanpa.nebula.transpiler.util;

import java.util.Properties;

/**
 * Holds configuration settings for the Nebula compiler, loaded from a properties file.
 * Provides sensible defaults if settings are not specified.
 */
public class CompilerConfig
{
	private final String cppCompilerPath;
	private final String ndkHeaderPath;
	private final String ndkLibraryPath;

	public CompilerConfig(Properties props)
	{
		// Load properties with new keys and sensible defaults
		this.cppCompilerPath = props.getProperty("compiler.cpp_path", "c++");
		this.ndkHeaderPath = props.getProperty("ndk.header_path", "ndk_cpp");
		// For the library path, we default to a 'libs' subdirectory in the current project
		this.ndkLibraryPath = props.getProperty("ndk.library_path", "libs/libndk.so");
	}

	public String getCppCompilerPath()
	{
		return cppCompilerPath;
	}

	public String getNdkHeaderPath()
	{
		return ndkHeaderPath;
	}

	public String getNdkLibraryPath()
	{
		return ndkLibraryPath;
	}
}