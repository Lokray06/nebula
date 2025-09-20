package com.juanpa.nebula.compiler.util;

public class Debug
{
	/**
	 * Master switch for all debug logging. Set to false to disable all output.
	 */
	public static final boolean ENABLED = false;

	private static int indentLevel = 0;

	/**
	 * Logs a formatted message if debugging is enabled.
	 *
	 * @param format The message format string (e.g., "Found var: %s").
	 * @param args   The arguments to format into the message.
	 */
	public static void log(String format, Object... args)
	{
		if (ENABLED)
		{
			String indent = "  ".repeat(indentLevel);
			System.out.println("[DEBUG] " + indent + String.format(format, args));
		}
	}

	/**
	 * Increases the indentation level for subsequent log messages.
	 */
	public static void indent()
	{
		if (ENABLED)
		{
			indentLevel++;
		}
	}

	/**
	 * Decreases the indentation level for subsequent log messages.
	 */
	public static void dedent()
	{
		if (ENABLED)
		{
			indentLevel = Math.max(0, indentLevel - 1);
		}
	}
}