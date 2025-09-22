package org.lokray.nebula.util;

public class ErrorReporter
{
	private boolean hasErrors = false; // Flag to indicate if any errors have been reported

	/**
	 * Reports a transpilation error.
	 *
	 * @param line    The line number where the error occurred.
	 * @param column  The column number where the error occurred.
	 * @param message The error message.
	 */
	public void report(int line, int column, String message)
	{
		System.err.println("[Error] Line " + line + ", Column " + column + ": " + message);
		hasErrors = true;
	}

	/**
	 * Checks if any errors have been reported.
	 *
	 * @return True if errors exist, false otherwise.
	 */
	public boolean hasErrors()
	{
		return hasErrors;
	}

	/**
	 * Resets the error flag.
	 */
	public void reset()
	{
		hasErrors = false;
	}
}