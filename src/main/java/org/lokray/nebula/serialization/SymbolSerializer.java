package org.lokray.nebula.serialization;

import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.lexer.TokenType;
import org.lokray.nebula.semantics.*;

import java.io.*;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Serializes and deserializes compiler symbols to/from a binary format (.nsf).
 * This allows for pre-compiling the NDK into a symbol cache for faster lookups.
 */
public class SymbolSerializer
{
	private static final int NSF_VERSION = 1;

	// Helper to write a potentially null string to the stream.
	private void writeString(DataOutputStream out, String s) throws IOException
	{
		out.writeBoolean(s != null);
		if (s != null)
		{
			out.writeUTF(s);
		}
	}

	// Helper to read a potentially null string from the stream.
	private String readString(DataInputStream in) throws IOException
	{
		if (in.readBoolean())
		{
			return in.readUTF();
		}
		return null;
	}

	public void writeSymbols(Map<String, ClassSymbol> declaredClasses, OutputStream os) throws IOException
	{
		try (DataOutputStream out = new DataOutputStream(os))
		{
			// --- Header ---
			out.writeInt(NSF_VERSION);

			// --- Classes ---
			out.writeInt(declaredClasses.size());
			for (ClassSymbol cs : declaredClasses.values())
			{
				// Class Info
				writeString(out, cs.getFqn());
				out.writeBoolean(cs.isNative());
				Type superClassType = cs.getType().getSuperClassType();
				writeString(out, superClassType != null ? superClassType.getName() : null);

				// Fields
				List<VariableSymbol> fields = new ArrayList<>();
				for (Symbol sym : cs.getClassScope().getSymbols().values())
				{
					if (sym instanceof VariableSymbol)
					{
						fields.add((VariableSymbol) sym);
					}
				}
				out.writeInt(fields.size());
				for (VariableSymbol vs : fields)
				{
					writeString(out, vs.getName());
					writeString(out, vs.getType().getName());
					out.writeBoolean(vs.isStatic());
					out.writeBoolean(vs.isPublic());
					out.writeBoolean(vs.isConst());
					out.writeBoolean(vs.isWrapper());
					writeString(out, vs.getCppTarget());
				}

				// Methods
				List<MethodSymbol> allMethods = new ArrayList<>();
				cs.methodsByName.values().forEach(allMethods::addAll);
				out.writeInt(allMethods.size());
				for (MethodSymbol ms : allMethods)
				{
					writeString(out, ms.getName());
					writeString(out, ms.getType().getName()); // Return type
					out.writeBoolean(ms.isStatic());
					out.writeBoolean(ms.isPublic());
					out.writeBoolean(ms.isConstructor());
					out.writeBoolean(ms.isWrapper());
					writeString(out, ms.getCppTarget());

					// Parameter types
					out.writeInt(ms.getParameterTypes().size());
					for (Type paramType : ms.getParameterTypes())
					{
						writeString(out, paramType.getName());
					}
				}
			}
		}
	}

	/**
	 * Reads symbols using a two-pass approach on the input stream.
	 * It relies on the stream supporting mark/reset, which is handled
	 * by the BufferedInputStream in Main.java.
	 *
	 * @param is The input stream of the .nsf file.
	 * @return A map of fully qualified class names to their ClassSymbol objects.
	 * @throws IOException if the stream cannot be read or is in the wrong format.
	 */
	public Map<String, ClassSymbol> readSymbols(InputStream is) throws IOException
	{
		// We MUST mark the stream at the beginning to be able to reset.
		// The read limit is set to a large value to ensure the mark remains valid.
		if (!is.markSupported())
		{
			throw new IOException("Input stream does not support mark/reset, which is required for symbol deserialization.");
		}
		is.mark(Integer.MAX_VALUE);

		Map<String, ClassSymbol> preloadedSymbols = new LinkedHashMap<>();
		DataInputStream in = new DataInputStream(is);

		// --- Pass 1: Read only FQNs to create stub ClassSymbol objects ---
		int version = in.readInt();
		if (version != NSF_VERSION)
		{
			throw new IOException("Unsupported .nsf version. Expected " + NSF_VERSION + ", but found " + version);
		}
		int classCount = in.readInt();
		for (int i = 0; i < classCount; i++)
		{
			String fqn = readString(in);
			// Create dummy types/tokens for now. They will be linked later.
			Token dummyToken = new Token(TokenType.IDENTIFIER, getSimpleName(fqn), null, 0, 0);
			ClassType ct = new ClassType(fqn, null, null);
			ClassSymbol cs = new ClassSymbol(getSimpleName(fqn), ct, dummyToken, new SymbolTable(null, "Class:" + fqn), false);
			ct.classSymbol = cs;
			preloadedSymbols.put(fqn, cs);

			// Skip the rest of the data for this class in the first pass
			skipClassData(in);
		}

		// --- Reset stream to the beginning to start the second pass ---
		is.reset();
		in = new DataInputStream(is); // Re-wrap the reset stream

		// --- Pass 2: Read full details and populate the stub objects ---
		in.readInt(); // Skip version
		in.readInt(); // Skip class count

		for (int i = 0; i < classCount; i++)
		{
			String fqn = readString(in);
			ClassSymbol cs = preloadedSymbols.get(fqn);

			// The ClassSymbol itself is now a placeholder, we just need to fill its fields
			boolean isNative = in.readBoolean();
			// TODO: You might want to update the isNative flag on the symbol here if needed
			// cs.setNative(isNative); <-- if you add a setter

			String superclassFqn = readString(in);
			if (superclassFqn != null)
			{
				// The superclass type is a dummy for now; it will be linked in SemanticAnalyzer
				cs.getType().setSuperClassType(new ClassType(superclassFqn, null, null));
			}

			// Read Fields
			int fieldCount = in.readInt();
			for (int j = 0; j < fieldCount; j++)
			{
				String name = readString(in);
				String typeName = readString(in);
				boolean isStatic = in.readBoolean();
				boolean isPublic = in.readBoolean();
				boolean isConst = in.readBoolean();
				boolean isWrapper = in.readBoolean();
				String cppTarget = readString(in);

				VariableSymbol vs = new VariableSymbol(name, new PrimitiveType(typeName), new Token(TokenType.IDENTIFIER, name, null, 0, 0), true, isStatic, isConst, isPublic, isWrapper, cppTarget, null);
				vs.setOwnerClass(cs);
				cs.getClassScope().define(vs);
			}

			// Read Methods
			int methodCount = in.readInt();
			for (int j = 0; j < methodCount; j++)
			{
				String name = readString(in);
				String returnTypeName = readString(in);
				boolean isStatic = in.readBoolean();
				boolean isPublic = in.readBoolean();
				boolean isConstructor = in.readBoolean();
				boolean isWrapper = in.readBoolean();
				String cppTarget = readString(in);

				int paramCount = in.readInt();
				List<Type> paramTypes = new ArrayList<>();
				for (int k = 0; k < paramCount; k++)
				{
					paramTypes.add(new PrimitiveType(readString(in)));
				}

				MethodSymbol ms;
				if (isConstructor)
				{
					ms = new MethodSymbol(name, paramTypes, new Token(TokenType.IDENTIFIER, name, null, 0, 0), new SymbolTable(cs.getClassScope(), "ctor"), isPublic);
				}
				else
				{
					ms = new MethodSymbol(name, new PrimitiveType(returnTypeName), paramTypes, new Token(TokenType.IDENTIFIER, name, null, 0, 0), new SymbolTable(cs.getClassScope(), "method"), isStatic, isPublic, isWrapper, cppTarget);
				}
				ms.setOwnerClass(cs);
				cs.defineMethod(ms);
			}
		}
		return preloadedSymbols;
	}

	// Helper to skip the variable-length data of a class entry during the first pass.
	private void skipClassData(DataInputStream in) throws IOException
	{
		in.readBoolean(); // isNative
		readString(in);   // superclassFqn

		// Skip fields
		int fieldCount = in.readInt();
		for (int i = 0; i < fieldCount; i++)
		{
			readString(in); // name
			readString(in); // type fqn
			in.readBoolean(); // isStatic
			in.readBoolean(); // isPublic
			in.readBoolean(); // isConst
			in.readBoolean(); // isWrapper
			readString(in); // cppTarget
		}

		// Skip methods
		int methodCount = in.readInt();
		for (int i = 0; i < methodCount; i++)
		{
			readString(in); // name
			readString(in); // return type fqn
			in.readBoolean(); // isStatic
			in.readBoolean(); // isPublic
			in.readBoolean(); // isConstructor
			in.readBoolean(); // isWrapper
			readString(in); // cppTarget

			// Skip parameters
			int paramCount = in.readInt();
			for (int j = 0; j < paramCount; j++)
			{
				readString(in); // param type fqn
			}
		}
	}

	private String getSimpleName(String fqn)
	{
		if (fqn == null)
		{
			return "";
		}
		int lastDot = fqn.lastIndexOf('.');
		return (lastDot == -1) ? fqn : fqn.substring(lastDot + 1);
	}
}