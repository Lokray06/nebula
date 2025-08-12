// File: src/main/java/com/juanpa/nebula/transpiler/ast/declarations/AccessorDeclaration.java
package com.juanpa.nebula.transpiler.ast.declarations;

import com.juanpa.nebula.transpiler.ast.ASTNode;
import com.juanpa.nebula.transpiler.ast.ASTVisitor;
import com.juanpa.nebula.transpiler.ast.statements.BlockStatement;
import com.juanpa.nebula.transpiler.lexer.Token;

import java.util.List;

public class AccessorDeclaration implements ASTNode
{
	private final Token keyword; // 'get' or 'set'
	private final List<Token> modifiers;
	private final BlockStatement body;
	private final Token semicolon;

	public AccessorDeclaration(Token keyword, List<Token> modifiers, BlockStatement body, Token semicolon)
	{
		this.keyword = keyword;
		this.modifiers = modifiers;
		this.body = body;
		this.semicolon = semicolon;
	}

	public Token getKeyword()
	{
		return keyword;
	}

	public List<Token> getModifiers()
	{
		return modifiers;
	}

	public BlockStatement getBody()
	{
		return body;
	}

	public boolean isAuto()
	{
		return body == null && semicolon != null;
	}

	@Override
	public <R> R accept(ASTVisitor<R> visitor)
	{
		// Accessors are visited as part of the PropertyDeclaration,
		// so they don't typically need their own visit method.
		return null;
	}
}