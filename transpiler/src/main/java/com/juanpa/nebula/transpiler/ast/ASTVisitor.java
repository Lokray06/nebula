// File: src/main/java/com/juanpa/nebula/transpiler/ast/ASTVisitor.java

package com.juanpa.nebula.transpiler.ast;

import com.juanpa.nebula.transpiler.ast.declarations.ClassDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.ConstructorDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.FieldDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.MethodDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.NamespaceDeclaration;
import com.juanpa.nebula.transpiler.ast.declarations.ImportDirective;
import com.juanpa.nebula.transpiler.ast.expressions.*;
import com.juanpa.nebula.transpiler.ast.statements.*;


/**
 * Interface for the Visitor pattern that allows AST traversal.
 * Each `visit` method corresponds to a specific AST node type.
 * The generic type `R` represents the return value type of the `visit` methods.
 * For expressions, `R` would be the expression's type (e.g., `Type` in semantic analysis).
 * For declarations and statements, which do not produce a value, `R` could be `Void`.
 */
public interface ASTVisitor<R>
{
	// --- Directives ---
	R visitImportDirective(ImportDirective directive);

	// --- Declarations ---
	R visitProgram(Program program);

	R visitNamespaceDeclaration(NamespaceDeclaration declaration);

	R visitClassDeclaration(ClassDeclaration declaration);

	R visitMethodDeclaration(MethodDeclaration declaration);

	R visitConstructorDeclaration(ConstructorDeclaration declaration);

	R visitFieldDeclaration(FieldDeclaration declaration);

	// --- Statements ---
	R visitBlockStatement(BlockStatement statement);

	R visitExpressionStatement(ExpressionStatement statement);

	R visitIfStatement(IfStatement statement);

	R visitWhileStatement(WhileStatement statement);

	R visitForStatement(ForStatement statement);

	R visitReturnStatement(ReturnStatement statement);

	R visitVariableDeclarationStatement(VariableDeclarationStatement statement);

	R visitSwitchStatement(SwitchStatement statement); // Added for SwitchStatement

	R visitArrayCreationExpression(ArrayCreationExpression expression);

	R visitArrayInitializerExpression(ArrayInitializerExpression expression);

	R visitSwitchCase(SwitchCase switchCase);         // Added for SwitchCase

	R visitConstructorChainingCallStatement(ConstructorChainingCallStatement statement); // NEW

	// --- Expressions ---
	R visitBinaryExpression(BinaryExpression expression);

	R visitUnaryExpression(UnaryExpression expression);

	R visitLiteralExpression(LiteralExpression expression);

	R visitIdentifierExpression(IdentifierExpression expression);

	R visitAssignmentExpression(AssignmentExpression expression);

	R visitCallExpression(CallExpression expression);

	R visitDotExpression(DotExpression expression);

	R visitThisExpression(ThisExpression expression);

	R visitNewExpression(NewExpression expression);

	R visitPostfixUnaryExpression(PostfixUnaryExpression expression);

	R visitArrayAccessExpression(ArrayAccessExpression expression);

	R visitGroupingExpression(GroupingExpression expression);

	R visitCastExpression(CastExpression expression);

	R visitTernaryExpression(TernaryExpression expression);

	R visitIsExpression(IsExpression expression);
}
