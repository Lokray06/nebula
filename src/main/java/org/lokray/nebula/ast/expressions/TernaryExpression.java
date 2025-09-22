// File: src/main/java/com/juanpa/nebula/transpiler/ast/expressions/TernaryExpression.java
// File: src/main/java/com/juanpa/nebula/transpiler/ast/expressions/TernaryExpression.java
package org.lokray.nebula.ast.expressions;

import org.lokray.nebula.ast.ASTVisitor;
import org.lokray.nebula.lexer.Token;
import org.lokray.nebula.semantics.Symbol;
import org.lokray.nebula.semantics.Type;

public class TernaryExpression implements Expression {
    private final Expression condition;
    private final Expression thenBranch;
    private final Expression elseBranch;
    private Type resolvedType;

    public TernaryExpression(Expression condition, Expression thenBranch, Expression elseBranch) {
        this.condition = condition;
        this.thenBranch = thenBranch;
        this.elseBranch = elseBranch;
    }

    public Expression getCondition() {
        return condition;
    }

    public Expression getThenBranch() {
        return thenBranch;
    }

    public Expression getElseBranch() {
        return elseBranch;
    }

    @Override
    public <R> R accept(ASTVisitor<R> visitor) {
        return visitor.visitTernaryExpression(this);
    }

    @Override
    public Token getFirstToken() {
        return condition.getFirstToken();
    }

    @Override
    public Type getResolvedType() {
        return resolvedType;
    }

    @Override
    public void setResolvedType(Type type) {
        this.resolvedType = type;
    }

    @Override
    public Symbol getResolvedSymbol() {
        return null;
    }

    @Override
    public void setResolvedSymbol(Symbol symbol) {
        // Not applicable
    }
}