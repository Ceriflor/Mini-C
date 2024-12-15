#ifndef DEF_H
#define DEF_H

#include <string>
#include <vector>
#include <list>
#include <map>

// 数据类型枚举
enum DataType {
    T_CHAR,
    T_INT,
    T_FLOAT,
    T_VOID
};

// 符号种类枚举
enum SymbolKind {
    VAR, 
    FUNC, 
    PARAM, 
    ARRAY 
};

// 操作码枚举
enum OpCode {
    LABEL,
    FUNCTION,
    ASSIGN,
    PLUS,
    MINUS,
    STAR,
    DIV,
    UPLUS,
    UMINUS,
    NOT,
    DPLUS,
    DMINUS,
    PLUSD,
    MINUSD,
    GT,
    LT,
    GE,
    LE,
    EQ,
    NE,
    JGT,
    JGE,
    JLT,
    JLE,
    JEQ,
    JNE,
    RETURN,
    ARG,
    PARAM,
    CALL
};

// 操作数类
class Operand {
public:
    std::string name;
    DataType type;
    int offset;

    Operand(std::string n = "", DataType t = T_VOID, int o = 0) : name(n), type(t), offset(o) {}
};

// 中间代码类
class IRCode {
public:
    OpCode op;
    Operand left, right, result;

    IRCode(OpCode o, Operand l = Operand(), Operand r = Operand(), Operand res = Operand())
        : op(o), left(l), right(r), result(res) {}
};

// 符号基类
class Symbol {
public:
    std::string Name; 
    SymbolKind Kind; 
    DataType Type; 

    Symbol(std::string name, SymbolKind kind, DataType type) : Name(name), Kind(kind), Type(type) {}
    virtual ~Symbol() {}
};

class VarSymbol : public Symbol {
public:
    std::string Alias; 
    int Offset; 
    bool isGlobal; 
    std::vector<int> Dims; 

    VarSymbol(std::string name, DataType type, bool isGlobal = false) : Symbol(name, VAR, type), isGlobal(isGlobal) {}
};

class FuncSymbol : public Symbol {
public:
    int ParamNum; 
    int ARSize; 

    FuncSymbol(std::string name, DataType type) : Symbol(name, FUNC, type) {}
};

class SymbolsInAScope {
public:
    std::vector<Symbol*> Symbols; 
};

class SymbolStackDef {
public:
    std::vector<SymbolsInAScope*> scopes;

    Symbol* LocateNameCurrent(const std::string& Name);
    Symbol* LocateNameGlobal(const std::string& Name);
    void DisplaySymbolTable();
};

class FunctionCallTable {
public:
    std::vector<std::tuple<int, int, std::string>> FuncCalls; 

    void addFuncCall(int Line, int Column, const std::string& Name);
    void deleteFuncCall(const std::string& Name);
};

// AST节点基类
class ASTNode {
public:
    int Line, Column;
    ASTNode() : Line(0), Column(0) {}
    virtual ~ASTNode() {}
    virtual void GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) = 0;
};

class ProgAST : public ASTNode {
public:
    std::vector<ExtDefAST*> ExtDefs;
    void GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) override {
        for (auto& ext_def : ExtDefs) {
            ext_def->GenIR(ir_codes, temp_var_offset);
        }
    }
};

class ExtDefAST : public ASTNode {
public:
    TypeAST* Type;
    std::vector<VarDecAST*> ExtVars;
    void ExtDefAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    if (Type != nullptr) {
        Type->GenIR(ir_codes, temp_var_offset);
    }
    for (auto& var_dec : ExtVars) {
        var_dec->GenIR(ir_codes, temp_var_offset);
    }
}
};

class VarDecAST : public ASTNode {
public:
    std::string Name;
    std::vector<int> Dims;
    void VarDecAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    Operand result(name, DataType(Type), temp_var_offset);
    ir_codes.emplace_back(ASSIGN, Operand(), Operand(), result);
    temp_var_offset += sizeof(DataType); 
}
};

class TypeAST : public ASTNode {
public:
    DataType Type;
};

class ParamAST : public ASTNode {
public:
    TypeAST* Type;
    std::string ParamName;
};

class CompStmAST : public ASTNode {
public:
    std::vector<DefAST*> Decls;
    std::vector<StmAST*> Stms;
    void CompStmAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    for (auto& decl : Decls) {
        decl->GenIR(ir_codes, temp_var_offset);
    }
    for (auto& stm : Stms) {
        stm->GenIR(ir_codes, temp_var_offset);
    }
}
};

class StmAST : public ASTNode {
public:
    virtual ~StmAST() {}
    virtual void GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) = 0;
};

class ExpAST : public ASTNode {
public:
    virtual ~ExpAST() {}
    virtual void GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) = 0;
};

class ReturnStmAST : public StmAST {
public:
    ExpAST* Exp;
    void ReturnStmAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    if (Exp != nullptr) {
        Exp->GenIR(ir_codes, temp_var_offset);
        Operand result("_RETURN", Exp->Type, 0);
        Operand exp_result(NewTemp(), Exp->Type, temp_var_offset);
        ir_codes.emplace_back(ASSIGN, exp_result, Operand(), result);
    }
}
};

class IfStmAST : public StmAST {
public:
    ExpAST* Cond;
    StmAST* ThenStm;
    StmAST* ElseStm;
    void IfStmAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    Cond->GenIR(ir_codes, temp_var_offset);
    Operand cond_result(NewTemp(), T_INT, temp_var_offset);
    ir_codes.emplace_back(ASSIGN, cond_result, Operand(), Cond);
    std::string label_true = NewLabel();
    std::string label_false = NewLabel();
    ir_codes.emplace_back(JEQ, cond_result, Operand("_CONST", T_INT, 0), Operand(label_false, 0, 0));
    ThenStm->GenIR(ir_codes, temp_var_offset);
    ir_codes.emplace_back(GOTO, Operand(), Operand(), Operand(label_true, 0, 0));
    if (ElseStm != nullptr) {
        ElseStm->GenIR(ir_codes, temp_var_offset);
    }
    ir_codes.emplace_back(LABEL, Operand(label_true, 0, 0), Operand(), Operand());
}
};

class WhileStmAST : public StmAST {
public:
    ExpAST* Cond;
    StmAST* Body;
    void WhileStmAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    std::string label_start = NewLabel();
    std::string label_end = NewLabel();
    ir_codes.emplace_back(LABEL, Operand(label_start, 0, 0), Operand(), Operand());
    Cond->GenIR(ir_codes, temp_var_offset);
    Operand cond_result(NewTemp(), T_INT, temp_var_offset);
    ir_codes.emplace_back(ASSIGN, cond_result, Operand(), Cond);
    ir_codes.emplace_back(JEQ, cond_result, Operand("_CONST", T_INT, 0), Operand(label_end, 0, 0));
    Body->GenIR(ir_codes, temp_var_offset);
    ir_codes.emplace_back(GOTO, Operand(), Operand(), Operand(label_start, 0, 0));
    ir_codes.emplace_back(LABEL, Operand(label_end, 0, 0), Operand(), Operand());
}
};

class AssignAST : public ExpAST {
public:
    int Op;
    ExpAST* LeftValExp;
    ExpAST* RightValExp;
    void AssignAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    LeftValExp->GenIR(ir_codes, temp_var_offset);
    RightValExp->GenIR(ir_codes, temp_var_offset);
    Operand left_result(NewTemp(), LeftValExp->Type, temp_var_offset);
    Operand right_result(NewTemp(), RightValExp->Type, temp_var_offset + sizeof(DataType));
    ir_codes.emplace_back(ASSIGN, right_result, Operand(), left_result);
}
};

class BinaryExprAST : public ExpAST {
public:
    int Op;
    ExpAST* LeftExp;
    ExpAST* RightExp;
    void BinaryExprAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    LeftExp->GenIR(ir_codes, temp_var_offset);
    RightExp->GenIR(ir_codes, temp_var_offset);
    Operand left_result(NewTemp(), LeftExp->Type, temp_var_offset);
    Operand right_result(NewTemp(), RightExp->Type, temp_var_offset + sizeof(DataType));
    Operand result(NewTemp(), LeftExp->Type, temp_var_offset + 2 * sizeof(DataType));
    switch(Op) {
        case PLUS: ir_codes.emplace_back(PLUS, left_result, right_result, result); break;
        case MINUS: ir_codes.emplace_back(MINUS, left_result, right_result, result); break;
        case STAR: ir_codes.emplace_back(STAR, left_result, right_result, result); break;
        case DIV: ir_codes.emplace_back(DIV, left_result, right_result, result); break;
    }
}
};

class UnaryExprAST : public ExpAST {
public:
    int Op;
    ExpAST* Exp;
    void UnaryExprAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    Exp->GenIR(ir_codes, temp_var_offset);
    Operand exp_result(NewTemp(), Exp->Type, temp_var_offset);
    Operand result(NewTemp(), Exp->Type, temp_var_offset + sizeof(DataType));
    switch(Op) {
        case UPLUS: ir_codes.emplace_back(UPLUS, exp_result, Operand(), result); break;
        case UMINUS: ir_codes.emplace_back(UMINUS, exp_result, Operand(), result); break;
        case NOT: ir_codes.emplace_back(NOT, exp_result, Operand(), result); break;
    }
}
};

class VarAST : public ExpAST {
public:
    std::string Name;
};

class ConstAST : public ExpAST {
public:
    DataType Type;
    union {
        int constInt;
        float constFloat;
    } ConstVal;
    void ConstAST::GenIR(std::list<IRCode>& ir_codes, int& temp_var_offset) {
    Operand result("_CONST", Type, 0);
    if (Type == T_INT) {
        result.constInt = ConstVal.constInt;
    } else if (Type == T_FLOAT) {
        result.constFloat = ConstVal.constFloat;
    }
    ir_codes.emplace_back(ASSIGN, Operand(), result, result);
}
};

#endif 