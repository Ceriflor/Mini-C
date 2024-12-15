#include "def.h"
#include <iostream>
#include <iomanip>

class Symbol {
public:
    std::string name;
    int type; 

    Symbol(std::string n, int t) : name(n), type(t) {}
    virtual ~Symbol() {}
};

class VarSymbol : public Symbol {
public:
    int offset;
    bool global;

    VarSymbol(std::string n, int t, int o, bool g) : Symbol(n, t), offset(o), global(g) {}
};

class FuncSymbol : public Symbol {
public:
    int paramCount;

    FuncSymbol(std::string n, int t, int p) : Symbol(n, t), paramCount(p) {}
};

Symbol* SymbolStackDef::LocateNameCurrent(const std::string& name) {
    for (auto sym : scopes.back()->symbols) {
        if (sym->name == name) return sym;
    }
    return nullptr;
}

Symbol* SymbolStackDef::LocateNameGlobal(const std::string& name) {
    for (auto scope : scopes) {
        for (auto sym : scope->symbols) {
            if (sym->name == name) return sym;
        }
    }
    return nullptr;
}

// 展示符号表
void SymbolStackDef::DisplaySymbolTable() {
    for (size_t i = 0; i < scopes.size(); ++i) {
        std::cout << "Scope " << i << ":\n";
        for (auto sym : scopes[i]->symbols) {
            std::cout << "Name: " << sym->name << ", Type: " << sym->type << ", ";
            if (dynamic_cast<VarSymbol*>(sym)) {
                auto varSym = dynamic_cast<VarSymbol*>(sym);
                std::cout << "Offset: " << varSym->offset << ", Global: " << std::boolalpha << varSym->global << "\n";
            } else if (dynamic_cast<FuncSymbol*>(sym)) {
                auto funcSym = dynamic_cast<FuncSymbol*>(sym);
                std::cout << "Param Count: " << funcSym->paramCount << "\n";
            }
        }
    }
}

void FunctionCallTable::AddFuncCall(int line, int column, const std::string& name) {
    calls.emplace_back(line, column, name);
}

void FunctionCallTable::RemoveFuncCall(const std::string& name) {
    calls.erase(std::remove_if(calls.begin(), calls.end(), [&name](const auto& call) {
        return std::get<2>(call) == name;
    }), calls.end());
}