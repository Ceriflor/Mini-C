%define parse.error verbose
%locations
%{
#include "def.h"
extern int ErrorCharNum;
extern int yylineno;
extern char *yytext;
extern FILE *yyin;
void yyerror(const char* fmt, ...);
extern "C" int yylex();
#define SavePosition t->Line=yylloc.first_line;t->Column=yylloc.first_column

typedef struct YYLVAL {
    int                  type_int;
    float                type_float;
    char                 type_id[32];
    ProgAST                     *program;
    std::vector<ExtDefAST *>        ExtDefList;  
    ExtDefAST                   *ExtDef;
    std::vector<VarDecAST*>         ExtDecList; 
    TypeAST                     *Specifier;
    VarDecAST                   *VarDec;
    CompStmAST                  *CompSt;
    std::vector<ParamAST *>         ParamList; 
    ParamAST                    *ParamDec;

    std::vector<StmAST *>           StmList;
    StmAST                      *Stmt;
    std::vector<DefAST *>           DefList;
    DefAST                      *Def;
    std::vector<VarDecAST *>        DecList;
    VarDecAST                   *Dec;
    ExpAST                      *Exp;
    std::vector<ExpAST *>           Args; 
    CaseStmAST                  *Case;
    std::vector<CaseStmAST *>       CaseList;
} YYLVAL;
#define YYSTYPE YYLVAL

extern SymbolStackDef symbolStack;
extern FunctionCallTable functionCallTable;
extern Errors errors;

%}

%type <program>    program
%type <ExtDefList> ExtDefList
%type <ExtDef>     ExtDef
%type <ExtDecList> ExtDecList
%type <Specifier>  Specifier
%type <VarDec>     VarDec
%type <VarDec>     ParamVarDec
%type <CompSt>     CompSt
%type <ParamList>  ParamList
%type <ParamDec>   ParamDec
%type <DefList>    DefList
%type <StmList>    StmList
%type <Stmt>       Stmt
%type <Def>        Def
%type <DecList>    DecList
%type <Dec>        Dec
%type <Exp>        Exp
%type <Exp>        Sub
%type <Args>       SubList
%type <Case>       Case;
%type <CaseList>   CaseList

%type <Args>  Args

%token <type_int> INT                  
%token <type_id>  ID TYPE               
%token <type_float> FLOAT               

%token CHAR INT FLOAT IF ELSE WHILE BREAK CONTINUE FOR
%token PLUS MINUS STAR DIV MOD EQ NEQ GT LT GE LE
%token AND OR NOT SEMICOLON COLON COMMA
%token LP RP LB RB
%token INC DEC ASSIGN
%token LBRACE RBRACE LBRACKET RBRACKET

%left COMMA
%left ASSIGN
%left OR
%left AND
%left LT LE GT GE
%left NE EQ
%left PLUS MINUS
%left STAR DIV
%right NOT
%left INC DEC
%nonassoc LOWER_THEN_ELSE
%nonassoc ELSE

%%

program:
    ExtDefList
    {
        $$ = new ProgAST();
        $$->ExtDefs = $1;
        if (errors.IsEmpty() && ErrorCharNum == 0)
        {
            $$->DisplayAST(0); // 无词法、语法错误显示语法树
        }
        else
        {
            errors.ErrorsDisplay(); return 0;
        }
        $$->Semantics0(); 
        if (errors.IsEmpty())
            $$->GenIR(); 
        exit(0);
    }
    ;

ExtDefList:
    /* empty */
    {
        $$ = new std::vector<ExtDefAST*>();
    }
    | ExtDef ExtDefList
    {
        $2->insert($2->begin(), $1);
        $$ = $2;
    }
    ;

ExtDef:
    Specifier ExtDecList SEMICOLON
    {
        ExtVarDefAST *t = new ExtVarDefAST();
        t->Type = $1;
        t->ExtVars = $2;
        $$ = t;
        SavePosition;
    }
    | Specifier ID LP ParamList RP CompSt
    {
        FuncDefAST *t = new FuncDefAST();
        t->Type = $1;
        t->Name = $2;
        t->Params = $4;
        t->Body = $6;
        $$ = t;
        SavePosition;
    }
    | Specifier ID LP ParamList RP SEMICOLON
    {
        FuncDefAST *t = new FuncDefAST();
        t->Type = $1;
        t->Name = $2;
        t->Params = $4;
        $$ = t;
        SavePosition;
    }
    ;

Specifier:
    TYPE
    {
        BasicTypeAST *t = new BasicTypeAST();
        if (strcmp($1, "int") == 0) t->Type = T_INT;
        if (strcmp($1, "float") == 0) t->Type = T_FLOAT;
        if (strcmp($1, "void") == 0) t->Type = T_VOID;
        $$ = t;
        SavePosition;
    }
    ;

ExtDecList:
    VarDec
    {
        $$ = new std::vector<VarDecAST*>();
        $$->push_back($1);
    }
    | VarDec COMMA ExtDecList
    {
        $3->insert($3->begin(), $1);
        $$ = $3;
    }
    ;

VarDec:
    ID
    {
        VarDecAST *t = new VarDecAST();
        t->Name = string($1);
        $$ = t;
        SavePosition;
    }
    | VarDec LB INT_RB
    {
        $1->Dims.push_back($3);
        $$ = $1;
    }
    ;

ParamList:
    /* empty */
    {
        $$ = new std::vector<ParamAST*>();
    }
    | ParamDec
    {
        $$ = new std::vector<ParamAST*>();
        $$->push_back($1);
    }
    | ParamList COMMA ParamDec
    {
        $1->push_back($3);
        $$ = $1;
    }
    ;

ParamDec:
    Specifier ParamVarDec
    {
        ParamAST* t = new ParamAST();
        t->Type = $1;
        t->ParamName = $2;
        $$ = t;
        SavePosition;
    }
    ;

CompSt:
    LBRACE DefList StmList RBRACE
    {
        CompStmAST *t = new CompStmAST();
        t->Decls = $2;
        t->Stms = $3;
        $$ = t;
        SavePosition;
    }
    ;

StmList:
    /* empty */
    {
        $$ = new std::vector<StmAST*>();
    }
    | Stmt StmList
    {
        $$ = $2;
        $$->insert($$->begin(), $1);
    }
    ;

DefList:
    /* empty */
    {
        $$ = new std::vector<DefAST*>();
    }
    | Def DefList
    {
        $$ = $2;
        $$->insert($$->begin(), $1);
    }
    ;

Def:
    Specifier DecList SEMICOLON
    {
        DefAST *t = new DefAST();
        t->Type = $1;
        t->LocVars = $2;
        $$ = t;
        SavePosition;
    }
    ;

DecList:
    Dec
    {
        $$ = new std::vector<VarDecAST*>();
        $$->push_back($1);
    }
    | Dec COMMA DecList
    {
        $$ = $3;
        $$->insert($$->begin(), $1);
    }
    ;

Dec:
    VarDec
    {
        $$ = $1;
    }
    | VarDec ASSIGN Exp
    {
        $$ = $1;
        $$->Exp = $3;
    }
    ;

Stmt:
    Exp SEMICOLON
    {
        ExprStmAST *t = new ExprStmAST();
        t->Exp = $1;
        $$ = t;
        SavePosition;
    }
    | CompSt
    {
        $$ = $1;
    }
    | RETURN Exp SEMICOLON
    {
        ReturnStmAST *t = new ReturnStmAST();
        t->Exp = $2;
        $$ = t;
        SavePosition;
    }
    | RETURN SEMICOLON
    {
        ReturnStmAST *t = new ReturnStmAST();
        t->Exp = NULL;
= t;
        SavePosition;
    }
    | IF LP Exp RP Stmt %prec LOWER_THEN_ELSE
    {
        IfStmAST *t = new IfStmAST();
        t->Cond = $3;
        t->ThenStm = $5;
        = t;
SavePosition;
}
| IF LP Exp RP Stmt ELSE Stmt
{
IfElseStmAST *t = new IfElseStmAST();
t->Cond = $3;
t->ThenStm = $5;
t->ElseStm = $7;
= t;
        SavePosition;
    }
    | WHILE LP Exp RP Stmt
    {
        WhileStmAST *t = new WhileStmAST();
        t->Cond = $3;
        t->Body = $5;
        = t;
SavePosition;
}
| FOR LP Exp SEMICOLON Exp SEMICOLON Exp RP Stmt
{
ForStmAST *t = new ForStmAST();
t->SinExp = $3;
t->Cond = $5;
t->EndExp = $7;
t->Body = $9;
= t;
        SavePosition;
    }
    | BREAK SEMICOLON
    {
        BreakStmAST *t = new BreakStmAST();
        = t;
SavePosition;
}
| CONTINUE SEMICOLON
{
ContinueStmAST *t = new ContinueStmAST();
= t;
        SavePosition;
    }
    | error SEMICOLON
    {
        = NULL;
}
;

Exp:
Exp ASSIGN Exp { AssignAST *t = new AssignAST(); t->Op = ASSIGN; t->LeftValExp = $1; t->RightValExp = $3; = t; SavePosition; }
    | Exp PLUS Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = PLUS; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
| Exp MINUS Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = MINUS; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
    | Exp STAR Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = STAR; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
| Exp DIV Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = DIV; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
    | Exp MOD Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = MOD; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
| LP Exp RP { = $2; }
    | MINUS Exp %prec UMINUS { UnaryExprAST *t = new UnaryExprAST(); t->Op = UMINUS; t->Exp = $2; = t; SavePosition; }
| PLUS Exp %prec UPLUS { UnaryExprAST *t = new UnaryExprAST(); t->Op = UPLUS; t->Exp = $2; = t; SavePosition; }
    | Exp AND Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = AND; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
| Exp OR Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = OR; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
    | NOT Exp { UnaryExprAST *t = new UnaryExprAST(); t->Op = NOT; t->Exp = $2; = t; SavePosition; }
| Exp GT Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = GT; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
    | Exp GE Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = GE; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
| Exp LT Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = LT; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
    | Exp LE Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = LE; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
| Exp NE Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = NEQ; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
    | Exp EQ Exp { BinaryExprAST *t = new BinaryExprAST(); t->Op = EQ; t->LeftExp = $1; t->RightExp = $3; = t; SavePosition; }
| INC Exp { UnaryExprAST *t = new UnaryExprAST(); t->Op = INC; t->Exp = $2; = t; SavePosition; }
    | DEC Exp { UnaryExprAST *t = new UnaryExprAST(); t->Op = DEC; t->Exp = $2; = t; SavePosition; }
| Exp INC { UnaryExprAST *t = new UnaryExprAST(); t->Op = PLUSD; t->Exp = $1; = t; SavePosition; }
    | Exp DEC { UnaryExprAST *t = new UnaryExprAST(); t->Op = MINUSD; t->Exp = $1; = t; SavePosition; }
| ID LP Args RP %prec ARRPRO { FuncCallAST *t = new FuncCallAST(); t->Name = $1; t->Params = $3; = t; SavePosition; }
    | ID { VarAST *t = new VarAST(); t->Name = $1; = t; SavePosition; }
| INT { ConstAST *t = new ConstAST(); t->Type = T_INT; t->ConstVal.constINT = $1; = t; SavePosition; }
    | FLOAT_CONST { ConstAST *t = new ConstAST(); t->Type = T_FLOAT; t->ConstVal.constFLOAT = $1; = t; SavePosition; }
;

Args:   {}
       |  Exp {=std::vector <ExpAST *>();.push_back($1); }
       |  Args COMMA  Exp   {$$=$1;$$.push_back($3);}
       ;

%%

int main(int argc, char *argv[]){
yyin = fopen(argv[1], "r");
if (!yyin) return 0;
yylineno = 1;
yyparse();
return 0;
}

void yyerror(const char* fmt, ...)
{
va_list ap;
va_start(ap, fmt);
errors.ErrorAdd(yylloc.first_line, yylloc.first_column, fmt, ap);
va_end(ap);
}