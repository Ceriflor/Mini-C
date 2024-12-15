%option yylineno
%{
#include "string.h"
#include "parser.tab.h"

int ErrorCharNum=0;
int yycolumn=1;

#define YY_USER_ACTION    \
    yylloc.first_line=yylloc.last_line=yylineno; \
    yylloc.first_column=yycolumn; \
    yylloc.last_column=yycolumn+yyleng-1; \
    yycolumn+=yyleng;

typedef struct {
    int type_int;
    float type_float;
    char type_id[32];
} YYLVAL;
#define YYSTYPE YYLVAL

%}

/* 词法单元定义 */
id   [A-Za-z_][A-Za-z0-9_]*
intconst    [1-9][0-9]*|0
floatconst    [0-9]+\.[0-9]+([eE][-+]?[0-9]+)?
Oneline_comment		"//"[^\n]*
Multiline_comment	"/*"([^\*]|(\*)*[^\*/])*(\*)*"*/"

%%
"char"       { strcpy(yylval.type_id, yytext); return CHAR; }
"int"        { strcpy(yylval.type_id, yytext); return INT; }
"float"      { yylval.type_float = atof(yytext); return FLOAT; }

"if"         { return IF; }
"else"       { return ELSE; }
"while"      { return WHILE; }
"break"      { return BREAK; }
"continue"   { return CONTINUE; }
"for"        { return FOR; }

{id}         { strcpy(yylval.type_id, yytext); return ID; }
[0-9]+       { yylval.type_int = atoi(yytext); return INT_CONST; }
{floatconst} { yylval.type_float = atof(yytext); return FLOAT_CONST; }
"+"          { return PLUS; }
"-"          { return MINUS; }
"*"          { return STAR; }
"/"          { return DIV; }
"%"          { return MOD; }
"++"         { return INC; }
"--"         { return DEC; }
"=="         { return EQ; }
"!="         { return NEQ; }
">"          { return GT; }
"<"          { return LT; }
">="         { return GE; }
"<="         { return LE; }
"="          { return ASSIGN; }
"&&"         { return AND; }
"||"         { return OR; }
"!"          { return NOT; }
";"          { return SEMICOLON; }
":"          { return COLON; }
","          { return COMMA; }
"("          { return LPAREN; }
")"          { return RPAREN; }
"{"          { return LBRACE; }
"}"          { return RBRACE; }
"["          { return LBRACKET; }
"]"          { return RBRACKET; }

{Oneline_comment}        {}
{Multiline_comment}       {}

[ \t\r]+                 { /* ignore whitespace */ }
\n                      { yycolumn = 1; }

.                       { printf("在第 %d 行出现错误的符号 '%s' \n", yylineno, yytext); ErrorCharNum++; }

%%

int yywrap()
{
    return 1;
}