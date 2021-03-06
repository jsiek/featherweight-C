%{
#include <stdlib.h>
#include "ast.h"
#include "syntax.tab.h"
%}
%option yylineno

PLUS  "+"
ASTR  "*"
MINUS "-"
COLON   ":"
SEMICOLON   ";"
COMMA   ","
LP    "("
RP    ")"
LC    "{"
RC    "}"
INT   [0-9]+
ID    [A-Za-z_][A-Za-z0-9_]*
AND   "&&"
AMP   "&"
OR    "||"
BANG "!"
EQUAL "=="
ASSGN "="
COMMENT \/\/[^\n]*\n
RETURN "return"
GOTO   "goto"
IF     "if"
ELSE   "else"
MALLOC "malloc"
FREE   "free"
INTTY  "int"
BOOLTY "bool"
FUN    "fun"
TRUE   "true"
FALSE  "false"
%%
{AND}    { return AND; }
{AMP}    { return AMP; }
{OR}     { return OR; }
{BANG}   { return BANG; }
{IF}     { return IF; }
{ELSE}   { return ELSE; }
{INTTY}  { return INTTY; }
{BOOLTY} { return BOOLTY; }
{FUN}    { return FUN; }
{COLON}  { return COLON; }
{SEMICOLON} { return SEMICOLON; }
{COMMA}  { return COMMA; }
{PLUS}   { return PLUS; }
{ASTR}   { return ASTR; }
{MINUS}  { return MINUS; }
{EQUAL}  { return EQUAL; }
{ASSGN}  { return ASSGN; }
{LP}     { return LP; }
{RP}     { return RP; }
{LC}     { return LC; }
{RC}     { return RC; }
{RETURN} { return RETURN; }
{GOTO}   { return GOTO; }
{FREE}   { return FREE; }
{MALLOC} { return MALLOC; }
{TRUE}   { return TRUE; }
{FALSE}  { return FALSE; }
{ID}     {
           int n = strlen(yytext);
           yylval.str = (char*)malloc((n + 1) * sizeof(char));
           strncpy(yylval.str, yytext, n + 1);
           return ID;
         }
{INT}    {yylval.num = atof(yytext);  return INT;}
[ \t\n]+   ;
{COMMENT} ;
.     {return yytext[0];}
%%
int yywrap()  {return 1;}

