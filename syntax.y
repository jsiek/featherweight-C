%{
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

extern FILE* yyin;
extern int yylineno;
char* input_filename;

void yyerror(char*s)  {
  fprintf(stderr, "%s:%d: %s\n", input_filename, yylineno, s);
  exit(-1);
}
// void yyerror(char *s, ...);

extern int yylex();
extern int yywrap();

#include "typecheck.h"
#include "eval.h"

static FunDefList* program;
%}
%union
 {
   char* str;
   int  num;
   Type* type;
   TypeList* type_list;
   Exp* expr;
   ExpList* expr_list;
   TypeEnv* var_decls;
   TypeEnv* params;
   Stmt* stmt;
   StmtList* stmt_list;
   FunDef* fun_def;
   FunDefList* fun_def_list;
};

%token <num> INT
%token <str> ID
%type <fun_def> fun_def
%type <fun_def_list> fun_def_list
%type <stmt> stmt
%type <stmt_list> stmt_list
%type <exp> expr
%type <exp_list> expr_list
%type <exp> simple_expr
%type <type> type
%type <type> simple_type
%type <type_list> type_list
%type <var_decls> var_decls
%type <params> params
%token NOT
%token AND
%token OR
%token INTTY
%token FUNTY
%token COLON
%token SEMICOLON
%token COMMA
%token LP
%token RP
%token LC
%token RC
%token PLUS
%token ASTR
%token MINUS
%token DIV
%token EQUAL
%token BANG
%token IF
%token GOTO
%token FREE
%token RETURN
%left BAR
%nonassoc IF 
%nonassoc LP RP
%nonassoc LS RS LT GT 
%nonassoc ASSGN ASTR
%nonassoc EQUAL
%left AND OR
%left PLUS MINUS
%nonassoc NOT
%start input
%locations
%%
input:
  fun_def_list {
  /*
  Type* t = typecheck($1, 0, 0);
  Value* v = eval($1, 0, 0); 
  if (v == 0) {
    printf("error during evaluation\n");
  } else {
    print_value(v);
    printf(" : ");
    print_type(t);
    printf("\n");
  }
  */
 }
;
params:
  /* empty */ { $$ = 0; }
| type ID  { $$ = make_type_binding($2, $1, 0); }
| type ID COMMA params { $$ = make_type_binding($2, $1, $4); }
;
var_decls:
  /* empty */ { $$ = 0; }
| type ID SEMICOLON var_decls { $$ = make_type_binding($2, $1, $4); }
;
type_list:
  /* empty */ { $$ = 0; }
| type { $$ = insert_type($1, 0); }
| type COMMA type_list { $$ = insert_type($1, $3); }
;
simple_type:
  INTTY             { $$ = make_int_type(yylineno); }
| LP type RP        { $$ = $2; }
;
type:
  simple_type                { $$ = $1; }
| FUNTY LP type_list RP type { $$ = make_fun_type(yylineno, $3, $5); }
| simple_type ASTR           { $$ = make_ptr_type(yylineno, $1); }
;
expr_list:
  /* empty */          { $$ = 0; }
| expr                 { $$ = insert_term($1, 0); }
| expr COMMA expr_list { $$ = insert_term($1, $3); }
;
simple_expr:
  INT              { $$ = make_int(yylineno, $1); }
| ID               { $$ = make_var(yylineno, $1); }
| LP expr RP       { $$ = $2; }
;
expr:
  simple_expr      { $$ = $1; }
| expr EQUAL expr  { $$ = make_binop(yylineno, Equal, $1, $3); }
| expr PLUS expr   { $$ = make_binop(yylineno, Add, $1, $3); }
| expr MINUS expr  { $$ = make_binop(yylineno, Sub, $1, $3); }
| expr AND expr    { $$ = make_binop(yylineno, And, $1, $3); }
| expr OR expr     { $$ = make_binop(yylineno, Or, $1, $3); }
| NOT expr         { $$ = make_uniop(yylineno, Not, $2); }
| MINUS expr       { $$ = make_uniop(yylineno, Neg, $2); }
;
stmt:
  ID ASSGN expr SEMICOLON
    { $$ = make_assign(yylineno, $1, $3); }
| ID ASSGN expr LP expr_list RP SEMICOLON
    { $$ = make_call(yylineno, $1, $3, $5); }
| FREE LP expr RP SEMICOLON
    { $$ = make_free(yylineno, $3); }
| IF LP expr RP GOTO ID SEMICOLON
    { $$ = make_if_goto(yylineno, $3, $6); }
| ID COLON stmt 
    { $$ = make_labeled(yylineno, $1, $3); }
| RETURN expr SEMICOLON
    { $$ = make_return(yylineno, $2); }
;
stmt_list:
  /* empty */    { $$ = 0; }
| stmt stmt_list { insert_stmt($1, $2); }
;
fun_def:
  type ID LP params RP LC var_decls stmt_list RC
    { $$ = make_fun_def(yylineno, $2, $1, $4, $7, $8); }
;
fun_def_list:
  /* empty */ { $$ = 0; }
| fun_def fun_def_list { insert_fun_def($1, $2); }
;
%%
int main(int argc, char* argv[])  { 
  /*yydebug = 1;*/

  if (argc > 1) {
    input_filename = argv[1];
    yyin = fopen(argv[1], "r");
  }
  if (argc > 2) {
    FILE* program = fopen(argv[2], "r");
    input = read_file(program);
  }
  yyparse(); 
  return 0;
}

/*
void yyerror(char *s, ...)
{
  va_list ap;
  va_start(ap, s);

  if(yylloc.first_line) {
    fprintf(stderr, "%d.%d-%d.%d: error: ", 
	    yylloc.first_line, yylloc.first_column,
	    yylloc.last_line, yylloc.last_column);
  }
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}

void lyyerror(YYLTYPE t, char *s, ...)
{
  va_list ap;
  va_start(ap, s);

  if(t.first_line) {
    fprintf(stderr, "%d.%d-%d.%d: error: ", 
	    t.first_line, t.first_column,
	    t.last_line, t.last_column);
  }
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");
}
*/
