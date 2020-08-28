%{
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "ast.h"
#include "typecheck.h"
#include "interp.h"

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

//#include "typecheck.h"
//#include "eval.h"
#include <list>
#include "ast.h"
using std::list;
using std::pair;
using std::make_pair;
  
static list<FunDef*> program;
%}
%union
 {
   char* str;
   int  num;
   Type* type;
   list<Type*>* type_list;
   Exp* lvalue;
   Exp* expr;
   list<Exp*>* expr_list;
   VarTypes* var_decls;
   VarTypes* params;
   Stmt* stmt;
   Stmt* stmt_list;
   FunDef* fun_def;
   list<FunDef*>* fun_def_list;
};

%token <num> INT
%token <str> ID
%type <fun_def> fun_def
%type <fun_def_list> fun_def_list
%type <stmt> stmt
%type <stmt_list> stmt_list
%type <lvalue> lvalue
%type <expr> expr
%type <expr_list> expr_list
%type <type> type
%type <type_list> type_list
%type <var_decls> var_decls
%type <params> params
%token NOT
%token AND
%token OR
%token INTTY
%token FUN
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
    printf("program:\n");
    print_list($1, print_fun_def, "\n");
    TypeEnv* top = top_level($1);
    for (auto i = $1->begin(); i != $1->end(); ++i) {
      typecheck_fun_def(*i, top);
    }
    printf("\n");
    printf("type checking complete\n");
    //interp_program($1);
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
  /* empty */ { $$ = new VarTypes(); }
| type ID COMMA params { $$ = $4; $$->push_front(make_pair($2, $1)); }
;
var_decls:
  /* empty */ { $$ = new VarTypes(); }
| type ID SEMICOLON var_decls { $$ = $4; $$->push_front(make_pair($2, $1)); }
;
type_list:
  /* empty */ { $$ = new list<Type*>(); }
| type { $$ = new list<Type*>(); $$->push_front($1); }
| type COMMA type_list { $$ = $3; $$->push_front($1); }
;
type:
  INTTY             { $$ = make_int_type(yylineno); }
| FUN LP type_list RP type { $$ = make_fun_type(yylineno, $3, $5); }
| LP type RP          { $$ = $2; }
| type ASTR           { $$ = make_ptr_type(yylineno, $1); }
;
lvalue:
  ID               { $$ = make_var(yylineno, $1); }
| ASTR expr        { $$ = make_deref(yylineno, $2); }
;
expr:
  lvalue           { $$ = $1; }
| INT              { $$ = make_int(yylineno, $1); }
| expr EQUAL expr  { $$ = make_binop(yylineno, Eq, $1, $3); }
| expr PLUS expr   { $$ = make_binop(yylineno, Add, $1, $3); }
| expr MINUS expr  { $$ = make_binop(yylineno, Sub, $1, $3); }
| expr AND expr    { $$ = make_binop(yylineno, And, $1, $3); }
| expr OR expr     { $$ = make_binop(yylineno, Or, $1, $3); }
| NOT expr         { $$ = make_unop(yylineno, Not, $2); }
| MINUS expr       { $$ = make_unop(yylineno, Neg, $2); }
| LP expr RP       { $$ = $2; }
;
expr_list:
  /* empty */          { $$ = new list<Exp*>(); }
| expr                 { $$ = new list<Exp*>(); $$->push_front($1); }
| expr COMMA expr_list { $$ = $3; $$->push_front($1); }
;
stmt:
  lvalue ASSGN expr SEMICOLON
    { $$ = make_assign(yylineno, $1, $3); }
| lvalue ASSGN expr LP expr_list RP SEMICOLON
    { $$ = make_call(yylineno, $1, $3, $5); }
| FREE LP expr RP SEMICOLON
    { $$ = make_free(yylineno, $3); }
| IF LP expr RP GOTO ID SEMICOLON
    { $$ = make_if_goto(yylineno, $3, $6); }
| ID COLON stmt 
    { $$ = make_labeled(yylineno, $1, $3); }
| RETURN expr SEMICOLON
    { $$ = make_return(yylineno, $2); }
| LC stmt_list RC { $$ = $2; }
;
stmt_list :
  stmt { $$ = $1; }
| stmt stmt_list { $$ = make_seq(yylineno, $1, $2); }
;
fun_def:
  FUN ID LP params RP type var_decls stmt 
    { $$ = make_fun_def(yylineno, $2, $6, $4, $7, $8); }
;
fun_def_list:
  /* empty */ { $$ = new list<FunDef*>(); }
| fun_def fun_def_list { $$ = $2; $$->push_front($1); }
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
