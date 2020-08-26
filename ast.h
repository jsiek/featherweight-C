#ifndef AST_H
#define AST_H

#include <stdlib.h>

/***** Forward Declarations *****/

struct TypeS;
typedef struct TypeS Type;

struct TypeEnvS;
typedef struct TypeEnvS TypeEnv;

struct LValueS;
typedef struct LValueS LValue;

struct ExpS;
typedef struct ExpS Exp;

struct StmtS;
typedef struct StmtS Stmt;

struct FunDefS;
typedef struct FunDefS FunDef;

/***** Lists *****/

struct TypeListS {
  struct TypeS* type;
  struct TypeListS* next;
};
typedef struct TypeListS TypeList;

TypeList* insert_type(Type* t, TypeList* next);
int len_type_list(TypeList* l);
Type* nth_type_list(TypeList* l, int i);


struct ExpListS {
  struct ExpS* exp;
  struct ExpListS* next;
};
typedef struct ExpListS ExpList;

ExpList* insert_exp(Exp*, ExpList*);
int len_exp_list(ExpList* l);
Exp* nth_exp_list(ExpList* l, int i);


struct StmtListS {
  struct StmtS* exp;
  struct StmtListS* next;
};
typedef struct StmtListS StmtList;

StmtList* insert_stmt(Stmt*, StmtList*);
int len_stmt_list(StmtList* l);
Stmt* nth_stmt_list(StmtList* l, int i);


struct FunDefListS {
  struct FunDefS* exp;
  struct FunDefListS* next;
};
typedef struct FunDefListS FunDefList;

FunDefList* insert_fun_def(FunDef*, FunDefList*);
int len_fun_def_list(FunDefList* l);
FunDef* nth_fun_def_list(FunDefList* l, int i);
void print_fun_def_list(FunDefList*);

/***** Types *****/

enum TypeKind {  IntT, FunT, PtrT };

struct TypeS {
  int lineno;
  enum TypeKind tag;
  struct TypeS* source; // Remember the original source type for printing errors
  union {
    char* var;
    struct { TypeList* params; struct TypeS* ret; } fun;
    struct { struct TypeS* type; } ptr;
  } u;
};

struct TypeEnvS {
  char* var;
  Type* type;
  struct TypeEnvS* rest;
};

Type* make_int_type(int lineno);
Type* make_fun_type(int lineno, TypeList* params, Type* ret);
Type* make_ptr_type(int lineno, Type* type);
TypeEnv* make_type_binding(char* field, Type* type, TypeEnv* rest);

void print_type(Type*);
void print_type_list(TypeList*);
void print_type_env(TypeEnv* env);

/***** L-Values *****/

enum LValueKind { Var, Deref };

struct LValueS {
  int lineno;
  enum LValueKind tag;
  union {
    char* var;
    Exp* deref;
  } u;
};

LValue* make_var(int lineno, char* var);
LValue* make_deref(int lineno, Exp* exp);

/***** Expressions *****/

enum ExpKind { LValueExp, Int, AddrOf, PrimOp };
enum Operator { Neg, Add, Sub, Not, And, Or, Eq };

struct ExpS {
  int lineno;
  enum ExpKind tag;
  union {
    LValue* lvalue;
    int integer;
    LValue* addr_of;
    struct { enum Operator op; ExpList* args; } prim_op;
  } u;
};

Exp* make_lval_exp(int lineno, LValue* lval);
Exp* make_int(int lineno, int i);
Exp* make_addr_of(int lineno, LValue* lval);
Exp* make_op(int lineno, enum Operator op, ExpList* args);

void print_exp(Exp*);

/***** Statements *****/

enum StmtKind { Assign, Free, Call, IfGoto, Label, Return };

struct StmtS {
  int lineno;
  enum StmtKind tag;
  union {
    struct { LValue* lhs; Exp* rhs; } assign;
    Exp* free;
    struct { LValue* lhs; Exp* fun; ExpList* args; } call;
    struct { Exp* cond; char* target; } if_goto;
    struct { char* label; Stmt* stmt; } labeled;
    Exp* ret;
  } u;
};

Stmt* make_assign(int lineno, LValue* lhs, LValue* rhs);
Stmt* make_call(int lineno, LValue* lhs, Exp* fun, ExpList* args);
Stmt* make_free(int lineno, Exp* e);
Stmt* make_if_goto(int lineno, Exp* cond, char* target);
Stmt* make_labeled(int lineno, char* label, Stmt* stmt);
Stmt* make_return(int lineno, Exp* e);

void print_stmt(Stmt*);

/***** Declarations *****/

struct FunDefS {
  char* name;
  Type* return_type;
  TypeEnv* params;
  TypeEnv* locals;
  Stmt* body;
};

FunDef* make_fun_def(int lineno, char* name, Type* ret_type, TypeEnv* params,
                      TypeEnv* locals, Stmt* body);
void print_fun_def(FunDef*);



#endif
