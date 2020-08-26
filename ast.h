#ifndef AST_H
#define AST_H

#include <stdlib.h>
#include <string>
#include <list>

using std::string;
using std::list;
using std::pair;

/***** Forward Declarations *****/

struct Type;
struct LValue;
struct Exp;
struct Stmt;
struct FunDef;

typedef list< pair<string, Type*> > VarTypes;

/***** Types *****/

enum TypeKind {  IntT, FunT, PtrT };

struct Type {
  int lineno;
  TypeKind tag;
  union {
    struct { list<Type*>* params; Type* ret; } fun;
    struct { Type* type; } ptr;
  } u;
};

Type* make_int_type(int lineno);
Type* make_fun_type(int lineno, list<Type*>* params, Type* ret);
Type* make_ptr_type(int lineno, Type* type);

void print_type(Type*);
void print_type_list(list<Type*>*);

/***** L-Values *****/

enum LValueKind { Var, Deref };

struct LValue {
  int lineno;
  LValueKind tag;
  union {
    string* var;
    Exp* deref;
  } u;
};

LValue* make_var(int lineno, string var);
LValue* make_deref(int lineno, Exp* exp);
void print_lvalue(LValue* lv);

/***** Expressions *****/

enum ExpKind { LValueExp, Int, AddrOf, PrimOp };
enum Operator { Neg, Add, Sub, Not, And, Or, Eq };

struct Exp {
  int lineno;
  ExpKind tag;
  union {
    LValue* lvalue;
    int integer;
    LValue* addr_of;
    struct { Operator op; list<Exp*>* args; } prim_op;
  } u;
};

Exp* make_lval_exp(int lineno, LValue* lval);
Exp* make_int(int lineno, int i);
Exp* make_addr_of(int lineno, LValue* lval);
Exp* make_op(int lineno, Operator op, list<Exp*>* args);

void print_exp(Exp*);
void print_exp_list(list<Exp*>* ts);

/***** Statements *****/

enum StmtKind { Assign, Call, Free, IfGoto, Label, Return };

struct Stmt {
  int lineno;
  StmtKind tag;
  union {
    struct { LValue* lhs; Exp* rhs; } assign;
    struct { LValue* lhs; Exp* fun; list<Exp*>* args; } call;
    Exp* free;
    struct { Exp* cond; string* target; } if_goto;
    struct { string* label; Stmt* stmt; } labeled;
    Exp* ret;
  } u;
};

Stmt* make_assign(int lineno, LValue* lhs, Exp* rhs);
Stmt* make_call(int lineno, LValue* lhs, Exp* fun, list<Exp*>* args);
Stmt* make_free(int lineno, Exp* e);
Stmt* make_if_goto(int lineno, Exp* cond, string target);
Stmt* make_labeled(int lineno, string label, Stmt* stmt);
Stmt* make_return(int lineno, Exp* e);

void print_stmt(Stmt*);

/***** Declarations *****/

struct FunDef {
  int lineno;
  string name;
  Type* return_type;
  VarTypes* params;
  VarTypes* locals;
  Stmt* body;
};

FunDef* make_fun_def(int lineno, string name, Type* ret_type, VarTypes* params,
                      VarTypes* locals, Stmt* body);
void print_fun_def(FunDef*);



#endif
