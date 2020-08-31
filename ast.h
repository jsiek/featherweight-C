#ifndef AST_H
#define AST_H

#include <stdlib.h>
#include <string>
#include <list>
#include <vector>

using std::string;
using std::list;
using std::vector;
using std::pair;

/***** Utilities *****/

template<class T>
void print_list(list<T*>* ts, void(*printer)(T*), const char* sep) {
  int i = 0;
  for (auto iter = ts->begin(); iter != ts->end(); ++iter, ++i) {
    if (i != 0)
      printf("%s", sep);
    printer(*iter);
  }
}

template<class T>
void print_vector(vector<T*>* ts, void(*printer)(T*), const char* sep) {
  int i = 0;
  for (auto iter = ts->begin(); iter != ts->end(); ++iter, ++i) {
    if (i != 0)
      printf("%s", sep);
    printer(*iter);
  }
}

char *read_file(FILE* fp);

extern char* input;

/***** Forward Declarations *****/

struct Type;
struct LValue;
struct Exp;
struct Stmt;
struct FunDef;

typedef list< pair<string, Type*> > VarTypes;

/***** Types *****/

enum TypeKind {  IntT, FunT, PtrT, BoolT };

struct Type {
  int lineno;
  TypeKind tag;
  union {
    struct { list<Type*>* params; Type* ret; } fun;
    struct { Type* type; } ptr;
  } u;
};

Type* make_int_type(int lineno);
Type* make_bool_type(int lineno);
Type* make_fun_type(int lineno, list<Type*>* params, Type* ret);
Type* make_ptr_type(int lineno, Type* type);

void print_type(Type*);

/***** Expressions *****/

enum ExpKind { Var, Deref, Int, Bool, AddrOf, PrimOp, Call };
enum Operator { Neg, Add, Sub, Not, And, Or, Eq };

struct Exp {
  int lineno;
  ExpKind tag;
  union {
    string* var;
    Exp* deref;
    int integer;
    bool boolean;
    Exp* addr_of;
    struct { Operator op; vector<Exp*>* args; } prim_op;
    struct { Exp* fun; vector<Exp*>* args; } call;
  } u;
};

Exp* make_var(int lineno, string var);
Exp* make_deref(int lineno, Exp* exp);
Exp* make_int(int lineno, int i);
Exp* make_bool(int lineno, bool b);
Exp* make_addr_of(int lineno, Exp* lval);
Exp* make_op(int lineno, Operator op, list<Exp*>* args);
Exp* make_unop(int lineno, enum Operator op, Exp* arg);
Exp* make_binop(int lineno, enum Operator op, Exp* arg1, Exp* arg2);
Exp* make_call(int lineno, Exp* fun, list<Exp*>* args);
  
void print_exp(Exp*);

/***** Statements *****/

enum StmtKind { Assign, Free, IfGoto, Label, Return, Seq };

struct Stmt {
  int lineno;
  StmtKind tag;
  union {
    struct { Exp* lhs; Exp* rhs; } assign;
    Exp* free;
    struct { Exp* cond; string* target; } if_goto;
    struct { string* label; Stmt* stmt; } labeled;
    Exp* ret;
    struct { Stmt* stmt; Stmt* next; } seq;
  } u;
};

Stmt* make_assign(int lineno, Exp* lhs, Exp* rhs);
Stmt* make_free(int lineno, Exp* e);
Stmt* make_if_goto(int lineno, Exp* cond, string target);
Stmt* make_labeled(int lineno, string label, Stmt* stmt);
Stmt* make_return(int lineno, Exp* e);
Stmt* make_seq(int lineno, Stmt* s1, Stmt* s2);

void print_stmt(Stmt*, int);

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
void print_fun_def_depth(FunDef*, int);

#endif
