#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "ast.h"

struct TypeEnv {
  string var;
  Type* type;
  TypeEnv* next;
  TypeEnv(string v, Type* t, TypeEnv* n) : var(v), type(t), next(n) { }
};

Type* typecheck_exp(Exp*, TypeEnv*);
void typecheck_stmt(Stmt*, TypeEnv*, Type*);
void typecheck_fun_def(FunDef*, TypeEnv*);
TypeEnv* top_level(list<FunDef*>* fs);

#endif 
