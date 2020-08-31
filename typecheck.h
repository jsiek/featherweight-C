#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "ast.h"
#include "assoc_list.h"

typedef AList<string, Type*> TypeEnv;

Type* typecheck_exp(Exp*, TypeEnv*);
void typecheck_stmt(Stmt*, TypeEnv*, Type*);
void typecheck_fun_def(FunDef*, TypeEnv*);
TypeEnv* top_level(list<FunDef*>* fs);
void print_error_string(string s);

#endif 
