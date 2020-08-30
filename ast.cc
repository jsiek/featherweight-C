#include <stdio.h>
#include <string>
#include <iostream>
#include "ast.h"

using std::cout;
using std::endl;

/***** Utilities *****/

char* input;

/***** Types *****/

Type* make_int_type(int lineno) {
  Type* t = new Type();
  t->tag = IntT;
  t->lineno = lineno;
  return t;
}

Type* make_bool_type(int lineno) {
  Type* t = new Type();
  t->tag = BoolT;
  t->lineno = lineno;
  return t;
}

Type* make_fun_type(int lineno, list<Type*>* params, Type* ret) {
  Type* t = new Type();
  t->tag = FunT;
  t->lineno = lineno;
  t->u.fun.params = params;
  t->u.fun.ret = ret;
  return t;
}

Type* make_ptr_type(int lineno, Type* type) {
  Type* t = new Type();
  t->tag = PtrT;
  t->lineno = lineno;
  t->u.ptr.type = type;
  return t;
}

void print_type(Type* t) {
  switch (t->tag) {
  case BoolT:
    printf("bool");
    break;
  case IntT:
    printf("int");
    break;
  case PtrT:
    print_type(t->u.ptr.type);
    printf("*");
  case FunT:
    printf("fun ");
    printf("(");
    print_list(t->u.fun.params, print_type, ", ");
    printf(") ");
    print_type(t->u.fun.ret);
  }
}

/***** Expressions *****/

Exp* make_var(int lineno, string var) {
  Exp* lv = new Exp();
  lv->lineno = lineno;
  lv->tag = Var;
  lv->u.var = new string(var);
  return lv;
}

Exp* make_deref(int lineno, Exp* exp) {
  Exp* lv = new Exp();
  lv->lineno = lineno;
  lv->tag = Deref;
  lv->u.deref = exp;
  return lv;
}

Exp* make_int(int lineno, int i) {
  Exp* e = new Exp();
  e->lineno = lineno;
  e->tag = Int;
  e->u.integer = i;
  return e;
}

Exp* make_addr_of(int lineno, Exp* lval) {
  Exp* e = new Exp();
  e->lineno = lineno;
  e->tag = AddrOf;
  e->u.addr_of = lval;
  return e;
}

Exp* make_op(int lineno, enum Operator op, list<Exp*>* args) {
  Exp* e = new Exp();
  e->lineno = lineno;
  e->tag = PrimOp;
  e->u.prim_op.op = op;
  e->u.prim_op.args = new vector<Exp*>(args->begin(), args->end());
  return e;
}

Exp* make_unop(int lineno, enum Operator op, Exp* arg) {
  Exp* e = new Exp();
  e->lineno = lineno;
  e->tag = PrimOp;
  e->u.prim_op.op = op;
  vector<Exp*>* args = new vector<Exp*>();
  args->push_back(arg);
  e->u.prim_op.args = args;
  return e;
}

Exp* make_binop(int lineno, enum Operator op, Exp* arg1, Exp* arg2) {
  Exp* e = new Exp();
  e->lineno = lineno;
  e->tag = PrimOp;
  e->u.prim_op.op = op;
  vector<Exp*>* args = new vector<Exp*>();
  args->push_back(arg1);
  args->push_back(arg2);
  e->u.prim_op.args = args;
  return e;
}

Exp* make_call(int lineno, Exp* fun, list<Exp*>* args) {
  Exp* s = new Exp();
  s->lineno = lineno;
  s->tag = Call;
  s->u.call.fun = fun;
  s->u.call.args = new vector<Exp*>(args->begin(), args->end());
  return s;
}

void print_op(Operator op) {
  switch (op) {
  case Neg:
    printf("-");
    break;
  case Add:
    printf("+");
    break;
  case Sub:
    printf("-");
    break;
  case Not:
    printf("!");
    break;
  case And:
    printf("&&");
    break;
  case Or:
    printf("||");
    break;
  case Eq:
    printf("==");
    break;
  }
}

void print_exp(Exp* e) {
  switch (e->tag) {
  case Int:
    printf("%d", e->u.integer);
    break;
  case AddrOf:
    printf("&");
    print_exp(e->u.addr_of);
    break;
  case PrimOp:
    if (e->u.prim_op.args->size() == 0) {
      print_op(e->u.prim_op.op);
    } else if (e->u.prim_op.args->size() == 1) {
      print_op(e->u.prim_op.op);
      auto iter = e->u.prim_op.args->begin();
      print_exp(*iter);
    } else if (e->u.prim_op.args->size() == 2) {
      auto iter = e->u.prim_op.args->begin();
      print_exp(*iter);
      print_op(e->u.prim_op.op);
      ++iter;
      print_exp(*iter);
    }
    break;
  case Var:
    printf("%s", e->u.var->c_str());
    break;
  case Deref:
    printf("*");
    print_exp(e->u.deref);
    break;
  case Call:
    print_exp(e->u.call.fun);
    printf("(");
    print_vector(e->u.call.args, print_exp, ", ");
    printf(")");
    break;
  }
}

/***** Statements *****/

Stmt* make_assign(int lineno, Exp* lhs, Exp* rhs) {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = Assign;
  s->u.assign.lhs = lhs;
  s->u.assign.rhs = rhs;
  return s;
}

Stmt* make_free(int lineno, Exp* e)  {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = Free;
  s->u.free = e;
  return s;
}

Stmt* make_if_goto(int lineno, Exp* cond, string target)  {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = IfGoto;
  s->u.if_goto.cond = cond;
  s->u.if_goto.target = new string(target);
  return s;
}

Stmt* make_labeled(int lineno, string label, Stmt* stmt)  {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = Label;
  s->u.labeled.label = new string(label);
  s->u.labeled.stmt = stmt;
  return s;
}

Stmt* make_return(int lineno, Exp* e) {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = Return;
  s->u.ret = e;
  return s;
}

Stmt* make_seq(int lineno, Stmt* s1, Stmt* s2) {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = Seq;
  s->u.seq.stmt = s1;
  s->u.seq.next = s2;
  return s;
}

void print_stmt(Stmt* s) {
  switch (s->tag) {
  case Assign:
    print_exp(s->u.assign.lhs);
    printf(" = ");
    print_exp(s->u.assign.rhs);
    printf(";");
    break;
  case Free:
    printf("free(");
    print_exp(s->u.free);
    printf(")");
    break;
  case IfGoto:
    printf("if (");
    print_exp(s->u.if_goto.cond);
    printf(") goto ");
    printf("%s;", s->u.if_goto.target->c_str());
    break;
  case Label:
    printf("%s:\n", s->u.labeled.label->c_str());
    print_stmt(s->u.labeled.stmt);
    break;
  case Return:
    printf("return ");
    print_exp(s->u.ret);
    printf(";");
    break;
  case Seq:
    printf("{\n");
    print_stmt(s->u.seq.stmt);
    cout << endl;
    print_stmt(s->u.seq.next);
    printf("\n}\n");
    break;
  }
}

/***** Declarations *****/

FunDef* make_fun_def(int lineno, string name, Type* ret_type, VarTypes* params,
                     VarTypes* locals, Stmt* body) {
  FunDef* f = new FunDef();
  f->lineno = lineno;
  f->name = name;
  f->return_type = ret_type;
  f->params = params;
  f->locals = locals;
  f->body = body;
  return f;
}

void print_params(VarTypes* ps) {
  int i = 0;
  for (auto iter = ps->begin(); iter != ps->end(); ++iter, ++i) {
    if (i != 0)
      printf(", ");
    print_type(iter->second);
    printf(" %s", iter->first.c_str());
  }
}

void print_var_decls(VarTypes* ps) {
  int i = 0;
  for (auto iter = ps->begin(); iter != ps->end(); ++iter, ++i) {
    print_type(iter->second);
    printf(" %s; ", iter->first.c_str());
  }
}

void print_fun_def(FunDef* f) {
  printf("fun %s", f->name.c_str());
  printf("(");
  print_params(f->params);
  printf(") ");
  print_type(f->return_type);
  printf(" {\n");
  if (f->locals->size() > 0) {
    printf("  ");
    print_var_decls(f->locals);
    printf("\n");
  }
  print_stmt(f->body);
  printf("\n}");
}

char *read_file(FILE* fp)
{
    char *fcontent = NULL;
    int fsize = 0;

    if(fp) {
        fseek(fp, 0, SEEK_END);
        fsize = ftell(fp);
        rewind(fp);

        fcontent = (char*) malloc(sizeof(char) * fsize);
        fread(fcontent, 1, fsize, fp);

        fclose(fp);
    }
    return fcontent;
}
