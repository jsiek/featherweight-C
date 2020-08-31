#include <stdio.h>
#include <string>
#include <iostream>
#include <iomanip>
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
    cout << "bool";
    break;
  case IntT:
    cout << "int";
    break;
  case PtrT:
    print_type(t->u.ptr.type);
    cout << "*";
    break;
  case FunT:
    cout << "fun ";
    cout << "(";
    print_list(t->u.fun.params, print_type, ", ");
    cout << " ";
    print_type(t->u.fun.ret);
    break;
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

Exp* make_bool(int lineno, bool b) {
  Exp* e = new Exp();
  e->lineno = lineno;
  e->tag = Bool;
  e->u.boolean = b;
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
  Exp* e = new Exp();
  e->lineno = lineno;
  e->tag = Call;
  e->u.call.fun = fun;
  e->u.call.args = new vector<Exp*>(args->begin(), args->end());
  return e;
}

Exp* make_malloc(int lineno, Type* type) {
  Exp* e = new Exp();
  e->lineno = lineno;
  e->tag = Malloc;
  e->u.malloc = type;
  return e;
}

void print_op(Operator op) {
  switch (op) {
  case Neg:
    cout << "-";
    break;
  case Add:
    cout << "+";
    break;
  case Sub:
    cout << "-";
    break;
  case Not:
    cout << "!";
    break;
  case And:
    cout << "&&";
    break;
  case Or:
    cout << "||";
    break;
  case Eq:
    cout << "==";
    break;
  }
}

void print_exp(Exp* e) {
  switch (e->tag) {
  case Int:
    cout << e->u.integer;
    break;
  case Bool:
    cout << std::boolalpha;   
    cout << e->u.boolean;
    break;
  case AddrOf:
    cout << "&";
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
    cout << *(e->u.var);
    break;
  case Deref:
    cout << "*";
    print_exp(e->u.deref);
    break;
  case Call:
    print_exp(e->u.call.fun);
    cout << "(";
    print_vector(e->u.call.args, print_exp, ", ");
    cout << ")";
    break;
  case Malloc:
    cout << "malloc(";
    print_type(e->u.malloc);
    cout << ")";
    break;
  }
}

/***** Statements *****/

Stmt* make_exp_stmt(int lineno, Exp* exp) {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = ExpStmt;
  s->u.exp = exp;
  return s;
}

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

Stmt* make_if(int lineno, Exp* cond, Stmt* thn, Stmt* els)  {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = If;
  s->u.if_stmt.cond = cond;
  s->u.if_stmt.thn = thn;
  s->u.if_stmt.els = els;
  return s;
}

Stmt* make_goto(int lineno, string target)  {
  Stmt* s = new Stmt();
  s->lineno = lineno;
  s->tag = Goto;
  s->u.goto_stmt.target = new string(target);
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

void print_stmt(Stmt* s, int depth) {
  if (depth == 0) {
    cout << " ... ";
    return;
  }
  switch (s->tag) {
  case ExpStmt:
    print_exp(s->u.exp);
    cout << ";";
    break;
  case Assign:
    print_exp(s->u.assign.lhs);
    cout << " = ";
    print_exp(s->u.assign.rhs);
    cout << ";";
    break;
  case Free:
    cout << "free(";
    print_exp(s->u.free);
    cout << ")";
    break;
  case If:
    cout << "if (";
    print_exp(s->u.if_stmt.cond);
    cout << ")" << endl;
    print_stmt(s->u.if_stmt.thn, depth - 1);
    cout << endl << "else" << endl;
    print_stmt(s->u.if_stmt.els, depth - 1);
    break;
  case Goto:
    cout << "goto ";
    cout << (*s->u.goto_stmt.target) << ";";
    break;
  case Label:
    cout << *s->u.labeled.label << ":";
    if (depth != 0)
      cout << endl;
    print_stmt(s->u.labeled.stmt, depth - 1);
    break;
  case Return:
    cout << "return ";
    print_exp(s->u.ret);
    cout << ";";
    break;
  case Seq:
    print_stmt(s->u.seq.stmt, depth);
    if (depth != 0)    
      cout << endl;
    print_stmt(s->u.seq.next, depth - 1);
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
      cout << ", ";
    print_type(iter->second);
    cout << " " << iter->first;
  }
}

void print_var_decls(VarTypes* ps) {
  int i = 0;
  for (auto iter = ps->begin(); iter != ps->end(); ++iter, ++i) {
    print_type(iter->second);
    cout << " " << iter->first << "; ";
  }
}

void print_fun_def_depth(FunDef* f, int depth) {
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
  print_stmt(f->body, depth);
  printf("\n}\n");
}

void print_fun_def(FunDef* f) {
  print_fun_def_depth(f, -1);
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
