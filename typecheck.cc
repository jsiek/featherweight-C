#include "typecheck.h"
#include <vector>
#include <set>
#include <iostream>
using std::vector;
using std::set;
using std::cerr;
using std::endl;

template<class T>
bool list_equal(list<T*>* ts1, list<T*>* ts2, bool(*eq)(T*,T*)) {
  if (ts1->size() == ts2->size()) {
    auto iter2 = ts2->begin();
    for (auto iter1 = ts1->begin(); iter1 != ts1->end(); ++iter1, ++iter2) {
      if (! eq(*iter1, *iter2))
        return false;
    }
    return true;
  } else {
    return false;
  }
}

bool type_equal(Type* t1, Type* t2) {
  return (t1->tag == IntT && t2->tag == IntT)
    || (t1->tag == BoolT && t2->tag == BoolT)
    || (t1->tag == PtrT && t2->tag == PtrT
        && type_equal(t1->u.ptr.type, t2->u.ptr.type))
    || (t1->tag == FunT && t2->tag == FunT
        && type_equal(t1->u.fun.ret, t2->u.fun.ret)
        && list_equal(t1->u.fun.params, t2->u.fun.params, type_equal));
}

void expect_type(string context, Type* expected, Type* actual) {
  if (! type_equal(expected, actual)) {
    cerr << "in " << context << ", expected type ";
    print_type(expected);
    cerr << " but got type ";
    print_type(actual);
    cerr << endl;
    exit(-1);
  }
}

void print_error_string(string s) {
  cerr << s;
}

Type* typecheck_exp(Exp* e, TypeEnv* env) {
  switch (e->tag) {
  case Var:
    return lookup(env, *(e->u.var), print_error_string);
  case Deref: {
    Type* t = typecheck_exp(e->u.deref, env);
    switch (t->tag) {
    case PtrT:
      return t->u.ptr.type;
    default:
      cerr << "type error, expected a pointer in dereference" << endl;
      exit(-1);
      break;
    }
    break;
  }
  case Int:
    return make_int_type(e->lineno);
    break;
  case Bool:
    return make_bool_type(e->lineno);
    break;
  case AddrOf:
    return make_ptr_type(e->lineno, typecheck_exp(e->u.addr_of, env));
  case PrimOp: {
    vector<Type*> ts;
    for (auto iter = e->u.prim_op.args->begin();
         iter != e->u.prim_op.args->end(); ++iter) {
      ts.push_back(typecheck_exp(*iter, env));
    }
    switch (e->u.prim_op.op) {
    case Neg:
      expect_type("negation", make_int_type(e->lineno), ts[0]);
      return make_int_type(e->lineno);
    case Add:
    case Sub:
      expect_type("subtraction(1)", make_int_type(e->lineno), ts[0]);
      expect_type("substration(2)", make_int_type(e->lineno), ts[1]);
      return make_int_type(e->lineno);
    case And:
      expect_type("&&(1)", make_bool_type(e->lineno), ts[0]);
      expect_type("&&(2)", make_bool_type(e->lineno), ts[1]);
      return make_bool_type(e->lineno);
    case Or:
      expect_type("||(1)", make_bool_type(e->lineno), ts[0]);
      expect_type("||(2)", make_bool_type(e->lineno), ts[1]);
      return make_bool_type(e->lineno);
    case Not:
      expect_type("!", make_bool_type(e->lineno), ts[0]);
      return make_bool_type(e->lineno);
    case Eq:
      expect_type("==(1)", make_int_type(e->lineno), ts[0]);
      expect_type("==(2)", make_int_type(e->lineno), ts[1]);
      return make_bool_type(e->lineno);
    }
    break;
  }
  case Call: {
    Type* funT = typecheck_exp(e->u.call.fun, env);
    if (funT->tag != FunT) {
      cerr << "error, expected a function in function call" << endl;
      exit(-1);
    }
    if (e->u.call.args->size() != funT->u.fun.params->size()) {
      cerr << "error, wrong number of arguments in function call" << endl;
      exit(-1);
    }
    auto param_iter = funT->u.fun.params->begin();
    for (auto arg_iter = e->u.call.args->begin();
         arg_iter != e->u.call.args->end(); ++arg_iter, ++param_iter) {
      expect_type("call", *param_iter, typecheck_exp(*arg_iter, env));
    }
    return funT->u.fun.ret;
    break;
  }
  case Malloc:
    return make_ptr_type(e->lineno, e->u.malloc);
  }
}

void typecheck_stmt(Stmt* s, TypeEnv* env, Type* ret_type,
                    set<string>& labels) {
  switch (s->tag) {
  case Seq: {
    typecheck_stmt(s->u.seq.stmt, env, ret_type, labels);
    typecheck_stmt(s->u.seq.next, env, ret_type, labels);
    break;
  }
  case Assign: {
    Type* lhsT = typecheck_exp(s->u.assign.lhs, env);
    Type* rhsT = typecheck_exp(s->u.assign.rhs, env);
    expect_type("assign", lhsT, rhsT);
    break;
  }
  case ExpStmt: {
    typecheck_exp(s->u.exp, env);
    break;
  }
  case Free: {
    switch (typecheck_exp(s->u.free, env)->tag) {
    case PtrT:
      break;
    default:
      printf("error, free expects a pointer\n");
      exit(-1);
    }
    break;
  }
  case If:
    expect_type("condition of `if`", make_bool_type(s->lineno),
                typecheck_exp(s->u.if_stmt.cond, env));
    typecheck_stmt(s->u.if_stmt.thn, env, ret_type, labels);
    typecheck_stmt(s->u.if_stmt.els, env, ret_type, labels);
    break;
  case Goto:
    // to do: check the label
    break;
  case Label:
    if (labels.count(*s->u.labeled.label) > 0) {
      printf("error, duplicate label %s\n", s->u.labeled.label->c_str());
      exit(-1);
    }
    labels.insert(*s->u.labeled.label);
    typecheck_stmt(s->u.labeled.stmt, env, ret_type, labels);
    break;
  case Return:
    expect_type("return", ret_type, typecheck_exp(s->u.ret, env));
    break;
  }
}

void typecheck_fun_def(FunDef* f, TypeEnv* env) {
  for (auto i = f->params->begin(); i != f->params->end(); ++i) {
    env = new TypeEnv(i->first, i->second, env);
  }
  for (auto i = f->locals->begin(); i != f->locals->end(); ++i) {
    env = new TypeEnv(i->first, i->second, env);
  }
  if (f->name == "main") {
    expect_type("return type of `main`",
                make_int_type(f->lineno), f->return_type);
    if (f->params->size() != 0) {
      printf("error, main function may not have any parameters\n");
      exit(-1);
    }
  }
  set<string> labels;
  typecheck_stmt(f->body, env, f->return_type, labels);
}

TypeEnv* top_level(list<FunDef*>* fs) {
  TypeEnv* top = 0;
  bool found_main = false;
  for (auto i = fs->begin(); i != fs->end(); ++i) {
    list<Type*>* ps = new list<Type*>();
    for (auto pt = (*i)->params->begin(); pt != (*i)->params->end(); ++pt) {
      ps->push_back(pt->second);
    }
    top = new TypeEnv((*i)->name,
                      make_fun_type((*i)->lineno, ps, (*i)->return_type),
                      top);
    if ((*i)->name == "main") {
      found_main = true;
    }
  }
  if (found_main == false)
    cerr << "error, program must contain a function named `main`" << endl;
  return top;
}
