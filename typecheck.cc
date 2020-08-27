#include "typecheck.h"
#include <vector>
using std::vector;

Type* lookup(TypeEnv* env, string var) {
  if (env == NULL)
    return NULL;
  else if (env->var == var) {
    return env->type;
  } else {
    return lookup(env->next, var);
  }
}

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

Type* typecheck_lvalue(LValue* lval, TypeEnv* env) {
    switch (lval->tag) {
    case Var:
      return lookup(env, *(lval->u.var));
    case Deref:
      Type* t = typecheck_exp(lval->u.deref, env);
      switch (t->tag) {
      case PtrT:
        return t->u.ptr.type;
      default:
        printf("type error, expected a pointer in dereference");
        exit(-1);
        break;
      }
      break;
    }

}

void expect_type(Type* expected, Type* actual) {
  if (! type_equal(expected, actual)) {
    printf("expected argument of type ");
    print_type(expected);
    printf(" but got argument of type ");
    print_type(actual);
    printf("\n");
    exit(-1);
  }
}

Type* typecheck_exp(Exp* e, TypeEnv* env) {
  switch (e->tag) {
  case LValueExp:
    return typecheck_lvalue(e->u.lvalue, env);
  case Int:
    return make_int_type(e->lineno);
    break;
  case AddrOf:
    return make_ptr_type(e->lineno, typecheck_lvalue(e->u.addr_of, env));
  case PrimOp:
    vector<Type*> ts;
    for (auto iter = e->u.prim_op.args->begin();
         iter != e->u.prim_op.args->end(); ++iter) {
      ts.push_back(typecheck_exp(*iter, env));
    }
    switch (e->u.prim_op.op) {
    case Neg:
      expect_type(make_int_type(e->lineno), ts[0]);
      return make_int_type(e->lineno);
    case Add:
    case Sub:
      expect_type(make_int_type(e->lineno), ts[0]);
      expect_type(make_int_type(e->lineno), ts[1]);
      return make_int_type(e->lineno);
    case And:
    case Or:
      expect_type(make_bool_type(e->lineno), ts[0]);
      expect_type(make_bool_type(e->lineno), ts[1]);
      return make_bool_type(e->lineno);
    case Not:
      expect_type(make_bool_type(e->lineno), ts[0]);
      return make_bool_type(e->lineno);
    case Eq:
      expect_type(make_int_type(e->lineno), ts[0]);
      expect_type(make_int_type(e->lineno), ts[1]);
      return make_bool_type(e->lineno);
    }
    break;
  }
}

void typecheck_stmt(Stmt* s, TypeEnv* env, Type* ret_type) {
  switch (s->tag) {
  case Assign: {
    Type* lhsT = typecheck_lvalue(s->u.assign.lhs, env);
    Type* rhsT = typecheck_exp(s->u.assign.rhs, env);
    expect_type(lhsT, rhsT);
    break;
  }
  case Call: {
    Type* lhsT = typecheck_lvalue(s->u.assign.lhs, env);
    Type* funT = typecheck_exp(s->u.call.fun, env);
    if (funT->tag != FunT) {
      printf("error, expected a function in function call");
      exit(-1);
    }
    if (s->u.call.args->size() != funT->u.fun.params->size()) {
      printf("error, wrong number of arguments in function call\n");
      exit(-1);
    }
    auto param_iter = funT->u.fun.params->begin();
    for (auto arg_iter = s->u.call.args->begin();
         arg_iter != s->u.call.args->end(); ++arg_iter, ++param_iter) {
      expect_type(*param_iter, typecheck_exp(*arg_iter, env));
    }
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
  case IfGoto:
    expect_type(make_bool_type(s->lineno),
                typecheck_exp(s->u.if_goto.cond, env));
    break;
  case Label:
    typecheck_stmt(s->u.labeled.stmt, env, ret_type);
    break;
  case Return:
    expect_type(ret_type, typecheck_exp(s->u.ret, env));
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
  for (auto i = f->body->begin(); i != f->body->end(); ++i) {
    typecheck_stmt(*i, env, f->return_type);
  }
}

TypeEnv* top_level(list<FunDef*>* fs) {
  TypeEnv* top = 0;
  for (auto i = fs->begin(); i != fs->end(); ++i) {
    list<Type*>* ps = new list<Type*>();
    for (auto pt = (*i)->params->begin(); pt != (*i)->params->end(); ++pt) {
      ps->push_back(pt->second);
    }
    top = new TypeEnv((*i)->name,
                      make_fun_type((*i)->lineno, ps, (*i)->return_type),
                      top);
  }
  return top;
}
