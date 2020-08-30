#include <vector>
#include <map>
#include <iostream>
#include "interp.h"
#include "assoc_list.h"
#include "cons_list.h"

using std::vector;
using std::map;
using std::cout;
using std::cerr;
using std::endl;

typedef unsigned int address;

/***** Values *****/

struct Value {
  TypeKind tag;
  bool alive;
  union {
    int integer;
    bool boolean;
    FunDef* fun;
    address ptr;
  } u;
};

Value* make_int_val(int i) {
  Value* v = new Value();
  v->tag = IntT; v->alive = true; v->u.integer = i;
  return v;
}

Value* make_bool_val(bool b) {
  Value* v = new Value();
  v->tag = BoolT; v->alive = true; v->u.boolean = b;
  return v;
}

Value* make_fun_val(FunDef* f) {
  Value* v = new Value();
  v->tag = FunT; v->alive = true; v->u.fun = f;
  return v;
}

Value* make_ptr_val(address addr) {
  Value* v = new Value();
  v->tag = FunT; v->alive = true; v->u.ptr = addr;
  return v;
}

/***** Contexts *****/

enum CtxKind { LValCtx, ExpCtx, StmtCtx, ValCtx };

struct Ctx {
  CtxKind tag;
  union {
    Exp* exp;   // for LValCtx and ExpCtx
    Stmt* stmt;
    Value* val;
  } u;
  
  int pos;
  vector<Value*> results;
};

Ctx* make_exp_ctx(Exp* e) {
  Ctx* ctx = new Ctx();
  ctx->tag = ExpCtx;
  ctx->u.exp = e;
  ctx->pos = -1;
  return ctx;
}

Ctx* make_lval_ctx(Exp* e) {
  Ctx* ctx = new Ctx();
  ctx->tag = LValCtx;
  ctx->u.exp = e;
  ctx->pos = -1;
  return ctx;
}

Ctx* make_stmt_ctx(Stmt* s) {
  Ctx* ctx = new Ctx();
  ctx->tag = StmtCtx;
  ctx->u.stmt = s;
  ctx->pos = -1;
  return ctx;
}

Ctx* make_val_ctx(Value* v) {
  Ctx* ctx = new Ctx();
  ctx->tag = ValCtx;
  ctx->u.val = v;
  ctx->pos = -1;
  return ctx;
}

typedef AList<string, address> Env;

// { C, E, F }
struct Frame {
  Cons<Ctx*>* control;
  Env* env;
  FunDef* fun;
  Frame(FunDef* f, Env* e, Cons<Ctx*>* c) : fun(f), env(e), control(c) { }
};

// { S, H }
struct State {
  Cons<Frame*>* stack;
  vector<Value*> heap;
  list<FunDef*>* funs;
};

int val_to_int(Value* v) {
  switch (v->tag) {
  case IntT:
    return v->u.integer;
  default:
    cerr << "type error, expected an integer" << endl;
    exit(-1);
  }
}

int val_to_bool(Value* v) {
  switch (v->tag) {
  case BoolT:
    return v->u.boolean;
  default:
    cerr << "type error, expected a Boolean" << endl;
    exit(-1);
  }
}

address val_to_ptr(Value* v) {
  switch (v->tag) {
  case PtrT:
    return v->u.ptr;
  default:
    cerr << "type error, expected a pointer" << endl;
    exit(-1);
  }
}

FunDef* val_to_fun(Value* v) {
  switch (v->tag) {
  case FunT:
    return v->u.fun;
  default:
    cerr << "type error, expected a pointer" << endl;
    exit(-1);
  }
}

bool val_equal(Value* v1, Value* v2) {
  return (v1->tag == IntT && v2->tag == IntT && v1->u.integer == v2->u.integer)
    || (v1->tag == BoolT && v2->tag == BoolT && v1->u.boolean == v2->u.boolean)
    || (v1->tag == PtrT && v2->tag == PtrT && v1->u.ptr == v2->u.ptr)
    || (v1->tag == FunT && v2->tag == FunT && v1->u.fun == v2->u.fun);
}

Value* eval_prim(Operator op, const vector<Value*>& args) {
  switch (op) {
  case Neg:
    return make_int_val(- val_to_int(args[0]));
  case Add:
    return make_int_val(val_to_int(args[0]) + val_to_int(args[1]));
  case Sub:
    return make_int_val(val_to_int(args[0]) - val_to_int(args[1]));
  case Not:
    return make_bool_val(- val_to_bool(args[0]));
  case And:
    return make_bool_val(val_to_bool(args[0]) && val_to_bool(args[1]));
  case Or:
    return make_bool_val(val_to_bool(args[0]) || val_to_bool(args[1]));
  case Eq:
    return make_bool_val(val_equal(args[0], args[1]));
  }
}

Env* global_functions(State* state) {
  Env* env = 0;
  for (auto iter = state->funs->begin(); iter != state->funs->end(); ++iter) {
    address a = state->heap.size();
    state->heap.push_back(make_fun_val(*iter));    
    env = new Env((*iter)->name, a, env);
  }
  return env;
}

//    { S, H} -> { { C, E, F} :: S, H}
// where C is the body of the function,
//       E is the environment (functions + parameters + locals)
//       F is the function
void call_function(vector<Value*> operas, State* state) {
  FunDef* f = val_to_fun(operas[0]);
  Env* env = global_functions(state);

  // Bind arguments to parameters
  int pos = 1;
  for (auto vt = f->params->begin(); vt != f->params->end(); ++vt, ++pos) {
    address a = state->heap.size();
    state->heap.push_back(operas[pos]);    
    env = new Env((*vt).first, a, env);
  }
  
  // Allocate the local variables for the function
  for (auto l = f->locals->begin(); l != f->locals->end(); ++l) {
    address a = state->heap.size();
    state->heap.push_back(0);
    env = new Env((*l).first, a, env);
  }

  // Create the new frame and push it on the stack
  
  Frame* frame = new Frame(f, env, cons(make_stmt_ctx(f->body),
                                        (Cons<Ctx*>*)0));
  state->stack = cons(frame, state->stack);
}

Cons<Ctx*>* goto_label(string label, Stmt* stmt, Cons<Ctx*>* context) {
  switch (stmt->tag) {
  case Label: {
    if (*(stmt->u.labeled.label) == label) {
      return cons(make_stmt_ctx(stmt->u.labeled.stmt), (Cons<Ctx*>*)0);
    } else {
      return goto_label(label, stmt->u.labeled.stmt,
                        cons(make_stmt_ctx(stmt), context));
    }
  }
  case Seq: {
    Cons<Ctx*>* new_context = goto_label(label, stmt->u.seq.stmt, context);
    if (new_context) {
      return new_context;
    } else {
      return goto_label(label, stmt->u.seq.next, context);
    }
  }
  default:
    return 0;
  } // switch (stmt->tag)
}

void handle_value(State* state) {
  Frame* frame = state->stack->curr;
  Ctx* val_ctx = frame->control->curr;
  Ctx* ctx = frame->control->next->curr;
  ctx->results[ctx->pos] = val_ctx->u.val;
  ctx->pos++;
             
  switch (ctx->tag) {
  case ExpCtx: {
    Exp* exp = ctx->u.exp;
    switch (exp->tag) {
    case Deref: {
      //    { {a :: *[] :: C, E, F} :: S, H}
      // -> { {H(a) :: C, E, F} :: S, H}
      address a = val_to_ptr(ctx->results[0]);
      frame->control = cons(make_val_ctx(state->heap[a]),
                            frame->control->next->next);
    }
    case PrimOp: {
      if (ctx->pos != exp->u.prim_op.args->size()) {
        //    { {v :: op(vs,[],e,es) :: C, E, F} :: S, H}
        // -> { {e :: op(vs,v,[],es) :: C, E, F} :: S, H}
        Exp* arg = (*exp->u.prim_op.args)[ctx->pos];
        frame->control = cons(make_exp_ctx(arg), frame->control);
      } else {
        //    { {v :: op(vs,[]) :: C, E, F} :: S, H}
        // -> { {eval_prim(op, (vs,v)) :: C, E, F} :: S, H}
        Value* v = eval_prim(exp->u.prim_op.op, ctx->results);
        frame->control = cons(make_val_ctx(v), frame->control->next);
      }
      break;
    }
    default:
      cerr << "bad expression context in handle_value" << endl;
    }
    break;
  }
  case StmtCtx: {
    Stmt* stmt = ctx->u.stmt;
    switch (stmt->tag) {
    case Assign:
      if (ctx->pos == 1) {
        //    { { a :: ([] = e) :: C, E, F} :: S, H}
        // -> { { e :: (a = []) :: C, E, F} :: S, H}
        frame->control = cons(make_exp_ctx(stmt->u.assign.rhs),
                              frame->control->next);
      } else if (ctx->pos == 2) {
        //    { { v :: (a = []) :: C, E, F} :: S, H}
        // -> { { C, E, F} :: S, H(a := v)}
        Value* a = ctx->results[0];
        Value* v = ctx->results[1];
        state->heap[val_to_ptr(a)] = v;
        frame->control = frame->control->next->next;
      }
      break;
    case Call:
      if (ctx->pos == 1) {
        //    { { v :: call([],(e,es)) :: C, E, F} :: S, H}
        // -> { { e :: call(v, ([],es)) :: C, E, F} :: S, H}
        frame->control = cons(make_exp_ctx((*stmt->u.call.args)[0]),
                              frame->control->next);
      } else if (ctx->pos != 1 + stmt->u.call.args->size()) {
        //    { { v :: call(f, (vs,[],e,es)) :: C, E, F} :: S, H}
        // -> { { e :: call(f, (vs,v,[],es)) :: C, E, F} :: S, H}
        frame->control = cons(make_exp_ctx((*stmt->u.call.args)[ctx->pos - 1]),
                              frame->control->next);
      } else {
        //    { { v :: call(f, (vs,[])) :: C, E, F} :: S, H}
        // -> { {C',E',F'} :: {C, E, F} :: S, H}
        frame->control = frame->control->next->next;
        call_function(ctx->results, state);
      }
      break;
    case IfGoto:
      if (val_to_bool(ctx->results[0])) {
        //    { {v :: if [] goto l :: C, E, F} :: S, H}
        // -> { { goto(l), E, F } :: S, H}
        frame->control = goto_label(* (stmt->u.if_goto.target),
                                    frame->fun->body, 0);
      } else {
        //    { {v :: if [] goto l :: C, E, F} :: S, H}
        // -> { { C, E, F} :: S, H}
        frame->control = frame->control->next->next;
      }
      break;
    case Return:
      //    { {v :: return [] :: C, E, F} :: {C', E', F'} :: S, H}
      // -> { {v :: C', E', F'} :: S, H}
      state->stack = state->stack->next;
      frame = state->stack->curr;
      frame->control = cons(val_ctx, frame->control);
      break;
    default:
      cerr << "unhandled statement" << endl;
    } // switch stmt
    break;
  }
  default:
    cerr << "bad context in handle_value" << endl;
  } // switch ctx
}

void step_lvalue(State* state) {
  Frame* frame = state->stack->curr;
  Ctx* ctx = frame->control->curr;
  Exp* exp = ctx->u.exp;
  switch (exp->tag) {
  case Var: {
    // { {x :: C, E, F} :: S, H} -> { {E(x) :: C, E, F} :: S, H}
    address a = lookup(frame->env, *(exp->u.var));
    Value* v = make_ptr_val(a);
    frame->control = cons(make_val_ctx(v), frame->control->next);
    break;
  }
  case Deref: {
    // { {*e :: C, E, F} :: S, H} -> { e :: C, E, F} :: S, H}
    // Note: we do not push *[] because it's not needed in lvalue context.
    frame->control = cons(make_exp_ctx(exp->u.deref), frame->control->next);
    ctx->pos++;
    break;
  }
  default:
    cerr << "error, not an lvalue" << endl;
  }
}

void step_exp(State* state) {
  Frame* frame = state->stack->curr;
  Ctx* ctx = frame->control->curr;
  Exp* exp = ctx->u.exp;
  switch (exp->tag) {
  case Var: {
    // { {x :: C, E, F} :: S, H} -> { {H(E(x)) :: C, E, F} :: S, H}
    address a = lookup(frame->env, *(exp->u.var));
    Value* v = state->heap[a];
    frame->control = cons(make_val_ctx(v), frame->control->next);
    break;
  }
  case Deref: {
    // { {*e :: C, E, F} :: S, H} -> { { e :: *[] :: C, E, F} :: S, H}
    frame->control = cons(make_exp_ctx(exp->u.deref), frame->control);
    ctx->pos++;
    break;
  }
  case Int:
    // { {n :: C, E, F} :: S, H} -> { {n' :: C, E, F} :: S, H}
    frame->control = cons(make_val_ctx(make_int_val(exp->u.integer)),
                          frame->control->next);
    break;
  case AddrOf:
    // { {&e :: C, E, F} :: S, H} -> { {e :: C, E, F} :: S, H}
    // Note: we do not push &[] because it's the identity.
    frame->control = cons(make_lval_ctx(exp->u.addr_of),
                          frame->control->next);
    ctx->pos++;
    break;
  case PrimOp:
    //    { {op(e :: es) :: C, E, F} :: S, H}
    // -> { e :: op([] :: es) :: C, E, F} :: S, H}
    frame->control = cons(make_exp_ctx(exp->u.prim_op.args->front()),
                          frame->control);
    ctx->pos++;
    break;
  }
}

void step_stmt(State* state) {
  Frame* frame = state->stack->curr;
  Ctx* ctx = frame->control->curr;
  Stmt* stmt = ctx->u.stmt;
  switch (stmt->tag) {
  case Assign:
    //    { {(lv = e) :: C, E, F} :: S, H}
    // -> { {lv :: ([] = e) :: C, E, F} :: S, H}
    frame->control = cons(make_lval_ctx(stmt->u.assign.lhs),
                          frame->control);
    ctx->pos++;
    break;
  case Call:
    //    { {call(e, es) :: C, E, F} :: S, H}
    // -> { {e :: call([],es) :: C, E, F} :: S, H}
    frame->control = cons(make_exp_ctx(stmt->u.call.fun),
                          frame->control);
    ctx->pos++;
    break;
  case Free:
    cerr << "step free not implemented" << endl;
    break;
  case IfGoto:
    //    { {(if (e) goto l) :: C, E, F} :: S, H}
    // -> { { e :: (if [] goto l) :: C, E, F} :: S, H}
    frame->control = cons(make_exp_ctx(stmt->u.if_goto.cond),
                          frame->control);
    ctx->pos++;
    break;
  case Label:
    //    { {(l: s) :: C, E, F} :: S, H}
    // -> { {s :: C, E, F} :: S, H}
    frame->control = cons(make_stmt_ctx(stmt->u.labeled.stmt),
                          frame->control->next);
    break;
  case Return:
    //    { {return e :: C, E, F} :: S, H}
    // -> { {e :: return [] :: C, E, F} :: S, H}
    frame->control = cons(make_exp_ctx(stmt->u.ret),
                          frame->control);
    ctx->pos++;
    break;
  case Seq:
    //    { { (s1,s2) :: C, E, F} :: S, H}
    // -> { { s1 :: s2 :: C, E, F} :: S, H}
    frame->control = cons(make_stmt_ctx(stmt->u.seq.stmt),
                              cons(make_stmt_ctx(stmt->u.seq.next),
                                       frame->control->next));
    break;
  }
}

void step(State* state) {
  Frame* frame = state->stack->curr;
  Ctx* ctx = frame->control->curr;
  switch (ctx->tag) {
  case ValCtx:
    handle_value(state);
    break;
  case LValCtx:
    step_lvalue(state);
    break;
  case ExpCtx:
    step_exp(state);
    break;
  case StmtCtx:
    step_stmt(state);
    break;
  } // switch
}

int interp_program(list<FunDef*>* fs) {
  State* state = new State();
  state->funs = fs;

  Env* env = global_functions(state);
  Frame* frame = new Frame(0, env, 0);
  state->stack = cons(frame, (Cons<Frame*>*)0);

  // Run the program
  while (state->stack->next != 0
         || state->stack->curr->control->curr->tag != ValCtx) {
    step(state);
  }
  Value* v = state->stack->curr->control->curr->u.val;
  return val_to_int(v);
}
