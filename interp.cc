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

void print_val(Value* val) {
  switch (val->tag) {
  case IntT:
    cout << val->u.integer;
    break;
  case BoolT:
    cout << val->u.boolean;
    break;
  case FunT:
    cout << "fun<" << val->u.fun->name << ">";
    break;
  case PtrT:
    cout << "ptr<" << val->u.ptr << ">";
    break;
  }
}

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

void print_ctx(Ctx* ctx) {
  switch (ctx->tag) {
  case LValCtx:
  case ExpCtx:
    print_exp(ctx->u.exp);
    break;
  case StmtCtx:
    print_stmt(ctx->u.stmt);
    break;
  case ValCtx:
    print_val(ctx->u.val);
    break;
  }
  cout << "[" << ctx->pos << "]";
  for (auto iter = ctx->results.begin(); iter != ctx->results.end(); ++iter) {
    if (*iter)
      print_val(*iter);
    cout << ",";
  }
}

void print_ctx_list(Cons<Ctx*>* ls) {
  if (ls) {
    print_ctx(ls->curr);
    if (ls->next) {
      cout << " :: ";
      print_ctx_list(ls->next);
    }
  }
}

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

void print_frame(Frame* frame) {
  if (frame->fun)
    cout << frame->fun->name;
  cout << "{";
  print_ctx_list(frame->control);
  cout << "}"; 
}

// { S, H }
struct State {
  Cons<Frame*>* stack;
  vector<Value*> heap;
  list<FunDef*>* funs;
};

void print_stack(Cons<Frame*>* ls) {
  if (ls) {
    print_frame(ls->curr);
    if (ls->next) {
      cout << " :: ";
      print_stack(ls->next);
    }
  }
}

void print_heap(vector<Value*>& heap) {
  for (auto iter = heap.begin(); iter != heap.end(); ++iter) {
    if (*iter) {
      print_val(*iter);
    } else {
        cout << "_";
    }
      cout << ", ";
  }
}

void print_state(State* state) {
  cout << "{" << endl;
  cout << "stack: ";
  print_stack(state->stack);
  cout << endl << "heap: ";
  print_heap(state->heap);
  cout << endl << "}" << endl;
}

int val_to_int(Value* v) {
  switch (v->tag) {
  case IntT:
    return v->u.integer;
  default:
    cerr << "runtime type error: expected an integer" << endl;
    exit(-1);
  }
}

int val_to_bool(Value* v) {
  switch (v->tag) {
  case BoolT:
    return v->u.boolean;
  default:
    cerr << "runtime type error: expected a Boolean" << endl;
    exit(-1);
  }
}

address val_to_ptr(Value* v) {
  switch (v->tag) {
  case PtrT:
    return v->u.ptr;
  default:
    cerr << "runtime type error: expected a pointer" << endl;
    exit(-1);
  }
}

FunDef* val_to_fun(Value* v) {
  switch (v->tag) {
  case FunT:
    return v->u.fun;
  default:
    cerr << "runtime type error: expected a function" << endl;
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
  
  cout << "handle value "; print_val(val_ctx->u.val);
  cout << " in context "; print_ctx(ctx); cout << endl;
  
  ctx->results.push_back(val_ctx->u.val);
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
    case Call: {
      if (ctx->pos == 1 + exp->u.call.args->size()) {
        //    { { v :: lhs = f(vs,[]) :: C, E, F} :: S, H}
        // -> { {C',E',F'} :: {(x = []) :: C, E, F} :: S, H}
        frame->control = frame->control->next->next;
        call_function(ctx->results, state);
      } else if (ctx->pos == 1) {
        //    { { v :: [](e,es) :: C, E, F} :: S, H}
        // -> { { e :: v([],es) :: C, E, F} :: S, H}
        if (exp->u.call.args->size() > 0) {
          frame->control = cons(make_exp_ctx((*exp->u.call.args)[0]),
                                frame->control->next);
        } else {
          
        }
      } else {
        //    { { v :: call(f, (vs,[],e,es)) :: C, E, F} :: S, H}
        // -> { { e :: call(f, (vs,v,[],es)) :: C, E, F} :: S, H}
        frame->control = cons(make_exp_ctx((*exp->u.call.args)[ctx->pos - 1]),
                              frame->control->next);
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
  cout << "step lvalue "; print_exp(exp); cout << endl;
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
  cout << "step exp "; print_exp(exp); cout << endl;
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
  case Call:
    //    { {e(es) :: C, E, F} :: S, H}
    // -> { {e :: [](es) :: C, E, F} :: S, H}
    frame->control = cons(make_exp_ctx(exp->u.call.fun),
                          frame->control);
    ctx->pos++;
    break;
  }
}

void step_stmt(State* state) {
  Frame* frame = state->stack->curr;
  Ctx* ctx = frame->control->curr;
  Stmt* stmt = ctx->u.stmt;
  cout << "step stmt "; print_stmt(stmt); cout << endl;
  switch (stmt->tag) {
  case Assign:
    //    { {(lv = e) :: C, E, F} :: S, H}
    // -> { {lv :: ([] = e) :: C, E, F} :: S, H}
    frame->control = cons(make_lval_ctx(stmt->u.assign.lhs),
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
  address a = state->heap.size();
  state->heap.push_back(0);

  Exp* call_main = make_call(0, make_var(0, "main"), new list<Exp*>());
  Cons<Ctx*>* control = cons(make_exp_ctx(call_main), (Cons<Ctx*>*)0);
  Frame* frame = new Frame(0, env, control);
  state->stack = cons(frame, (Cons<Frame*>*)0);

  print_state(state);
  // Run the program
  while (length(state->stack) > 1
         || length(state->stack->curr->control) > 1
         || state->stack->curr->control->curr->tag != ValCtx) {
    step(state);
    print_state(state);
  }
  Value* v = state->stack->curr->control->curr->u.val;
  return val_to_int(v);
}
