#include <vector>
#include <map>
#include <iostream>
#include "interp.h"
#include "assoc_list.h"
#include "cons_list.h"
#include "typecheck.h" // for print_error_string

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
  v->alive = true;
  v->tag = IntT; v->alive = true; v->u.integer = i;
  return v;
}

Value* make_bool_val(bool b) {
  Value* v = new Value();
  v->alive = true;
  v->tag = BoolT; v->alive = true; v->u.boolean = b;
  return v;
}

Value* make_fun_val(FunDef* f) {
  Value* v = new Value();
  v->alive = true;
  v->tag = FunT; v->alive = true; v->u.fun = f;
  return v;
}

Value* make_ptr_val(address addr) {
  Value* v = new Value();
  v->alive = true;
  v->tag = PtrT; v->alive = true; v->u.ptr = addr;
  return v;
}

/***** Actions *****/

enum ActKind { LValAct, ExpAct, StmtAct, ValAct };

struct Act {
  ActKind tag;
  union {
    Exp* exp;             // exp is for LValAct and ExpAct
    Stmt* stmt;
    Value* val;           // val is for finished actions with a value (ValAct)
  } u;
  int pos;                // position or state of the action
  vector<Value*> results; // results from subexpression
};
typedef Cons<Act*>* ActList;

void print_act(Act* act) {
  switch (act->tag) {
  case LValAct:
  case ExpAct:
    print_exp(act->u.exp);
    break;
  case StmtAct:
    print_stmt(act->u.stmt, 1);
    break;
  case ValAct:
    print_val(act->u.val);
    break;
  }
  cout << "[" << act->pos << "]";
  if (act->results.size() > 0) {
    cout << "(";
    for (auto iter = act->results.begin(); iter != act->results.end(); ++iter) {
      if (*iter)
        print_val(*iter);
      cout << ",";
    }
    cout << ")";
  }
}

void print_act_list(Cons<Act*>* ls) {
  if (ls) {
    print_act(ls->curr);
    if (ls->next) {
      cout << " :: ";
      print_act_list(ls->next);
    }
  }
}

Act* make_exp_act(Exp* e) {
  Act* act = new Act();
  act->tag = ExpAct;
  act->u.exp = e;
  act->pos = -1;
  return act;
}

Act* make_lval_act(Exp* e) {
  Act* act = new Act();
  act->tag = LValAct;
  act->u.exp = e;
  act->pos = -1;
  return act;
}

Act* make_stmt_act(Stmt* s) {
  Act* act = new Act();
  act->tag = StmtAct;
  act->u.stmt = s;
  act->pos = -1;
  return act;
}

Act* make_val_act(Value* v) {
  Act* act = new Act();
  act->tag = ValAct;
  act->u.val = v;
  act->pos = -1;
  return act;
}

/***** Frames and State *****/

typedef AList<string, address> Env;

// { C, E, F }
struct Frame {
  Cons<Act*>* todo;
  Env* env;
  FunDef* fun;
  Frame(FunDef* f, Env* e, Cons<Act*>* c) : fun(f), env(e), todo(c) { }
};

void print_frame(Frame* frame) {
  if (frame->fun)
    cout << frame->fun->name;
  cout << "{";
  print_act_list(frame->todo);
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

/***** Auxilliary Functions *****/


void check_alive(Value* v) {
  if (! v->alive) {
    cerr << "undefined: access to dead value" << endl;
    exit(-1);
  }
}

int val_to_int(Value* v) {
  check_alive(v);
  switch (v->tag) {
  case IntT:
    return v->u.integer;
  default:
    cerr << "runtime type error: expected an integer" << endl;
    exit(-1);
  }
}

int val_to_bool(Value* v) {
  check_alive(v);
  switch (v->tag) {
  case BoolT:
    return v->u.boolean;
  default:
    cerr << "runtime type error: expected a Boolean" << endl;
    exit(-1);
  }
}

address val_to_ptr(Value* v) {
  check_alive(v);
  switch (v->tag) {
  case PtrT:
    return v->u.ptr;
  default:
    cerr << "runtime type error: expected a pointer" << endl;
    exit(-1);
  }
}

FunDef* val_to_fun(Value* v) {
  check_alive(v);
  switch (v->tag) {
  case FunT:
    return v->u.fun;
  default:
    cerr << "runtime type error: expected a function" << endl;
    exit(-1);
  }
}

bool val_equal(Value* v1, Value* v2) {
  check_alive(v1);
  check_alive(v2);
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

Env* global_functions;
  
void init_global_functions(State* state) {
  global_functions = 0;
  for (auto iter = state->funs->begin(); iter != state->funs->end(); ++iter) {
    address a = state->heap.size();
    state->heap.push_back(make_fun_val(*iter));    
    global_functions = new Env((*iter)->name, a, global_functions);
  }
}

//    { S, H} -> { { C, E, F} :: S, H}
// where C is the body of the function,
//       E is the environment (functions + parameters + locals)
//       F is the function
void call_function(vector<Value*> operas, State* state) {
  FunDef* f = val_to_fun(operas[0]);
  Env* env = global_functions;

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
    Value* v = make_int_val(0);
    v->alive = false;
    state->heap.push_back(v);
    env = new Env((*l).first, a, env);
  }

  // Create the new frame and push it on the stack
  
  Frame* frame = new Frame(f, env, cons(make_stmt_act(f->body),
                                        (Cons<Act*>*)0));
  state->stack = cons(frame, state->stack);
}

Cons<Act*>* goto_label(string label, Stmt* stmt, Cons<Act*>* todo) {
  switch (stmt->tag) {
  case Label: {
    if (*(stmt->u.labeled.label) == label) {
      return cons(make_stmt_act(stmt->u.labeled.stmt), (Cons<Act*>*)0);
    } else {
      return goto_label(label, stmt->u.labeled.stmt,
                        cons(make_stmt_act(stmt), todo));
    }
  }
  case Seq: {
    Cons<Act*>* new_todo = goto_label(label, stmt->u.seq.stmt, todo);
    if (new_todo) {
      return new_todo;
    } else {
      return goto_label(label, stmt->u.seq.next, todo);
    }
  }
  case If: {
    Cons<Act*>* new_todo = goto_label(label, stmt->u.if_stmt.thn, todo);
    if (new_todo) {
      return new_todo;
    } else {
      return goto_label(label, stmt->u.if_stmt.els, todo);
    }
  }
  default:
    return 0;
  } // switch (stmt->tag)
}

/***** State transition for handling a value *****/

void handle_value(State* state) {
  Frame* frame = state->stack->curr;
  Act* val_act = frame->todo->curr;
  Act* act = frame->todo->next->curr;
  
  cout << "--- handle value "; print_val(val_act->u.val);
  cout << " with "; print_act(act); cout << " --->" << endl;
  
  act->results.push_back(val_act->u.val);
  act->pos++;
             
  switch (act->tag) {
  case ExpAct: {
    Exp* exp = act->u.exp;
    switch (exp->tag) {
    case Deref: {
      //    { {a :: *[] :: C, E, F} :: S, H}
      // -> { {H(a) :: C, E, F} :: S, H}
      address a = val_to_ptr(val_act->u.val);
      Value* v = state->heap[a];
      if (! v->alive) {
        cerr << "undefined: access to dead memory" << endl;
        exit(-1);
      }
      frame->todo = cons(make_val_act(v),
                            frame->todo->next->next);
      break;
    }
    case PrimOp: {
      if (act->pos != exp->u.prim_op.args->size()) {
        //    { {v :: op(vs,[],e,es) :: C, E, F} :: S, H}
        // -> { {e :: op(vs,v,[],es) :: C, E, F} :: S, H}
        Exp* arg = (*exp->u.prim_op.args)[act->pos];
        frame->todo = cons(make_exp_act(arg), frame->todo->next);
      } else {
        //    { {v :: op(vs,[]) :: C, E, F} :: S, H}
        // -> { {eval_prim(op, (vs,v)) :: C, E, F} :: S, H}
        Value* v = eval_prim(exp->u.prim_op.op, act->results);
        frame->todo = cons(make_val_act(v), frame->todo->next->next);
      }
      break;
    }
    case Call: {
      if (act->pos == 1 + exp->u.call.args->size()) {
        //    { { v :: f(vs,[]) :: C, E, F} :: S, H}
        // -> { {C',E',F'} :: {C, E, F} :: S, H}
        frame->todo = frame->todo->next->next;
        call_function(act->results, state);
      } else {
        //    { { v :: f(vs,[],e,es)) :: C, E, F} :: S, H}
        // -> { { e :: f(vs,v,[],es)) :: C, E, F} :: S, H}
        frame->todo = cons(make_exp_act((*exp->u.call.args)[act->pos - 1]),
                              frame->todo->next);
      }
      break;
    }
    default:
      cerr << "bad expression context in handle_value" << endl;
    }
    break;
  }
  case StmtAct: {
    Stmt* stmt = act->u.stmt;
    switch (stmt->tag) {
    case Assign:
      if (act->pos == 1) {
        //    { { a :: ([] = e) :: C, E, F} :: S, H}
        // -> { { e :: (a = []) :: C, E, F} :: S, H}
        frame->todo = cons(make_exp_act(stmt->u.assign.rhs),
                              frame->todo->next);
      } else if (act->pos == 2) {
        //    { { v :: (a = []) :: C, E, F} :: S, H}
        // -> { { C, E, F} :: S, H(a := v)}
        Value* a = act->results[0];
        Value* v = act->results[1];
        state->heap[val_to_ptr(a)] = v;
        frame->todo = frame->todo->next->next;
      }
      break;
    case If:
      if (val_to_bool(act->results[0])) {
        //    { {true :: if ([]) thn else els :: C, E, F} :: S, H}
        // -> { { thn :: C, E, F } :: S, H}
        frame->todo = cons(make_stmt_act(stmt->u.if_stmt.thn),
                           frame->todo->next->next);
      } else {
        //    { {false :: if ([]) thn else els :: C, E, F} :: S, H}
        // -> { { els :: C, E, F } :: S, H}
        frame->todo = cons(make_stmt_act(stmt->u.if_stmt.els),
                           frame->todo->next->next);
      }
      break;
    case Goto:
      break;
    case Return:
      //    { {v :: return [] :: C, E, F} :: {C', E', F'} :: S, H}
      // -> { {v :: C', E', F'} :: S, H}
      state->stack = state->stack->next;
      frame = state->stack->curr;
      frame->todo = cons(val_act, frame->todo);
      break;
    default:
      cerr << "unhandled statement" << endl;
    } // switch stmt
    break;
  }
  default:
    cerr << "bad context in handle_value" << endl;
  } // switch act
}

/***** state transitions for lvalues *****/

void step_lvalue(State* state) {
  Frame* frame = state->stack->curr;
  Act* act = frame->todo->curr;
  Exp* exp = act->u.exp;
  cout << "--- step lvalue "; print_exp(exp); cout << " --->" << endl;
  switch (exp->tag) {
  case Var: {
    // { {x :: C, E, F} :: S, H} -> { {E(x) :: C, E, F} :: S, H}
    address a = lookup(frame->env, *(exp->u.var), print_error_string);
    Value* v = make_ptr_val(a);
    frame->todo = cons(make_val_act(v), frame->todo->next);
    break;
  }
  case Deref: {
    // { {*e :: C, E, F} :: S, H} -> { e :: C, E, F} :: S, H}
    // Note: we do not push *[] because it's not needed in lvalue context.
    frame->todo = cons(make_exp_act(exp->u.deref), frame->todo->next);
    act->pos++;
    break;
  }
  default:
    cerr << "error, not an lvalue" << endl;
  }
}

/***** state transitions for expressions *****/

void step_exp(State* state) {
  Frame* frame = state->stack->curr;
  Act* act = frame->todo->curr;
  Exp* exp = act->u.exp;
  cout << "--- step exp "; print_exp(exp); cout << " --->" << endl;
  switch (exp->tag) {
  case Var: {
    // { {x :: C, E, F} :: S, H} -> { {H(E(x)) :: C, E, F} :: S, H}
    address a = lookup(frame->env, *(exp->u.var), print_error_string);
    Value* v = state->heap[a];
    frame->todo = cons(make_val_act(v), frame->todo->next);
    break;
  }
  case Deref: {
    // { {*e :: C, E, F} :: S, H} -> { { e :: *[] :: C, E, F} :: S, H}
    frame->todo = cons(make_exp_act(exp->u.deref), frame->todo);
    act->pos++;
    break;
  }
  case Int:
    // { {n :: C, E, F} :: S, H} -> { {n' :: C, E, F} :: S, H}
    frame->todo = cons(make_val_act(make_int_val(exp->u.integer)),
                          frame->todo->next);
    break;
  case Bool:
    // { {n :: C, E, F} :: S, H} -> { {n' :: C, E, F} :: S, H}
    frame->todo = cons(make_val_act(make_bool_val(exp->u.boolean)),
                          frame->todo->next);
    break;
  case AddrOf:
    // { {&e :: C, E, F} :: S, H} -> { {e :: C, E, F} :: S, H}
    // Note: we do not push &[] because it's the identity.
    frame->todo = cons(make_lval_act(exp->u.addr_of),
                          frame->todo->next);
    act->pos++;
    break;
  case Malloc: {
    // { { malloc(T) :: C, E, F} :: S, H}
    // -> { { a :: C, E, F} :: S, H(a = _) }
    address a = state->heap.size();
    state->heap.push_back(0);
    frame->todo = cons(make_val_act(make_ptr_val(a)), frame->todo->next);
    break;
  }
  case PrimOp:
    //    { {op(e :: es) :: C, E, F} :: S, H}
    // -> { e :: op([] :: es) :: C, E, F} :: S, H}
    frame->todo = cons(make_exp_act(exp->u.prim_op.args->front()),
                          frame->todo);
    act->pos++;
    break;
  case Call:
    //    { {e(es) :: C, E, F} :: S, H}
    // -> { {e :: [](es) :: C, E, F} :: S, H}
    frame->todo = cons(make_exp_act(exp->u.call.fun),
                          frame->todo);
    act->pos++;
    break;
  }
}

/***** state transitions for statements *****/

void step_stmt(State* state) {
  Frame* frame = state->stack->curr;
  Act* act = frame->todo->curr;
  Stmt* stmt = act->u.stmt;
  cout << "--- step stmt "; print_stmt(stmt, 1); cout << " --->" << endl;
  switch (stmt->tag) {
  case Assign:
    //    { {(lv = e) :: C, E, F} :: S, H}
    // -> { {lv :: ([] = e) :: C, E, F} :: S, H}
    frame->todo = cons(make_lval_act(stmt->u.assign.lhs),
                          frame->todo);
    act->pos++;
    break;
  case Free:
    cerr << "step free not implemented" << endl;
    break;
  case If:
    //    { {(if (e) thn else els) :: C, E, F} :: S, H}
    // -> { { e :: (if ([]) thn else els) :: C, E, F} :: S, H}
    frame->todo = cons(make_exp_act(stmt->u.if_stmt.cond), frame->todo);
    act->pos++;
    break;
  case Goto:
    //    { { goto l :: C, E, F} :: S, H} -> { { goto_label(l), E, F } :: S, H}
    frame->todo = goto_label(* (stmt->u.goto_stmt.target), frame->fun->body, 0);
    break;
  case Label:
    //    { {(l: s) :: C, E, F} :: S, H}
    // -> { {s :: C, E, F} :: S, H}
    frame->todo = cons(make_stmt_act(stmt->u.labeled.stmt), frame->todo->next);
    break;
  case Return:
    //    { {return e :: C, E, F} :: S, H}
    // -> { {e :: return [] :: C, E, F} :: S, H}
    frame->todo = cons(make_exp_act(stmt->u.ret), frame->todo);
    act->pos++;
    break;
  case Seq:
    //    { { (s1,s2) :: C, E, F} :: S, H}
    // -> { { s1 :: s2 :: C, E, F} :: S, H}
    frame->todo = cons(make_stmt_act(stmt->u.seq.stmt),
                       cons(make_stmt_act(stmt->u.seq.next),
                            frame->todo->next));
    break;
  }
}

/***** state transition *****/

void step(State* state) {
  Frame* frame = state->stack->curr;
  Act* act = frame->todo->curr;
  switch (act->tag) {
  case ValAct:
    handle_value(state);
    break;
  case LValAct:
    step_lvalue(state);
    break;
  case ExpAct:
    step_exp(state);
    break;
  case StmtAct:
    step_stmt(state);
    break;
  } // switch
}

/***** interpret the whole program *****/

int interp_program(list<FunDef*>* fs) {
  State* state = new State();
  state->funs = fs;

  init_global_functions(state);
  address a = state->heap.size();
  state->heap.push_back(0);

  Exp* call_main = make_call(0, make_var(0, "main"), new list<Exp*>());
  Cons<Act*>* todo = cons(make_exp_act(call_main), (Cons<Act*>*)0);
  Frame* frame = new Frame(0, global_functions, todo);
  state->stack = cons(frame, (Cons<Frame*>*)0);

  print_state(state);
  // run the program
  while (length(state->stack) > 1
         || length(state->stack->curr->todo) > 1
         || state->stack->curr->todo->curr->tag != ValAct) {
    step(state);
    print_state(state);
  }
  Value* v = state->stack->curr->todo->curr->u.val;
  return val_to_int(v);
}
