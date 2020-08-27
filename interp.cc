#include <vector>
#include <map>
#include <iostream>
#include "interp.h"
#include "assoc_list.h"

using std::vector;
using std::map;
using std::cout;
using std::cerr;
using std::endl;

typedef unsigned int address;

struct Value {
  TypeKind tag;
  bool alive;
  union {
    int integer;
    bool boolean;
    int fun;
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

Value* make_fun_val(int index) {
  Value* v = new Value();
  v->tag = FunT; v->alive = true; v->u.fun = index;
  return v;
}

Value* make_ptr_val(address addr) {
  Value* v = new Value();
  v->tag = FunT; v->alive = true; v->u.ptr = addr;
  return v;
}

typedef Node<string, address> Env;

struct Frame {
  FunDef* fun;
  Env* env;
};

struct State {
  vector<Value*> heap;
  vector<Frame*> stack;
};

Value* interp_exp(Exp* e, State* state);

address interp_lvalue(LValue* lval, State* state) {
  switch (lval->tag) {
  case Var:
    return lookup(state->stack.front()->env, *(lval->u.var));
  case Deref:
    Value* v = interp_exp(lval->u.deref, state);
    switch (v->tag) {
    case PtrT:
      if (! state->heap[v->u.ptr]->alive)
        cerr << "undefined behavior: access to dead object" << endl;
      return v->u.ptr;
    default:
      cerr << "type error: dereference expected a pointer" << endl;
      exit(-1);
    }
  }
}

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

Value* interp_exp(Exp* e, State* state) {
  switch (e->tag) {
  case LValueExp: {
    address addr = interp_lvalue(e->u.lvalue, state);
    return state->heap[addr];
  }
  case Int:
    return make_int_val(e->u.integer);
  case AddrOf: {
    address addr = interp_lvalue(e->u.addr_of, state);
    return make_ptr_val(addr);
  }
  case PrimOp: {
    vector<Value*> args(e->u.prim_op.args->size());
    // Left-to-right evaluation order
    int i = 0;
    for (auto iter = e->u.prim_op.args->begin();
         iter != e->u.prim_op.args->end();
         ++iter, ++i) {
      args[i] = interp_exp(*iter, state);
    }
    return eval_prim(e->u.prim_op.op, args);
  }
  }
}

typedef list<Block*>::iterator block_iter;

block_iter find_block(list<Block*>* blocks, string label) {
  for (auto b = blocks->begin(); b != blocks->end(); ++b) {
    if ((*b)->label == label)
      return b;
  }
  return blocks->end();
}

int interp_blocks(list<Block*>* blocks, State* state) {
  auto curr = find_block(blocks, "start");

  while (curr != blocks->end()) {
    for (auto s = (*curr)->stmts->begin(); s != (*curr)->stmts->end(); ++s) {
      switch ((*s)->tag) {
      case Assign: {
        address a = interp_lvalue((*s)->u.assign.lhs, state);
        Value* v = interp_exp((*s)->u.assign.rhs, state);
        state->heap[a] = v;
        break;
      }
      case Call: {
        cerr << "call not implemented" << endl;
        break;
      }
      case Free: {
        Value* v = interp_exp((*s)->u.free, state);
        address a = val_to_ptr(v);
        state->heap[a]->alive = false;
        break;
      }
      case IfGoto: {
        Value* c = interp_exp((*s)->u.if_goto.cond, state);
        if (val_to_bool(c)) {
          curr = find_block(blocks, *((*s)->u.if_goto.target));
          goto finish_block;
        }
        break;
      }
      case Return: {
        Value* v = interp_exp((*s)->u.free, state);
        return val_to_int(v);
      }
      }
    }
  finish_block:
    ;
  }
  cerr << "fell through end of function without return" << endl;
  exit(-1);
}

void interp_program(list<FunDef*>* fs) {
  FunDef* main = 0;
  for (auto iter = fs->begin(); iter != fs->end(); ++iter) {
    if ((*iter)->name == string("main"))
      main = *iter;
  }
  State* initial = new State();
  for (auto p = main->params->begin(); p != main->params->end(); ++p) {
    address a = initial->heap.size();
    initial->heap.push_back(0);
    initial->stack.front()->env =
      new Env((*p).first, a, initial->stack.front()->env);
  }
  int ret = interp_blocks(main->body, initial);
  cout << "result: " << ret << endl;
}
