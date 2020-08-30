#ifndef CON_LIST_H
#define CON_LIST_H

template<class T>
struct Cons {
  T curr;
  Cons* next;
  Cons(T e, Cons* n) : curr(e), next(n) { }
};


#endif
