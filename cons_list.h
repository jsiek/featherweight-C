#ifndef CONS_LIST_H
#define CONS_LIST_H

template<class T>
struct Cons {
  T curr;
  Cons* next;
  Cons(T e, Cons* n) : curr(e), next(n) { }
};

template<class T>
Cons<T>* cons(const T& x, Cons<T>* ls) {
  return new Cons<T>(x, ls);
}


#endif
