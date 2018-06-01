:- module(eval, [
      bool/2,
      arith/3,
      vars/3,
      expr/3
    ]).

:- use_module(state).

arith(State, +(X,Y), Res) :-
    arith(State, X, X_),
    arith(State, Y, Y_),
    !,
    Res is X_ + Y_.
arith(State, -(X,Y), Res) :-
    arith(State, X, X_),
    arith(State, Y, Y_),
    !,
    Res is X_ - Y_.
arith(State, *(X,Y), Res) :-
    arith(State, X, X_),
    arith(State, Y, Y_),
    !,
    Res is X_ * Y_.
arith(State, /(X,Y), Res) :-
    arith(State, X, X_),
    arith(State, Y, Y_),
    !,
    Res is X_ / Y_.
arith(State, intvar(Var), Res) :-
    state:lookup(State, var(Var), Res),!.
arith(_, X, X).

% or, and, <, =, lookup

bool(_, false)        :- false.
bool(_, true)         :- true.
bool(State, or(X, Y)) :- bool(State, X) ; bool(State, Y).
bool(State, and(X,Y)) :- bool(State, X) , bool(State, Y). 
bool(State, eq(X, Y)) :-
    arith(State, X, X_), arith(State, Y, Y_), X_ = Y_ ;
    bool(State, X) = bool(State, Y).
bool(State, leq(X, Y)) :-
    arith(State, X, X_),
    arith(State, Y, Y_),!,
    X_ =< Y_.
bool(State, !(X))     :- bool(State, X) = false.

% general expressions

vars(_, intvar(Var), [var(Var, 0)]).
vars(_, var(Var), [var(Var, 0)]).
vars(State, var(Var, Idx), [var(Var, IdxExpanded)]) :-
  arith(State, Idx, IdxExpanded).
vars(_, listval(List), [listval(List)]).
vars(State, o(G0,G1), Res) :-
  vars(State, G0, Res0), vars(State, G1, Res1),
  append(Res0, Res1, Res).
vars(State, +(G0,G1), Res) :-
  vars(State, G0, Res0), vars(State, G1, Res1),
  append(Res0, Res1, Res).
vars(State, -(G0,G1), Res) :-
  vars(State, G0, Res0), vars(State, G1, Res1),
  append(Res0, Res1, Res).
vars(State, *(G0,G1), Res) :-
  vars(State, G0, Res0), vars(State, G1, Res1),
  append(Res0, Res1, Res).
vars(State, /(G0,G1), Res) :-
  vars(State, G0, Res0), vars(State, G1, Res1),
  append(Res0, Res1, Res).
vars(State, /(G0,G1), Res) :-
  vars(State, G0, Res0), vars(State, G1, Res1),
  append(Res0, Res1, Res).
vars(State, leq(G0,G1), Res) :-
  vars(State, G0, Res0), vars(State, G1, Res1),
  append(Res0, Res1, Res).
vars(_, _, []).

expr(_, _, trefle).


instanciate_vars(State, [var(Name, Idx) | Tail], [var(Name, Idx_) | Tail]) :-
  eval:arith(State, Idx, Idx_).
instanciate_vars(_, [var(Name) | Tail], [var(Name) | Tail]).
instanciate_vars(_, [], []).
