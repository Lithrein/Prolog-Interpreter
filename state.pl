:- module(state, [ ]).

:- use_module(utils).
:- use_module(eval).

%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Definitions
%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A state is defined as a 4-uple
%   - Vec: An iteration vector stored as a list
%   - Reg: The next free block of memory available
%   - Loc: A table mapping names to their memory location
%   - Mem: A table mapping addresses to their values
   
% The empty state is made of:
%  - an empty iteration vector
%  - the next free available memory block is 0
%  - Loc is the empty map (no variables declared)
%  - Mem is the empty map (memory is completely empty)
%
% Most programs should use that state as their starting point.
init(state([], 0, Loc, Mem)) :-
  empty_assoc(Loc),
  empty_assoc(Mem).

get_vec(state(Vec, _, _, _), Vec).
get_reg(state(_, Reg, _, _), Reg).
get_loc(state(_, _, Loc, _), Loc).
get_mem(state(_, _, _, Mem), Mem).

update_vec(state(_, Reg, Loc, Mem), NewVec, state(NewVec, Reg, Loc, Mem)).
update_reg(state(Vec, _, Loc, Mem), NewReg, state(Vec, NewReg, Loc, Mem)).
update_loc(state(Vec, Reg, _, Mem), NewLoc, state(Vec, Reg, NewLoc, Mem)).
update_mem(state(Vec, Reg, Loc, _), NewMem, state(Vec, Reg, Loc, NewMem)).
%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Iteration vector
%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The iteration vector is a sequence with finite support used to number each
% operations in a program so as to be able to reference it later.
% 
% A program can be fully executed with an empty iteration vector all along but
% in that case it will not be possible to track operations and their
% dependencies.
%
% The annotation module is in charge of adding the iteration variables and
% basically, at the start of the program a new iteration variable `k0' will be
% pushed in the iteration vector. This variable will track each instruction
% executed at level 0 (ie. not in a loop nor in a conditional statement). Each
% time a loop or a conditional statement is found a new iteration variable with
% a name of the form `ki' where i is an integer never used before.
% Hence, the size of the iteration vector measures the depth of an operation.

% The iteration vector can be seen as a FIFO: `push' add a new iteration variable
% when the program enter a conditional statement or a loop. On the other hand
% 'drop` will get rid of the iteration variable of the top of the FIFO when the
% program leaves a loop or a conditional statement.
push(state(Vec, Reg, Loc, Mem), var(Var), state([Var | Vec], Reg, Loc, Mem)).
drop(state([_ | Vec], Reg, Loc, Mem), state(Vec, Reg, Loc, Mem)).

show_vector_(state([], _, _, _), [Vars, Vals]) :- !, Vars = [], Vals = [].
show_vector_(state([Var | Tail], _, Loc, Mem), [[Var | Vars], [Val | Vals]]) :-
  lookup(state([Var | Tail], _, Loc, Mem), var(Var), Val),
  show_vector_(state(Tail, _, Loc, Mem), [Vars, Vals]).

show_vector(state([], _, _, _), Res) :- !, Res = "".
show_vector(state(Vec, _, Loc, Mem), Res) :-
  show_vector_(state(Vec, _, Loc, Mem), Zipped),
  format(atom(Res), '~w = ~w', Zipped).

% @name lookup_var
% @brief Lookup a variable (array var) in the state and stores its
% value in val.
%
% ++ var is a string
% + offset is an arithmetic expression
% + state represent the state of reference
% - val
% @TODO: Address offset errors
lookup(state(_), _, nothing).
lookup(state(_, _, Loc, Mem), var(Var), Val) :- !,
  lookup(state(_, _, Loc, Mem), var(Var, 0), Val).
lookup(state(Vec, Reg, Loc, Mem), var(Var, Offset), Val) :- !,
  get_assoc(Var, Loc, var(Adr, _)),
  eval:arith(state(Vec, Reg, Loc, Mem), Offset, Offset_),
  Adr_ is Adr + Offset_,
  get_assoc(Adr_, Mem, (v(Val), _)).

lookup_vec(state(_), _, nothing).
lookup_vec(state(_, _, Loc, Mem), var(Var), Vect) :- !,
  lookup(state(_, _, Loc, Mem), var(Var, 0), Vect).
lookup_vec(state(Vec, Reg, Loc, Mem), var(Var, Offset), Vect) :- !,
  get_assoc(Var, Loc, var(Adr, _)),
  eval:arith(state(Vec, Reg, Loc, Mem), Offset, Offset_),
  Adr_ is Adr + Offset_,
  get_assoc(Adr_, Mem, (_, Vect)).


declare(state(ErrorMsg), var(_, _), state(ErrorMsg)).
declare(state(ErrorMsg), list(_), state(ErrorMsg)).
declare(state(Vec, Reg, Loc, Mem), var(Var), NewState) :-
  !,
  declare(state(Vec, Reg, Loc, Mem), var(Var, 1), NewState).
declare(state(Vec, Reg, Loc, Mem), var(Var, Size), NewState) :-
  ( get_assoc(Var, Loc, _) ->
      !,
      format(atom(ErrorMsg), 'ERROR: Attempt to redeclare the variable ~w.', Var),
      NewState = state(ErrorMsg)
  ;
      !,
      NewReg is Reg + Size,
      put_assoc(Var, Loc, var(Reg, Size), NewLoc),
      NewState = state(Vec, NewReg, NewLoc, Mem)
  ).
declare(state(Vec, Reg, Loc, Mem), list(Var), NewState) :-
  ( get_assoc(Var, Loc, _) ->
      format(atom(ErrorMsg), 'ERROR: Attempt to redeclare the variable ~w.', Var),
      NewState = state(ErrorMsg)
  ;
      put_assoc(Var, Loc, list(nil), NewLoc),
      NewState = state(Vec, Reg, NewLoc, Mem)
  ).

% Does not free memory ! Very dirty
% Works only with variable of size 1
undeclare(state(Vec, Reg, Loc, Mem), var(Var), NewState) :-
  ( get_assoc(Var, Loc, var(Adr, Size)) ->
      del_assoc(Var, Loc, var(Adr, Size), NewLoc),
      NewState = state(Vec, Reg, NewLoc, Mem)
  ;
      format(atom(ErrorMsg), 'ERROR: Attempt to undeclare the inexistant variable ~w.', Var),  
      NewState = state(ErrorMsg)
  ).

assign(state(ErrorMsg), var(_,_), _, state(ErrorMsg)).
assign(state(ErrorMsg), list(_), _, state(ErrorMsg)).
assign(state(Vec, Reg, Loc, Mem), var(Var), Val, NewState) :-
  !,
  eval:arith(state(Vec, Reg, Loc, Mem), Val, Val_),
  assign(state(Vec, Reg, Loc, Mem), var(Var, 0), Val_, NewState).
assign(state(Vec, Reg, Loc, Mem), var(Var, Offset), Val, NewState) :-
  ( get_assoc(Var, Loc, var(Adr, Size)) ->
      !,
      eval:arith(state(Vec, Reg, Loc, Mem), Offset, Offset_),
      ( Offset_ < Size ->
        !,
        RealAdr is Adr + Offset_,
        show_vector(state(Vec, Reg, Loc, Mem), IterVec),
        put_assoc(RealAdr, Mem, (v(Val), IterVec), NewMem),
        NewState = state(Vec, Reg, Loc, NewMem)
      ;
        !,
        format(atom(ErrorMsg), 'ERROR: Out of bound access (~w > ~w) to ~w.', [Offset_, Size, Var]),
        NewState = state(ErrorMsg)
      )
  ;
    !,
    format(atom(ErrorMsg), 'ERROR: Attempt to assign undeclared variable ~w.', Var),
    NewState = state(ErrorMsg)
  ).
assign(state(Vec, Reg, Loc, Mem), list(Var1), list(Var2), NewState) :-
  ( get_assoc(Var1, Loc, list(_)) ->
      !,
      ( get_assoc(Var2, Loc, list(Adr2)) ->
          !,
          put_assoc(Var1, Loc, list(Adr2), NewLoc),
          NewState = state(Vec, Reg, NewLoc, Mem)
      ;
          !,
          format(atom(ErrorMsg), 'ERROR: Attempt to access undeclared variable ~w.', Var2),
          NewState = state(ErrorMsg)
      )
  ;
    !,
    format(atom(ErrorMsg), 'ERROR: Attempt to assign undeclared variable ~w.', Var1),
    NewState = state(ErrorMsg)
  ).
assign(state(Vec, Reg, Loc, Mem), listval(Var), Val, NewState) :-
  ( get_assoc(Var, Loc, list(Adr)) ->
      !,
      ( get_assoc(Adr, Mem, (l(_, Nxt), _)) ->
        !,
        show_vector(state(Vec, Reg, Loc, Mem), IterVec),
        put_assoc(Adr, Mem, (l(Val, Nxt), IterVec), NewMem),
        NewState = state(Vec, Reg, Loc, NewMem)
      ;
        !,
        format(atom(ErrorMsg), 'ERROR: Attempt to assign nil list ~w.', Var),
        NewState = state(ErrorMsg)
      )
  ;
    !,
    format(atom(ErrorMsg), 'ERROR: Attempt to assign undeclared variable ~w.', Var),
    NewState = state(ErrorMsg)
  ).

cons(state(ErrorMsg), list(_), _, state(ErrorMsg)).
cons(state(Vec, Reg, Loc, Mem), list(Var), Val, NewState) :-
  ( get_assoc(Var, Loc, list(Adr)) ->
    !,
    show_vector(state(Vec, Reg, Loc, Mem), IterVec),
    put_assoc(Var, Loc, list(Reg), NewLoc),
    put_assoc(Reg, Mem, (l(Val, Adr), IterVec), NewMem),
    NewReg is Reg + 1,
    NewState = state(Vec, NewReg, NewLoc, NewMem)
  ;
    !,
    format(atom(ErrorMsg), 'ERROR: Attempt to cons to an undefined variable ~w.', Var),
    NewState = state(ErrorMsg)
  ).

nxt(state(ErrorMsg), list(_), state(ErrorMsg)).
nxt(state(Vec, Reg, Loc, Mem), list(Var), NewState) :- 
  ( get_assoc(Var, Loc, list(Adr)) ->
    !,
      ( get_assoc(Adr, Mem, (l(_, Nxt), _)) ->
        !,
        put_assoc(Var, Loc, list(Nxt), NewLoc),
        NewState = state(Vec, Reg, NewLoc, Mem)
      ;
        !,
        format(atom(ErrorMsg), 'ERROR: Attempt to assign nil list ~w.', Var),
        NewState = state(ErrorMsg)
      )

  ;
    format(atom(ErrorMsg), 'ERROR: Attempt to assign undeclared variable ~w.', Var),
    NewState = state(ErrorMsg)
  ).


%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%    Test suite
%%  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(1, "Declaration Test (normal variable without iteration vector)", [X, X1]) :-
  init(X),
  declare(X, var('i'), X1).

test(2, "Redeclaration Test (normal variable without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, var('i'), X1),
  declare(X1, var('i'), X2).

test(3, "Assignement Test (normal variable without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, var('i'), X1),
  assign(X1, var('i'), 1, X2).

test(4, "Out of Bound assignement Test (normal variable without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, var('i'), X1),
  assign(X1, var('i', 1), 1, X2).

test(5, "Declaration Test (array variable without iteration vector)", [X, X1]) :-
  init(X),
  declare(X, var('array', 3), X1).

test(6, "Redeclaration Test (array variable without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, var('array', 3), X1),
  declare(X1, var('array'), X2).

test(7, "Assignement Test (array variable without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, var('array', 3), X1),
  assign(X1, var('array'), 1, X2).

test(8, "Fill all cells of an array Test (array variable without iteration vector)", [X3, X4]) :-
  init(X),
  declare(X, var('array', 3), X1),
  assign(X1, var('array', 0), 1, X2),
  assign(X2, var('array', 1), 2, X3),
  assign(X3, var('array', 2), 3, X4).

test(9, "Out of Bound assignement Test (array variable without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, var('array', 3), X1),
  assign(X1, var('array', 3), 1, X2).

test(10, "List Declaration Test (without iteration vector)", [X, X1]) :-
  init(X),
  declare(X, list('list'), X1).

test(11, "List Redeclaration Test (without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, list('list'), X1),
  declare(X1, list('list'), X2).

test(12, "Nil List Assign Test (without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, list('list'), X1),
  assign(X1, list('list'), 1, X2).

test(13, "Cons List (without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, list('list'), X1),
  cons(X1, list('list'), 1, X2).

test(14, "Cons List Assign (without iteration vector)", [X2, X3]) :-
  init(X),
  declare(X, list('list'), X1),
  cons(X1, list('list'), 1, X2),
  assign(X2, list('list'), 50, X3).

test(15, "Cons List Save (without iteration vector)", [X4, X5]) :-
  init(X),
  declare(X, list('list'), X1),
  declare(X1, list('copy'), X2),
  cons(X2, list('list'), 1, X3),
  assign(X3, list('copy'), list('list'), X4),
  cons(X4, list('list'), 4, X5).

test(16, "Cons List Nxt (without iteration vector)", [X3, X4]) :-
  init(X),
  declare(X, list('list'), X1),
  cons(X1, list('list'), 1, X2),
  nxt(X2, list('list'), X3),
  cons(X3, list('list'), 4, X4).

test(17, "Mixed Redeclaration Test 1 (without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, var('list'), X1),
  declare(X1, list('list'), X2).

test(18, "Mixed Redeclaration Test 2 (without iteration vector)", [X1, X2]) :-
  init(X),
  declare(X, list('list'), X1),
  declare(X1, var('list'), X2).

test(19, "Declaration Test (normal variable with iteration vector)", [X7, X10]) :-
  init(X),
  declare(X, var('k0'), X1),
  assign(X1, var('k0'), 50, X2),
  push(X2, var('k0'), X3),
  declare(X3, var('k1'), X4),
  assign(X4, var('k1'), 1, X5),
  push(X5, var('k1'), X6),
  declare(X6, var('i'), X7),
  drop(X7, X8),
  assign(X8, var('k0'), 51, X9),
  declare(X9, var('j'), X10).
