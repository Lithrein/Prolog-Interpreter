:- module(exec, [
      exec/2
    ]).

:- use_module(annotation).
:- use_module(utils).
:- use_module(state).
:- use_module(eval).

exec(Prog, States) :-
  annot(Prog, AnnotedProg),
  state:init(InitState),!,
  interp(InitState, AnnotedProg, States).

interp(S, [], S).
interp(State, [start, declare(intvar(Name)), assign(intvar(Name), Val) | Tail], NewState) :- !,
  state:declare(State, var(Name), S1),
  state:assign(S1, var(Name), Val, S2),
  state:push(S2, var(Name), S3),
  interp(S3, Tail, NewState).
interp(S1, [end | Tail], S3) :- !, state:drop(S1, S2), interp(S2, Tail, S3).

interp(State, [declare(intvar(Name)), assign(intvar(Name), Val), while(Cond, Do), undeclare(intvar(Name)) | Tail], NewState) :- !,
  state:declare(State, var(Name), State1), 
  state:assign(State1, var(Name), Val, State2),
  state:push(State2, var(Name), State3),
  interp(State3, [while(Cond, Do) | Tail], State4),
  state:undeclare(State4, var(Name), State5),
  state:drop(State5, NewState).
interp(State, [declare(intvar(Name)), if(Cond, Then, Else) | Tail], NewState) :- !,
  state:declare(State, var(Name), State1), 
  state:push(State1, Name, State2),
  interp(State2, [if(Cond, Then, Else) | Tail], State3),
  state:drop(State3, NewState).

interp(State, [declare(var(Name)) | Tail], NewState) :- !,
  state:declare(State, var(Name), State1),
  interp(State1, Tail, NewState).
interp(State, [declare(intvar(Name)) | Tail], NewState) :- !,
  state:declare(State, var(Name), State1),
  interp(State1, Tail, NewState).
interp(State, [undeclare(intvar(Name)) | Tail], NewState) :- !,
  state:undeclare(State, var(Name), State1),
  interp(State1, Tail, NewState).
interp(State, [declare(var(Name, Size)) | Tail], NewState) :- !,
  state:declare(State, var(Name, Size), State1),
  interp(State1, Tail, NewState).
interp(State, [declare(list(Name)) | Tail], NewState) :- !,
  state:declare(State, list(Name), State1),
  interp(State1, Tail, NewState).
interp(State, [assign(var(Name, Idx), Val) | Tail], NewState) :- !,
  state:assign(State, var(Name, Idx), Val, State1),
  interp(State1, Tail, NewState).
interp(State, [assign(var(Name), Val) | Tail], NewState) :- !,
  state:assign(State, var(Name), Val, State1),
  interp(State1, Tail, NewState).
interp(State, [assign(intvar(Name), Val) | Tail], NewState) :- !,
  state:assign(State, var(Name), Val, State1),
  interp(State1, Tail, NewState).
interp(State, [assign(listval(Name), Val) | Tail], NewState) :- !,
  state:assign(State, listval(Name), Val, State1),
  interp(State1, Tail, NewState).
interp(State, [assign(list(L1), list(L2)) | Tail], NewState) :- !,
  state:assign(State, list(L1), list(L2), State1),
  interp(State1, Tail, NewState).
interp(State, [cons(Val, list(Var)) | Tail], NewState) :- !,
  state:cons(State, list(Var), Val, State1),
  interp(State1, Tail, NewState).
interp(State, [nxt(list(Var)) | Tail], NewState) :- !,
  state:nxt(State, list(Var), State1),
  interp(State1, Tail, NewState).
interp(State, [if(Cond, Then, Else) | Tail], NewState) :- !,
  (eval:bool(State, Cond) -> 
    interp(State, Then, State1)
  ;
    interp(State, Else, State1)
  ),
  interp(State1, Tail, NewState).
interp(State, [while(Cond, Do) | Tail], NewState) :- !,
  (eval:bool(State, Cond) ->
    interp(State, Do, State1),
    interp(State1, [while(Cond, Do)], State2)
  ; 
    State2 = State  
  ),
  interp(State2, Tail, NewState).

%%%%%%%%%%%%%%

test(1, "", [
    start,
    declare(intvar('i')),
    declare(intvar('j')),
    declare(var('tab', 4)),
    assign(intvar('i'), 0),
    while(leq(intvar('i'), 1), [ 
      assign(intvar('j'), 0),
      while(leq(intvar('j'), 1), [
        assign(var('tab', (2 * intvar('i') + intvar('j'))), truc),
        assign(intvar('j'), intvar('j') + 1)
      ]),
    assign(intvar('i'), intvar('i') + 1)
    ]),
    end
  ]).
