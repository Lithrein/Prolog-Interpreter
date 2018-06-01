:- module(dot, [
      exec/3,
      dependencies/2
    ]).

:- use_module(annotation).
:- use_module(utils).
:- use_module(state).
:- use_module(eval).

exec(Prog, Opes, States) :-
  annot(Prog, AnnotedProg),
  state:init(InitState),!,
  interp(InitState, AnnotedProg, Opes, States).

interp(S, [], [], S).
interp(State,
      [start, declare(intvar(Name)), assign(intvar(Name), Val) | Tail],
      Opes, 
      NewState) :- !,
  state:declare(State, var(Name), S1),
  state:assign(S1, var(Name), Val, S2),
  state:push(S2, var(Name), S3),
  interp(S3, Tail, Opes, NewState).

interp(S1, [end | Tail], Opes, S3) :- !,
  state:drop(S1, S2),
  interp(S2, Tail, Opes, S3).

interp(State,
      [ % Start by declaring the variable that will count the number of statements
        % in the loop
       declare(intvar(Name)), assign(intvar(Name), Val),
        % exec the loop
       while(Cond, Do),
        % and get rid of the variable counter.
       undeclare(intvar(Name))
      | Tail],
        % No meaningful Operations here, just an update of the iteration vector
        % before entering the loop.
      Opes,
      NewState) :- !,
  state:declare(State, var(Name), State1), 
  state:assign(State1, var(Name), Val, State2),
  state:push(State2, var(Name), State3),
  interp(State3, [while(Cond, Do) | Tail], Opes, State4),!,
  state:undeclare(State4, var(Name), State5),
  state:drop(State5, NewState).

% Probably buggy, check later because unused for now.
interp(State, [declare(intvar(Name)), if(Cond, Then, Else) | Tail], Opes, NewState) :- !,
  state:declare(State, var(Name), State1), 
  state:push(State1, Name, State2),
  interp(State2, [if(Cond, Then, Else) | Tail], Opes, State3),
  state:drop(State3, NewState).

% Of no importance, don't keep a record of those declaration Operations
interp(State, [declare(var(Name)) | Tail], Opes, NewState) :- !,
  state:declare(State, var(Name), State1),
  interp(State1, Tail, Opes, NewState).
interp(State, [declare(intvar(Name)) | Tail], Opes, NewState) :- !,
  state:declare(State, var(Name), State1),
  interp(State1, Tail, Opes, NewState).
interp(State, [undeclare(intvar(Name)) | Tail], Opes, NewState) :- !,
  state:undeclare(State, var(Name), State1),
  interp(State1, Tail, Opes, NewState).
interp(State, [declare(var(Name, Size)) | Tail], Opes, NewState) :- !,
  state:declare(State, var(Name, Size), State1),
  interp(State1, Tail, Opes, NewState).
interp(State, [declare(list(Name)) | Tail], Opes, NewState) :- !,
  state:declare(State, list(Name), State1),
  interp(State1, Tail, Opes, NewState).

% The assign Operations are important because there is both read and written
% variables !
interp(State, [assign(var(Name, Idx), Val) | Tail], [Ope | Opes], NewState) :- !,
  state:assign(State, var(Name, Idx), Val, State1),
  annot:operation_format(State, assign(var(Name, Idx), Val), Ope), 
  interp(State1, Tail, Opes, NewState).
interp(State, [assign(var(Name), Val) | Tail], [Ope | Opes], NewState) :- !,
  state:assign(State, var(Name), Val, State1),
  annot:operation_format(State, assign(var(Name), Val), Ope),
  interp(State1, Tail, Opes, NewState).
interp(State, [assign(intvar(Name), Val) | Tail], Opes, NewState) :- !,
  state:assign(State, var(Name), Val, State1),
  %annot:operation_format(State, assign(intvar(Name), Val), Ope),
  interp(State1, Tail, Opes, NewState).
interp(State, [assign(listval(Name), Val) | Tail], Opes, NewState) :- !,
  state:assign(State, listval(Name), Val, State1),
  interp(State1, Tail, Opes, NewState).
interp(State, [assign(list(L1), list(L2)) | Tail], Opes, NewState) :- !,
  state:assign(State, list(L1), list(L2), State1),
  interp(State1, Tail, Opes, NewState).
interp(State, [cons(Val, list(Var)) | Tail], Opes, NewState) :- !,
  state:cons(State, list(Var), Val, State1),
  interp(State1, Tail, Opes, NewState).
interp(State, [nxt(list(Var)) | Tail], Opes, NewState) :- !,
  state:nxt(State, list(Var), State1),
  interp(State1, Tail, Opes, NewState).

% The Operation in the two following cases is a pure reading condition.
interp(State, [if(Cond, Then, Else) | Tail], [Ope | Opes], NewState) :- !,
  annot:operation_format(State, cond(Cond), Ope),
  (eval:bool(State, Cond) -> 
    interp(State, Then, State1)
  ;
    interp(State, Else, State1)
  ),
  interp(State1, Tail, Opes, NewState).
interp(State, [while(Cond, Do) | Tail], Opes, NewState) :- !,
  annot:operation_format(State, cond(Cond), Ope_),
  (eval:bool(State, Cond) ->
    interp(State, Do, Ope__, State1),!,
    interp(State1, [while(Cond, Do)], Ope___, State2),
    append([Ope_ | Ope__], Ope___, Ope),
    interp(State2, Tail, Opes_, NewState),
    append(Ope, Opes_, Opes)
  ; 
    interp(State, Tail, Opes, NewState)
  ).

%%%%%%%%%%%%%%

compare_list([], [], Res) :- !, Res = '='.
compare_list(_, [], '>').
compare_list([], _, '<').
compare_list([X | XS], [Y | YS], Res) :-
  ( X == Y ->
    compare_list(XS, YS, Res) 
  ;
    ( X > Y ->
      Res = '>'
    ;
      Res = '<'
    )
  ).

max_vect(Vec1, Vec2, Max) :-
  compare_list(Vec1, Vec2, Cond),
  ( Cond == '=' -> 
    Max = Vec1
  ; ( Cond == '>' ->
      Max = Vec1
    ;
      Max = Vec2
    )
  ).

reverse_list([], []).
reverse_list([X | XS], Rev) :-
  reverse_list(XS, XS_rev), append(XS_rev, [X], Rev).

max_vects([], Res) :- !, Res = [].
max_vects([Vec], Res) :- !, Res = Vec.
max_vects([Vec | Vecs], Max) :-
  max_vects(Vecs, Max_), !,
  max_vect(Vec, Max_, Max).

iter_vect_of([], _, []).
iter_vect_of([((Wvars, Rvars), Vec) | Opes], Var, IterVec) :-
  append(Wvars, Rvars, Vars),
  ( member(Var, Vars) ->
      IterVec = Vec
  ;
      iter_vect_of(Opes, Var, IterVec)
  ),!.

get_vector_instances_from_variables(Opes, [Var | Vars], [IterVec | IterVects]) :-
  iter_vect_of(Opes, Var, IterVec), !,
  get_vector_instances_from_variables(Opes, Vars, IterVects), !.
get_vector_instances_from_variables(_, _, []).

% Take List of states
% List of dependencies

dependencies(Opes, Deps) :-
  reverse(Opes, Opes_),
  dependencies_(Opes_, Deps).
dependencies_([],[]).
dependencies_([((Wvars, Rvars), Vec) | Opes], [Dep | Deps]) :-
  append(Wvars, Rvars, Vars),
  get_vector_instances_from_variables(Opes, Vars, IterVects),
  max_vects(IterVects, Dep_),
  Dep = [Vec, Dep_],
  dependencies_(Opes, Deps).

print_deps([]).
print_deps([[A, B] | Deps]) :-
  format(atom(Res), "\"~w\" -> \"~w\";", [A, B]),
  writeln(Res),
  print_deps(Deps).

format_deps([], "").
format_deps([[A, B] | Deps], Res) :- 
  format(atom(Res1), '"~w" -> "~w";\n', [A, B]), 
  format_deps(Deps, Res2),
  concat(Res1, Res2, Res).

save_graph_to(Graphname, Deps) :-
  format_deps(Deps, Graph),
  format(atom(Filename), '~w.dot', Graphname),
  format(atom(Content), 'digraph ~w {\n ~w } \n', [Graphname, Graph]),
  writeln(Filename),
  open(Filename, append, F),
  write(F, Content),
  close(F).
  
%%%%%%%%%%%%%%

test(1, "", [
    start,
    declare(intvar('i')),
    declare(intvar('j')),
    declare(intvar('k')),
    declare(var('A', 16)),
    declare(var('B', 16)),
    declare(var('C', 16)),
    assign(intvar('i'), 0),
    while(leq(intvar('i'), 3), [ 
      assign(intvar('j'), 0),
      while(leq(intvar('j'), 3), [
        assign(var('A', (4 * intvar('i') + intvar('j'))), trefle),
        assign(intvar('j'), intvar('j') + 1)
      ]),
    assign(intvar('i'), intvar('i') + 1)
    ]),
    assign(intvar('i'), 0),
    while(leq(intvar('i'), 3), [ 
      assign(intvar('j'), 0),
      while(leq(intvar('j'), 3), [
        assign(var('A', (4 * intvar('i') + intvar('j'))), trefle),
        assign(intvar('j'), intvar('j') + 1)
      ]),
    assign(intvar('i'), intvar('i') + 1)
    ]),
    assign(intvar('i'), 0),
    while(leq(intvar('i'), 3), [ 
      assign(intvar('j'), 0),
      while(leq(intvar('j'), 3), [
        assign(var('C', (4 * intvar('i') + intvar('j'))), trefle),
        assign(intvar('k'), 0),
        while(leq(intvar('k'), 3), [
          assign(var('C', (4 * intvar('i') + intvar('j'))), o(var('C', (4 * intvar('i') + intvar('j'))), o(var('A', (4 * intvar('i') + intvar('k'))), var('B', (4 * intvar('k') + intvar('j')))))),
          assign(intvar('k'), intvar('k') + 1)
        ]),
        assign(intvar('j'), intvar('j') + 1)
      ]),
    assign(intvar('i'), intvar('i') + 1)
    ]),
    end
  ]).
