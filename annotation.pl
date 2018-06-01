:- module(annot, [
    annot/2,
    pretty_print/1,
    pretty_print/2,
    operation_format/3
   ]).

:- use_module(utils).

annot(Prog, Res) :- annot_(0, [], Prog, Res, _).

annot_(X, _, [], Res, X) :- !, Res = [].
annot_(Fresh, Vec, [start | Tail], [start, declare(intvar(Iter)), assign(intvar(Iter), 0) | Tail_], NewFresh) :- !,
  Fresh1 is Fresh + 1,
  string_concat('k', Fresh, Iter),
  annot_(Fresh1, [Fresh | Vec], Tail, Tail_, NewFresh).
annot_(Fresh, [_ | Tl], [end | Tail], [end | Tail_], Fresh) :- !,
  annot_(Fresh, Tl, Tail, Tail_, Fresh).
annot_(Fresh, [Hd | Tl], [if(Cond, Then, Else) | Tail],
    [declare(intvar(NewIter)),
     if(Cond, [assign(intvar(NewIter), NbCmds) | Then_],
              [assign(intvar(NewIter), 0) | Else_]),
     undeclare(intvar(NewIter)), assign(intvar(Iter), intvar(Iter) + 1)
    | Tail_], NewFresh) :- !,
  Fresh1 is Fresh + 1,
  string_concat('k', Hd, Iter),
  string_concat('k', Fresh, NewIter),
  utils:length(Then, NbCmds_), NbCmds is -NbCmds_,
  annot_(Fresh1, [Fresh | [Hd | Tl]], Then, Then_, Fresh2),
  annot_(Fresh2, [Fresh | [Hd | Tl]], Else, Else_, Fresh3),
  annot_(Fresh3, [Hd | Tl], Tail, Tail_, NewFresh).
annot_(Fresh, [Hd | Tl], [while(Cond, Do) | Tail],
              [declare(intvar(NewIter)), assign(intvar(NewIter), 0),
               while(Cond, Do_), undeclare(intvar(NewIter)),
               assign(intvar(Iter), intvar(Iter) + 1) | Tail_], NewFresh) :- !,
  Fresh1 is Fresh + 1,
  string_concat('k', Hd, Iter),
  string_concat('k', Fresh, NewIter),
  annot_(Fresh1, [Fresh | [Hd | Tl]], Do, Do_, Fresh2),
  annot_(Fresh2, [Hd | Tl], Tail, Tail_, NewFresh).
annot_(Fresh, [Hd | Tl], [Cmd | Tail], [ Cmd, assign(intvar(Iter), intvar(Iter) + 1) | Tail_], NewFresh) :-
  string_concat('k', Hd, Iter),
  annot_(Fresh, [Hd | Tl], Tail, Tail_, NewFresh).


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Pretty printing
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pretty_print_(_, [], Res) :- !, Res = "".
pretty_print_(Offset, [declare(var(Name)) | Tail], Res) :- !,
    string_concat(Offset, 'declare var ~w\n', Fmt),
    format(atom(Res1), Fmt, [Name]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [declare(intvar(Name)) | Tail], Res) :- !,
    string_concat(Offset, 'declare intvar ~w\n', Fmt),
    format(atom(Res1), Fmt, [Name]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [undeclare(intvar(Name)) | Tail], Res) :- !,
    string_concat(Offset, 'undeclare intvar ~w\n', Fmt),
    format(atom(Res1), Fmt, [Name]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [declare(var(Name, Size)) | Tail], Res) :- !,
    string_concat(Offset, 'declare array ~w[~w]\n', Fmt),
    format(atom(Res1), Fmt, [Name, Size]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [declare(list(Name)) | Tail], Res) :- !,
    string_concat(Offset, 'declare list ~w\n', Fmt),
    format(atom(Res1), Fmt, [Name]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [assign(var(Name, Idx), Val) | Tail], Res) :- !,
    string_concat(Offset, '~w[~w] := ~w\n', Fmt),
    format(atom(Res1), Fmt, [Name, Idx, Val]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [assign(var(Name), Val) | Tail], Res) :- !,
    string_concat(Offset, 'var ~w := ~w\n', Fmt),
    format(atom(Res1), Fmt, [Name, Val]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [assign(intvar(Name), Val) | Tail], Res) :- !,
    string_concat(Offset, 'intvar ~w := ~w\n', Fmt),
    format(atom(Res1), Fmt, [Name, Val]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [assign(list(Name), Val) | Tail], Res) :- !,
    string_concat(Offset, 'head of ~w := ~w\n', Fmt),
    format(atom(Res1), Fmt, [Name, Val]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [assign(list(L1), list(L2)) | Tail], Res) :- !,
    string_concat(Offset, 'save position of ~w\'s head in ~w', Fmt),
    format(atom(Res1), Fmt, [L1, L2]),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [start | Tail], Res) :- !, 
    string_concat(Offset, 'Program Start\n', Res1),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [end | Tail], Res) :- !,
    string_concat(Offset, 'Program End', Res1),
    pretty_print_(Offset, Tail, Res2),
    string_concat(Res1, Res2, Res).
pretty_print_(Offset, [if(Cond, Then, Else) | Tail], Res) :- !,
    string_concat('  ', Offset, NewOffset),
    string_concat(Offset, 'if (~w):\n', Fmt),
    format(atom(Res1), Fmt, [Cond]),
    pretty_print_(NewOffset, Then, Res2),
    string_concat(Res1, Res2, Res3),
    string_concat(Offset, 'else:\n', Res4),
    pretty_print_(NewOffset, Else, Res5),
    string_concat(Res4, Res5, Res6),
    string_concat(Res3, Res6, Res7),
    pretty_print_(Offset, Tail, Res8),
    string_concat(Res7, Res8, Res).
pretty_print_(Offset, [while(Cond, Do) | Tail], Res) :- !,
    string_concat('  ', Offset, NewOffset),
    string_concat(Offset, 'while (~w):\n', Fmt),
    format(atom(Res1), Fmt, [Cond]),
    pretty_print_(NewOffset, Do, Res2),
    string_concat(Res1, Res2, Res3),
    pretty_print_(Offset, Tail, Res4),
    string_concat(Res3, Res4, Res).
pretty_print_(Offset, [cons(Val, list(Var)) | Tail], Res) :- !,
  string_concat(Offset, 'cons(~w, ~w)\n', Fmt),
  format(atom(Res1), Fmt, [Val, Var]),
  pretty_print_(Offset, Tail, Res2),
  string_concat(Res1, Res2, Res).
pretty_print_(Offset, [nxt(Var) | Tail], Res) :- !,
  string_concat(Offset, 'nxt(~w)\n', Fmt),
  format(atom(Res1), Fmt, [Var]),
  pretty_print_(Offset, Tail, Res2),
  string_concat(Res1, Res2, Res).

pretty_print(Prog, Res) :- pretty_print_('', Prog, Res).
pretty_print(Prog) :- 
  pretty_print(Prog, Res),
  writeln(Res).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Pretty printing with eval
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_vector_instance(state(Vec, _, Loc, Mem), Inst) :-
  state:show_vector_(state(Vec, _, Loc, Mem), [_, Inst]).

operation_format(State, cond(Cond), Res) :- !,
  eval:vars(State, Cond, Cond_), !,
  eval:instanciate_vars(State, Cond_, Cond__), !,
  get_vector_instance(State, Vector), !,
  Res = (([], Cond__), Vector).
operation_format(State, assign(var(Name, Idx), Val), Res) :- !,
  eval:arith(State, Idx, Idx_),!,
  eval:vars(State, Val, Val_), !,
  eval:instanciate_vars(State, Val_, Val__), !,
  get_vector_instance(State, Vector), !,
  Res = (([var(Name, Idx_)], Val__), Vector).
operation_format(State, assign(var(Name), Val), Res) :- !,
  eval:vars(State, Val, Val_), !,
  eval:instanciate_vars(State, Val_, Val__),!,
  get_vector_instance(State, Vector), !,
  Res = (([var(Name)], Val__), Vector).
operation_format(State, assign(intvar(Name), Val), Res) :- !,
  eval:vars(State, Val, Val_), !,
  eval:instanciate_vars(State, Val_, Val__),!,
  get_vector_instance(State, Vector), !,
  Res = (([intvar(Name)], Val__), Vector).
% todo
% operation_format(State, assign(listval(Name), Val), Res) :- !,
%  eval:vars(State, Val, Val_), !,
%  eval:instanciate_vars(State, Val_, Val__),
%  Res = (listval(Name), Val__).
%operation_format(State, assign(list(L1), list(L2)), Res) :- !,
%  format(atom(Res), 'save position of ~w\'s head in ~w', [L1, L2]).
%operation_format(State, cons(Val, list(Var)), Res) :- !,
%  format(atom(Res), 'cons(~w, ~w)', [Val, Var]).
%operation_format(State, nxt(Var), Res) :- !,
%  format(atom(Res), 'nxt(~w)', [Var]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%
%%  Pretty Printing Tests
%% %%%%%%%%%%%%%%%%%%%%%%%%%

test(1, "2D Matrix Fill") :-
  pretty_print([
    start,
    declare(intvar('i')),
    declare(intvar('j')),
    declare(var('tab', 10)),
    assign(intvar('i'), 0),
    while(intvar('i') < 10, [
      while(intvar('j') < 10, [
        assign(var('tab', (intvar('i'), intvar('j'))), intvar('i'))
      ])
    ]),
    end
  ]).

test(2, "Syracuse Algorithm") :-
    pretty_print([
      start,
      declare(intvar('i')),
      assign(intvar('i'), random_number),
      while('intvar(i) <> 1', [
        if('intvar(i) mod 2 == 0', [
            assign(intvar('i'), intvar('i') / 2)
        ],
        [
            assign(intvar('i'), 3*intvar('i') + 1)
        ])
      ]),
      end
    ]).

test(3, "Suffixes") :-
  pretty_print([
    start,
    declare(list('res')),
    declare(list('acc')),
    declare(list('model')),
    declare(list('tmp')),
    cons(1, list('model')), 
    cons(2, list('model')), 
    cons(3, list('model')), 
    while(notempty(list('model')), [
      assign(list('acc'), list('model')),
      while(notempty(list('acc')), [
        cons(list('model'), list('acc')),
        nxt(list('acc'))
      ]),
      cons(listval('model'), list('tmp')),
      cons(list('tmp'), list('acc')),
      nxt(list('tmp')),
      nxt(list('model'))
    ]),
    end
  ]).

%%
%%
%%

test(4, "") :-
  annot([
    start,
    declare(intvar('i')),
    declare(intvar('j')),
    declare(var('tab', 10)),
    assign(intvar('i'), 0),
    while(intvar('i') < 10, [
      while(intvar('j') < 10, [
        assign(var('res', (intvar('i'), intvar('j'))), intvar('i'))
      ])
    ]),
    end
  ],
  Res), pretty_print(Res).

test(5, "Syracuse Algorithm (Annoted)") :-
    annot([
      start,
      declare(intvar('i')),
      assign(intvar('i'), random_number),
      while('intvar(i) <> 1', [
        if('intvar(i) mod 2 == 0', [
            assign(intvar('i'), intvar('i') / 2)
        ],
        [
            assign(intvar('i'), 3*intvar('i') + 1)
        ])
      ]),
      end
    ], Res), pretty_print(Res).

test(6, "Two whiles") :-
  annot([
    start,
      declare(intvar('i')),
      while('intvar(i) < 10', [
        assign(intvar('i'), intvar('i') + 1)
      ]),
      while('intvar(i) < 20', [
        assign(intvar('i'), intvar('i') + 1)
      ]),
      end
    ], Res), pretty_print(Res).
        
