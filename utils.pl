:- module(utils, [ ]).

zip([], [], []).
zip([X | XS], [Y | YS], [(X, Y) | ZS]) :- zip(XS, YS, ZS).

zip([], [], [], []).
zip([X | XS], [Y | YS], [Z | ZS], [(X, Y, Z) | WS]) :- zip(XS, YS, ZS, WS). 

filter(_, [], []) :- !.
filter(Cond, [X|XS], YS) :-
    ( call(Cond, X) ->
        filter(Cond, XS, ZS),
    	append([X], ZS, YS)
    ;
        filter(Cond, XS, YS)
    ).

memberchk_flip(List, Elem) :- memberchk(Elem, List).

