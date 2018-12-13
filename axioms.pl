%This file implements the origami axioms
%as found at https://en.wikipedia.org/wiki/Huzita%E2%80%93Hatori_axioms

:- use_module(library(clpq)).

papersize_(10, 10). %for prototyping
papersize(X,Y) :-
    papersize_(X, Y),
    {X >= 0},
    {Y >= 0}.

paper_x(X) :- papersize(X, _).
paper_y(Y) :- papersize(_, Y).

paper_square :- papersize(X, X).
paper_rect :-
    papersize(X, Y),
    {X =\= Y}.

%X & Y must have a linear relationship & be in bounds
%coordinate system has origin in left lower corner
vertex(X, Y) :-
    (vertex_Nbound(X, Y); vertex_Sbound(X, Y); vertex_Ebound(X, Y); vertex_Wbound(X, Y);
    vertex_NW(X, Y); vertex_NE(X, Y); vertex_SW(X, Y); vertex_SE(X, Y);
    vertex_np(X, Y)), !.
%standard vertex
vertex_np(X, Y) :-
    (papersize(Px, Py),
    {X =< Px},
    {Y =< Py},
    {X >= 0},
    {Y >= 0},
    {_ * X + _ =:= Y}).
%paper edges
vertex_Nbound(X, Y) :- vertex_np(X, Y), paper_y(Y).
vertex_Sbound(X, Y) :- vertex_np(X, Y), {Y =:= 0}.
vertex_Ebound(X, Y) :- vertex_np(X, Y), paper_x(X).
vertex_Wbound(X, Y) :- vertex_np(X, Y), {X =:= 0}.
%paper corners
vertex_NW(X, Y) :- vertex_Nbound(X, Y), vertex_Wbound(X, Y), !.
vertex_NE(X, Y) :- vertex_Nbound(X, Y), vertex_Ebound(X, Y), !.
vertex_SW(X, Y) :- vertex_Sbound(X, Y), vertex_Wbound(X, Y), !.
vertex_SE(X, Y) :- vertex_Sbound(X, Y), vertex_Ebound(X, Y), !.

equal(V1, V2) :-
    V1   = vertex(X1, Y1),
    V2   = vertex(X2, Y2),
    {X1 =:= X2},
    {Y1 =:= Y2}.


%implements axiom 1
crease(V1,V2, S, O) :-
    call(V1),
    call(V2),
    arg(1, V1, X1),
    arg(2, V1, Y1),
    arg(1, V2, X2),
    arg(2, V2, Y2),
    ((equal(V1, V2) -> false);
    crease_std(X1, Y1, X2, Y2, S, O);
    crease_vertical(X1, Y1, X2, Y2, S, O);
    crease_horizontal(X1, Y1, X2, Y2, S, O)), !.
crease_vertical(X1, Y1, X2, Y2, S, O) :-
    S is inf,
    O is nan,
    {X1 =:= X2},
    {Y1 =\= Y2}.
crease_horizontal(X1, Y1, X2, Y2, S, O) :-
    {S =:= 0},
    {O =:= Y1},
    {X1 =\= X2},
    {Y1 =:= Y2}.
crease_std(X1, Y1, X2, Y2, S, O) :-
    {Y1 =:= S * X1 + O},
    {Y2 =:= S * X2 + O}.

%axiom 2


























