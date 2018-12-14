%This file implements the origami axioms
%as found at https://en.wikipedia.org/wiki/Huzita%E2%80%93Hatori_axioms

:- use_module(library(ic)).

papersize_(10, 10). %for prototyping
papersize(X,Y) :-
    papersize_(X, Y),
    X #>= 0,
    Y #>= 0.

paper_x(X) :- papersize(X, _).
paper_y(Y) :- papersize(_, Y).

paper_square :- papersize(X, X).
paper_rect :-
    papersize(X, Y),
    X #\= Y.

%X & Y must have a linear relationship & be in bounds
%coordinate system has origin in left lower corner
vertex(X, Y) :-
    (vertex_Nbound(X, Y); vertex_Sbound(X, Y); vertex_Ebound(X, Y); vertex_Wbound(X, Y);
    vertex_NW(X, Y); vertex_NE(X, Y); vertex_SW(X, Y); vertex_SE(X, Y);
    vertex_np(X, Y); vertex_normal(X, Y)),
    !.
%standard vertex
vertex_np(X, Y) :-
    papersize(Px, Py),
    X $:: 0.0..Px,
    Y $:: 0.0..Py.
%paper edges
vertex_Nbound(X, Y) :- vertex_np(X, Y), paper_y(Y).
vertex_Sbound(X, Y) :- vertex_np(X, Y), Y $= 0.
vertex_Ebound(X, Y) :- vertex_np(X, Y), paper_x(X).
vertex_Wbound(X, Y) :- vertex_np(X, Y), X $= 0.
%paper corners
vertex_NW(X, Y) :- vertex_Nbound(X, Y), vertex_Wbound(X, Y), !.
vertex_NE(X, Y) :- vertex_Nbound(X, Y), vertex_Ebound(X, Y), !.
vertex_SW(X, Y) :- vertex_Sbound(X, Y), vertex_Wbound(X, Y), !.
vertex_SE(X, Y) :- vertex_Sbound(X, Y), vertex_Ebound(X, Y), !.
%used to compute, can be out-of-bounds
vertex_normal(X, Y) :-
    papersize(Px, Py),
    X $:: -Px..Px,
    Y $:: -Py..Py.

vertex_equal(V1, V2) :-
    V1  = vertex(X1, Y1),
    V2  = vertex(X2, Y2),
    X1 $= X2,
    Y1 $= Y2.

%axiom 1 in parametric form (functional, is not relational)
axiom1(V1, V2, Param, Img) :-
    V1 = vertex(V1x, V1y),
    V2 = vertex(V2x, V2y),
    Img = vertex(Ix, Iy),
    Ix $= V1x + (Param * (V2x - V1x)),
    Iy $= V1y + (Param * (V2y - V1y)),
    \+ vertex_equal(V1, V2).

%axiom 2 (not relational since depends on axiom 1)
axiom2(V1, V2, Param, Img) :-
    V1 = vertex(V1x, V1y),
    Img = vertex(Ix, Iy),
    Mp = vertex(Mpx, Mpy),
    axiom1(V1, V2, 1/2, Mp),
    Ix $= Mpx + (Param * (V1y - Mpy)),
    Iy $= Mpy + (Param * (Mpx - V1x)).

%axiom 3











