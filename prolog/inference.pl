% -*- mode: prolog; coding: utf-8  -*-
% vim: set ft=prolog fileencoding=utf-8 fileformats=unix :

%
%!  infer(+categories, +category)
%   infer/2 infers the category from the categories by CCG rules.
%

:-module(inference, [infer/2]).

infer([X], Y) :-
    member(Y, [category(right, Z,category(left, Z, X)), category(left, Z, category(right, Z,X)), X]).

infer([category(right, X,Y), Y], X).

infer([X, category(left, Y,X)], Y).

infer([category(right, X,Y), category(right, Y,Z)], category(right, X,Z)).

infer([category(left, X,Y), category(left, Y,Z)], category(left, X,Z)).

infer(categories, X) :-
    length(categories, N), N > 2,
    append([U|Us], [V|Vs], categories),
    maplist(infer, [[U|Us], [V|Vs]], [Y, Z]),
    infer([Y, Z], X).
