% -*- mode: prolog; coding: utf-8  -*-
% vim: set ft=prolog fileencoding=utf-8 fileformats=unix :

:- module(constructor, [construct/2]).
:- use_module('inference.pl').
:- table (split/2).

%
%!   grammar_to_tree(?Tree, ?Grammar)
%    grammar_to_tree/2 constructs a tree from the grammar.
%
construct(Tree, Grammar) :-
    Tree = node('ROOT', _),
    member(Next, [terminal, split, lift]),
    apply(Next, [Grammar, Tree]).

%
%!   terminal(?Tree, -Grammar)
%    The grammar is the leaf trivially, if tree is a leaf.
%
terminal(Grammar, Tree) :-
    Grammar = [Tree].

%
%!   split(?Tree, -Grammar)
%    split/2 creates two grammars from children of the node, and combine them into one.
%
split(Grammar, Tree) :-
    append([X|Xs], [Y|Ys], Grammar),
    member(Next1, [terminal, split, lift]),
    apply(Next1, [[X|Xs], node(U, A)]),
    member(Next2, [terminal, split, lift]),
    apply(Next2, [[Y|Ys], node(V, B)]),
    infer([U, V], W),
    acyclic_term([U, V, W]),
    Tree = node(W, [node(U, A), node(V, B)]).

%
%!   lift(?Tree, -Grammar)
%    lift/2 lift the category of the tree by category-raising rules.
%
lift(Grammar, Tree) :-
    member(Next, [terminal, split]),
    member(X, ['NN', 'NNS', 'NNP', 'NNPS', 'PRP', 'PRP_S']),
    apply(Next, [Grammar, node(X, A)]),
    member(Y, [category(left, Z, category(right, Z, X)), category(right, Z, category(left, Z, X))]),
    acyclic_term([X, Y, Z]),
    Tree = node(Y, [node(X,A)]).
