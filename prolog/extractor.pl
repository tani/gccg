% -*- mode: prolog; coding: utf-8  -*-
% vim: set ft=prolog fileencoding=utf-8 fileformats=unix :

:- module(extractor, [extract/2]).
:- use_module('inference.pl').
:- table (split/2).
%
%!   extract(?Tree, -Grammar)
%    extract/2 creates a grammar that generates the tree.
%
extract(Tree, Grammar) :-
    tree_to_sentence(Tree, Sentence),
    maplist([X, Y]>>(X=node(_, [Y])), Grammar, Sentence),
    member(Next, [terminal, split, lift]),
    apply(Next, [Tree, Grammar]).

%
%!   tree_to_sentence(?Tree, -Sentence)
%    tree_to_sentence/2 extracts an original sentence from the tree.
%
tree_to_sentence(node(_, [A]), Sentence) :-
    tree_to_sentence(A, Sentence).

tree_to_sentence(node(_, [A,B|Bs]), Sentence) :-
    maplist(tree_to_sentence, [A,B|Bs], Subsentences),
    append(Subsentences, Sentence).

tree_to_sentence(A, [A]) :- atom(A).

%
%!   terminal(?Tree, -Grammar)
%    The grammar is the leaf trivially, if tree is a leaf.
%
terminal(Tree, Grammar) :-
    Grammar = [Tree].

%
%!   split(?Tree, -Grammar)
%    split/2 creates two grammars from children of the node, and combine them into one.
%
split(Tree, Grammar) :-
    Tree = node(W, [node(U, A), node(V, B)]),
    infer([U, V], W),
    acyclic_term([U, V, W]),
    append([X|Xs], [Y|Ys], Grammar),
    member(Next1, [terminal, split, lift]),
    apply(Next1, [node(U, A), [X|Xs]]),
    member(Next2, [terminal, split, lift]),
    apply(Next2, [node(V, B), [Y|Ys]]).


%
%!   lift(?Tree, -Grammar)
%    lift/2 lift the category of the tree by category-raising rules.
%
%
lift(Tree, Grammar) :-
    Tree = node(Y, [node(X, A)]),
    member(X, ['NN', 'NNS', 'NNP', 'NNPS', 'PRP', 'PRP_S']),
    member(Y, [X, category(right, Z, category(left, Z, X)), category(left, Z, category(right, Z, X))]),
    acyclic_term([X, Y, Z]),
    member(Next, [terminal, split]),
    apply(Next, [node(X, A), Grammar]).

lift(Tree, Grammar) :-
    Tree = node(X, [node(X, A)]),
    acyclic_term(X),
    member(Next, [terminal, split]),
    apply(Next, [node(X, A), Grammar]).
