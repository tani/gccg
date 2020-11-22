% -*- mode: prolog; coding: utf-8  -*-
% vim: set ft=prolog fileencoding=utf-8 fileformats=unix :

:- module(converter, [variable_to_term/1, term_to_variable/2, dict_node/2]).

replace(Subterm0, Subterm, Term0, Term) :-
    (   Term0 == Subterm0 -> Term = Subterm
    ;   var(Term0) -> Term = Term0
    ;   Term0 =.. [F|Args0],
        maplist(replace(Subterm0,Subterm), Args0, Args),
        Term =.. [F|Args]
    ).

variable_to_term(X) :-
    term_variables(X, Vars),
    length(Vars, N),
    findall(I, between(1, N, I), Indices),
    maplist(atom_concat('x_'), Indices, Atoms),
    Vars = Atoms.

xre_replace(Pattern, With, Term0, Term) :-
    term_string(Term0, Str0),
    re_replace(Pattern, With, Str0, Str),
    term_string(Term, Str).

term_to_variable(RawTree, Tree) :-
    xre_replace("@"/g, "_", RawTree, RawTree2),
    extract_categories(RawTree2, Labels),
    exclude([X]>>member(X, ['NN', 'NNS', 'NNP', 'NNPS', 'PRP', 'PRP_', 'ROOT', '.']), Labels, Filtered),
    exclude(compound, Filtered, Atomic),
    maplist(term_string, Placeholders, Atomic),
    foldl(replace, Atomic, Placeholders, RawTree2, Tree).

extract_categories(List, Categories) :-
    is_list(List), !,
    maplist(extract_categories, List, ListOfCategories),
    append(ListOfCategories, Categories).

extract_categories(node(Category, Children), Categories) :- !,
    maplist(extract_categories, Children, ListOfCategories),
    append(ListOfCategories, UnsortedCategories),
    sort([Category|UnsortedCategories], Categories).

extract_categories(_, []) :- !.

dict_node(ListOfDict, ListOfNode) :-
    (is_list(ListOfDict); is_list(ListOfNode)), !,
    maplist(dict_node, ListOfDict, ListOfNode).

dict_node(Dict, Node) :-
    Dict = _{label: Category0, children: Children0},
    Node = node(Category, Children), !,
    dict_category(Category0, Category),
    dict_node(Children0, Children).

dict_node(Dict, Node) :-
    Dict = _{label: Category0},
    Node = Category0, !.

dict_category(Dict, Category) :-
    Dict = _{label: Direction, children: [Codomain0, Domain0]},
    Category = category(Direction, Codomain, Domain), !,
    dict_category(Codomain0, Codomain),
    dict_category(Domain0, Domain).

dict_category(Dict, Category) :-
    Dict = _{label: Category}, !.

