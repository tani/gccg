#!/usr/bin/env swipl
% -*- mode: prolog; coding: utf-8  -*-
% vim: set ft=prolog fileencoding=utf-8 fileformats=unix :

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(extractor).
:- use_module('./extractor.pl').

test(extract_simple, [all(G == [[node('ROOT', [s])]])]) :-
    extract(node('ROOT', [s]), G).

test(extract_complex, [true(Expected =@= Actual), nondet]) :-
    Tree = node('ROOT', [node('PRP', [i]), node(_VP, [node(VBP, [love]), node('PRP', [you])])]),
    Expected = [node('PRP', [i]), node(VBP, [love]), node('PRP', [you])],
    extract(Tree, Actual).
:- end_tests(extractor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(constructor).
:- use_module('./constructor.pl').

test(construct_simple, [all(T == [node('ROOT', [s])])]) :-
    construct(T, [node('ROOT', [s])]).

test(construct_complex, [nondet]) :-
    Grammar = [node(NP, [i]), node(VBP, [love]), node(NP, [you])],
    Expected = node('ROOT', [node(NP, [i]), node(_VP, [node(VBP, [love]), node(NP, [you])])]),
    construct(Expected, Grammar).
:- end_tests(constructor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(converter).
:- use_module('./converter.pl').

test(variable_to_term) :-
    Vars = [_X, a, something(_Y), [c, _Z]],
    Terms = ['x_1', a, something('x_2'), [c, 'x_3']],
    variable_to_term(Vars),
    Vars = Terms.

test(term_to_variable_extractor, [true(NVars =:= 2)]) :-
    RawTree = node('ROOT', [node('PRP', [i]), node('VP', [node('VBP', [love]), node('PRP', [you])]), node('.', ['.'])]),
    term_to_variable(RawTree, Tree),
    term_variables(Tree, Vars),
    length(Vars, NVars).

test(term_to_variable_constructor, [true(NVars =:= 1)]) :-
    RawGrammar = [node('PRP', [i]), node('VBP', [love]), node('PRP', [you])],
    term_to_variable(RawGrammar, Grammar),
    term_variables(Grammar, Vars),
    length(Vars, NVars).

test(dict_node, [true(Actual =@= Expected)]) :-
    Dict = _{
               label: _{label: 'ROOT'},
               children: [
                   _{
                     label: _{label: 'NP'},
                     children: [
                         _{label: i}
                     ]
                   },
                   _{
                     label: _{label: 'VP'},
                     children: [
                         _{label: love}
                     ]
                   },
                   _{
                     label: _{label: 'NP'},
                     children: [
                         _{label: you}
                     ]
                   }
               ]
           },
    Expected = node('ROOT', [node('NP', [i]), node('VP', [love]), node('NP', [you])]),
    dict_node(Dict, Actual).

:- end_tests(converter).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(inference).
:- use_module('./inference.pl').

test(application) :-
    findall(X, infer([category(right, x, y), y], X), [x]),
    findall(X, infer([y, category(left, x, y)], X), [x]).

test(composition) :-
    findall(YZ, infer([category(right, x, y), category(right, y, z)], YZ), [category(right, x, z)]),
    findall(YZ, infer([category(left, x, y), category(left, y, z)], YZ), [category(left, x, z)]).

test(type_raising) :-
    findall(T, infer([x], T), [category(right, y, category(left, y, x)), category(left, y, category(right, y, x)), x]).

:- end_tests(inference).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- run_tests,halt;halt.
%:- show_coverage(run_tests),halt;halt.
