#!/usr/bin/env swipl
% -*- mode: prolog; coding: utf-8  -*-
% vim: set ft=prolog fileencoding=utf-8 fileformats=unix :

:- module(processor, [process/3]).
:- use_module('extractor.pl').
:- use_module('constructor.pl').
:- use_module('converter.pl').

process(Method, InDict, OutDict) :-
    dict_node(InDict, RawInput),
    term_to_variable(RawInput, Input),
    with_output_to(user_error, format('>> ~p(~p, ~p)~n', [Method, Input, Output])),
    ((Method == extract, extract(Input, Output));
     (Method == construct, construct(Output, Input))),
    with_output_to(user_error, format('<< ~p(~p, ~p)~n', [Method, Input, Output])),
    variable_to_term(Output),
    dict_node(OutDict, Output).

