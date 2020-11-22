#!/usr/bin/env swipl
% -*- mode: prolog; coding: utf-8  -*-
% vim: set ft=prolog fileencoding=utf-8 fileformats=unix :

:- use_module(library(http/http_server)).
:- use_module(library(http/http_unix_daemon)).
:- use_module('processor.pl').

handle(Request) :-
    http_read_json_dict(Request, Body, [value_string_as(atom)]),
    Body = _{jsonrpc:Version, id:Id, method:Method, params:[Input]},
    ((findall(Output, process(Method, Input, Output), ListOfOutput),
     ListOfOutput = [_|_],
     reply_json_dict(_{jsonrpc: Version, id: Id, result: ListOfOutput}, [width(0)]));
    (Error =  _{message: "Failed to process request", request: Body},
     reply_json_dict(_{jsonrpc: Version, id: Id, error: Error}, [width(0)]))).

:- http_handler(root(.), handle, []).

main :- http_daemon.
