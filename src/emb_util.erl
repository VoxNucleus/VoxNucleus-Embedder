%%
%% File : emb_util.erl
%% Description : Utils used in embedder
%% Author : Victor Kabdebon
%% Date 06/11/2011

-module(emb_util).
-author("Victor Kabdebon").

-export([string_to_tuple/1]).


% Functions that transforms a string into a tuple.
% Output -> {ok,Tuple}|fail
%
string_to_tuple(InString)->
    {ok,Tokens,_}=erl_scan:string(InString),
    {ok,Term}=erl_parse:parse_term(Tokens),
    {ok,Term}.
