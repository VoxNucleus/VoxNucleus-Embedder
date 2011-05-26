%%
%% File : embedder_engine.erl
%% Description : "Core" of the algorithm
%% Supports two kind of methods for viewing videos : either ?param=videokey&anyotherparam or /videokey?anyotherparam=
%% Author : Victor Kabdebon  <victor.kabdebon@victorkabdebon.net>
%% Date : 15/05/2011
%%


-module(embedder_engine).

-export([extract_url/1]).
-export([find_key/4]).
-export([extract_key/3]).
-export([find_param/3]).
-export([replace_embedder/2]).
-export([string_to_list/1]).

-include("embedder.hrl").
-include("yaws_api.hrl").

%
% Return the following :
% impossible | {possible, Website, Key}
% impossible to extract
% Website : String containing the "key" to associate (e.g. http://www.vimeo.com/abcde)
% Key : This is the stuff that is used for 
extract_url(URI)->
    {ok,MP}=re:compile(?URIRegex,[]),
    case extract_begin(URI) of
	URLKeyBegin ->
	    case re:run(string:substr(URI,URLKeyBegin+1),MP,[]) of 
		{match,[FirstMatch|_]}->
		    {_,Length}=FirstMatch,
		    Extracted_URL=string:substr(string:substr(URI,URLKeyBegin+1),1,Length),
		    Extracted_URL;
		nomatch->
		    nomatch
	    end;
	nomatch ->
	    nomatch
   end.
    
%Find the portion that is before the URL key (http://www.youtube.com) will match "http://www."
extract_begin(URI)->
    {ok,MP}=re:compile(?URIBegin,[]),
    case re:run(URI,MP,[]) of 
	{match,[FirstMatch|_]}->
	    {_,Length}=FirstMatch,
	    Length;
	nomatch->
	    nomatch
    end.
    
    
%
% Find the key from the argument
% Arguments :
% URL : Found URL
% A : Http Request (Needed ?)
% Output -> Key to the video
find_key(Arguments,DefaultValues,URL,A)->
    {ok,Tokens,_}=erl_scan:string(Arguments),
    {ok,Term}=erl_parse:parse_term(Tokens),
    [Whatever|_]=tuple_to_list(Term),
    {ok,ValTokens,_}=erl_scan:string(DefaultValues),
    {ok,Values}=erl_parse:parse_term(ValTokens),
    [Param|_]=tuple_to_list(Values),
    
    case Whatever of
	slash->
	    extract_key(slash,Param,URL);
	param ->
	    extract_key(param,Param,URL)
    end.


% 
% Extract the key from the URI
% Can either be a slash "/key"
% or a param "?param=key"
extract_key(slash,Place,URI)->
    {ok,MP}=re:compile(?URISlashKeyExtractor,[]),
    {match,[FirstMatch|_]}=re:run(URI,MP,[]),
    {_,Length}= FirstMatch,
    TempURI=string:substr(URI,Length),
    Tokens=string:tokens(TempURI,"/"),
    Key=lists:flatten(find_slash(Tokens,tuple_to_list(Place),1)),
    Key;
%Extract the key from the request
extract_key(param,ParamName,URI) ->
    Pattern=lists:flatten([atom_to_list(ParamName),?ParamExtractor]),
    {ok,MP}=re:compile(Pattern),
    case re:run(URI,MP,[]) of
	{match,[FirstMatch|_]} ->
	    {Offset,Length}=FirstMatch,
	    CompleteKey=string:substr(URI,Offset+1,Length+1),
	    Key=string:substr(CompleteKey,lists:flatlength([ParamName,"="])+1,
			      length(CompleteKey));
	nomatch->
	    throw("Nothing")
    end.

% 
% 
% 
find_slash([Token|TokenRest],[Place|Rest],N)->
    if
	Place==N->
	    [Token,find_slash(TokenRest,Rest,N+1)];
%Else statement
	true ->
	    find_slash(TokenRest,Rest,N+1)
    end;
find_slash(_,[],_) ->
    [];
find_slash([],_,_) ->
    [].
    

%
% Replace the embedder code (this)
% 
replace_embedder(Replacement,EmbedderCode)->
    replace_code(Replacement,EmbedderCode,1).


% 
% Replace the code from a list
% 
replace_code([{_,ToReplace}|ReplaceRest],EmbedderCode,N)->
    Pattern= build_replace_pattern(N),
    {ok,MP}=re:compile(Pattern,[]),
    ModifEmbedderCode = re:replace(EmbedderCode,MP,ToReplace,[{return,list}]),
    replace_code(ReplaceRest,ModifEmbedderCode,N+1);
replace_code([],EmbedderCode,_)->
    EmbedderCode.
%
% Build replace pattern
% N : integer()
% Output -> a string of the form "||N||"

build_replace_pattern(N)->
    	lists:flatten([?ReplacePattern,integer_to_list(N),?ReplacePattern]).


    
% A : Http Request
% Params : [param|OtherParam]
% DefaultValues : [DefaultValue|OtherDefaultValues]
% Output -> [param,Replacement]
% param : atom()
%Replacement : List() String
find_param(A,[Param|RestParam],[DefaultValue|RestValues])->
    ParamString = atom_to_list(Param),
    DefValString=atom_to_list(DefaultValue),
    case yaws_api:getvar(A,ParamString) of
	undefined->
	    ParamValue=DefValString;
	{ok,Value}->
	    ParamValue=Value;
	{Value1,_}->
	    ParamValue=Value1
    end,
    [{Param,ParamValue}|find_param(A,RestParam,RestValues)];
find_param(_,[],_) ->
    [].

%
% Take a tuple of the form "{one,two,three,four}." and outputs [one,two,three]
% Output : [ Atom|List]
% Atom = atom()
string_to_list(TupleAsString)->
    {ok,Tokens,_}=erl_scan:string(TupleAsString),
    {ok,Term}=erl_parse:parse_term(Tokens),
    tuple_to_list(Term).
    
