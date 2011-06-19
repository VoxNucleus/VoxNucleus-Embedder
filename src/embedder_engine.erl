%%
%% File : embedder_engine.erl
%% Description : "Core" of the algorithm
%% Supports two kind of methods for viewing videos : either ?param=videokey&anyotherparam or /videokey?anyotherparam=
%% Author : Victor Kabdebon  <victor.kabdebon@victorkabdebon.net>
%% Date : 15/05/2011
%%


-module(embedder_engine).
-author("Victor Kabdebon").

-export([extract_url/1]).
-export([find_key/4]).
-export([extract_key/3]).
-export([find_param/3]).
-export([replace_embedder/2]).
-export([string_to_list/1]).
-export([get_classification/1]).

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
	{match,URLKeyBegin} ->
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
%
% Output : {match,List}|{nomatch}
extract_begin(URI)->
    {ok,MP}=re:compile(?URIBegin,[]),
    case re:run(URI,MP,[]) of 
	{match,[FirstMatch|_]}->
	    {_,Length}=FirstMatch,
	    {match,Length};
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
    [Whatever|_]=tuple_to_list(Arguments),
    [Param|_]=tuple_to_list(DefaultValues),
    case Whatever of
	slash->
	    extract_key(slash,Param,URL);
	param ->
	    extract_key(param,Param,URL)
    end.


% 
% Extract the key from the URI
% Can either be a slash "/key" or a param "?param=key"
% Output -> Key
% Throws error when no key is found

extract_key(slash,Place,URI)->
    {ok,MP}=re:compile(?URISlashKeyExtractor,[]),
    case re:run(URI,MP,[]) of
	{match,[FirstMatch|RestQueue]}->
	    {_,Length}= FirstMatch,
	    TempURI=string:substr(URI,Length),
	    Tokens=string:tokens(TempURI,"/"),
	    Key=lists:flatten(find_slash(Tokens,tuple_to_list(Place),false,1)),
	    Key;
	nomatch->
	    throw("Not found")
    end;
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
	    throw("The key cannot be found")
    end.
% Finish that
%
extract_key(shortcode,A) ->
    case yaws_api:getvar(A,"website") of
	{ok,Website}->
	    Website;
	nomatch ->
	    throw("The Key cannot be found")
    end.


% 
% Find key(s) when it is a slash
% At the moment the first time a keyis found there is no slash between the two.
%
find_slash([Token|TokenRest],[Place|Rest],Delimiter,N)->
    if
	Place==N->
	    case Delimiter of
		false->
		    Delim="",
		    NextDelimiter="/";
		Delimiter when is_list(Delimiter)->
		    Delim=Delimiter,
		    NextDelimiter=Delimiter
	    end,
	    [Delim,Token,find_slash(TokenRest,Rest,NextDelimiter,N+1)];
%Else statement
	true ->
	    find_slash(TokenRest,[Place|Rest],Delimiter,N+1)
    end;
find_slash(_,[],_,_) ->
    [];
find_slash([],_,_,_) ->
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
    ModifEmbedderCode = re:replace(EmbedderCode,MP,ToReplace,[{return,list},global]),
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
% Replacement : List(): String
find_param(A,[Param|RestParam],[DefaultValue|RestValues])->
    ParamString = atom_to_list(Param),
    DefValString=default_val_to_string(DefaultValue),
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
% Convert the default value to a list(a string)
%
default_val_to_string(DefValue) when is_integer(DefValue)->
    integer_to_list(DefValue);
default_val_to_string(DefValue) when is_atom(DefValue) ->
    atom_to_list(DefValue);
default_val_to_string(DefValue) when is_list(DefValue) ->
    DefValue.

    

%
% Take a tuple of the form "{one,two,three,four}." and outputs [one,two,three]
% Output : [ Atom|List]
% Atom = atom()
string_to_list(TupleAsString)->
    {ok,Tokens,_}=erl_scan:string(TupleAsString),
    {ok,Term}=erl_parse:parse_term(Tokens),
    tuple_to_list(Term).
    
%
% Output -> from|shortcode
%
get_classification(param)->
    from;
get_classification(slash) ->
    from;
get_classification(shortcode) ->
    shortcode.


