%% Description :
%% File: emb_loadfromfile.erl
%% Author : Victor Kabdebon  <victor.kabdebon@victorkabdebon.net>
%% Date : 14/05/2011
%%

-module(emb_loadfromfile).

-include("embedder.hrl").
-author("Victor Kabdebon victor.kabdebon@victorkabdebon.net").


-export([open_and_search/1]).

-export([read_all/0]).
-export([read_lines/0]).

-export([get_compatibility_list/0]).


%
% Read all the lines from the embedded server. The lines read are read as TEXT
% Output-> [Head|Tail], Head = {Key,Arguments,DefaultValue,Code}
read_all()->
    {ok,File}=file:open(?PathToEmbList,[read]),
    lists:flatten(read_all(File)).
%
% Recursive read
read_all(File)->
    case io:get_line(File,"") of
	eof->
	    file:close(File),
	    [];
	 Line->
	    case re:split(Line,?Separator,[{return,list}]) of
		List when length(List)==4 ->
		    [KeyInFile,Arguments,DefaultValues,Code,_]=List,
		    [{KeyInFile,Arguments,DefaultValues,Code},read_all(File)];
		_->
		    read_all(File)
	    end
    end.

% 
% 
read_lines() ->
    {ok,File}=file:open(?PathToEmbList,[read]),
    lists:flatten(read_lines(File)).
read_lines(File) ->
    case io:get_line(File,"") of
	eof->
	    file:close(File),
	    [];
	 Line->
	    case analyze_line(Line) of
		skip->
		    [read_lines(File)];
		{KeyInFile,Classification,ArgumentsTuple,DefaultValuesTuple,Code}->
		    [{KeyInFile,Classification,ArgumentsTuple,DefaultValuesTuple,Code},read_lines(File)]
	    end
    end.

%
% Read a line
% Output -> {Key,Classification,Args,DefValues,Code}|skip
analyze_line(Line)->
    case re:split(Line,?Separator,[{return,list}]) of
	List when length(List)==5 ->
	    [KeyInFile,Arguments,DefaultValues,Code,_]=List,
	    {ok,ArgumentsTuple}=emb_util:string_to_tuple(Arguments),
	    [Classif|_]=tuple_to_list(ArgumentsTuple),
	    Classification=embedder_engine:get_classification(Classif),
	    {ok,DefaultValuesTuple}=emb_util:string_to_tuple(DefaultValues),
	    {KeyInFile,Classification,ArgumentsTuple,DefaultValuesTuple,Code};
	_->
	    skip
    end.


% Open the file and search through it
% Output notfound|Params
% Params = {Key,Arguments,Code}
open_and_search(Key)->
    {ok,File}=file:open(?PathToEmbList,[read]),
    find_in_file(File,Key).

% 
% Find the line( and split it) 
% Output -> notfound | 
find_in_file(File,Key)->
    case io:get_line(File,"") of
	eof->	 
	    file:close(File),
	    notfound;
	Line ->
	    ParamList=re:split(Line,?Separator,[{return,list}]),
	    case ParamList of
		[KeyInFile|_] when KeyInFile==Key ->
		    [KeyInFile,Arguments,DefaultValues,Code,_]=ParamList,
		    file:close(File),
		    {KeyInFile,Arguments,DefaultValues,Code};
		_->
		    find_in_file(File,Key)
	    end
    end.

%
% Function to be called.
% Output -> [{Key,Arg,Default},List]
get_compatibility_list()->
    {ok,File}=file:open(?PathToEmbList,[read]),
    lists:flatten(build_compatibility(File)).

% Not recursive function, get rid of it in the future
% Output -> [{Key,Arg,Default}|List]
build_compatibility(File)->
    case io:get_line(File,"") of
	eof->	 
	    [];
	Line ->
	    ParamList=re:split(Line,?Separator,[{return,list}]),
	    [Key,Args,Default,_,_]=ParamList,
	    [{Key,Args,Default},build_compatibility(File)]
    end.
