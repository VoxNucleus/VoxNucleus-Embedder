%% Description :
%% File: emb_loadfromfile.erl
%% Author : Victor Kabdebon  <victor.kabdebon@victorkabdebon.net>
%% Date : 14/05/2011
%%

-module(emb_loadfromfile).

-include("embedder.hrl").
-author("Victor Kabdebon victor.kabdebon@victorkabdebon.net").

-export([open_and_read/0]).
-export([open_and_search/1]).

-export([read_all/0]).
% TODO Add advise for speedup ?

-export([get_compatibility_list/0]).

%
% Open a file for read
%
open_and_read()->
    {ok,File}=file:open(?PathToEmbList,[read]),
    lists:flatten(for_each_line(File)).
%
% Read all the lines from the embedded server
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
	    ParamList=re:split(Line,?Separator,[{return,list}]),
	    [KeyInFile,Arguments,DefaultValues,Code,_]=ParamList,
	    [{KeyInFile,Arguments,DefaultValues,Code},read_all(File)]
    end.
	    

% Read each line
%TODO : Remove this function
for_each_line(File)->
    case io:get_line(File,"") of
	eof->	 
	    file:close(File);
	Line ->
	    lists:concat([Line,for_each_line(File)])
    end.


%
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
		List->
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
    
