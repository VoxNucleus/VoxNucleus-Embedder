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
% TODO Add advise for speedup

open_and_read()->
    {ok,File}=file:open(?PathToEmbList,[read]),
    lists:flatten(for_each_line(File)).

% Read each line
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
% TODO : add parameters
find_in_file(File,Key)->
    case io:get_line(File,"") of
	eof->	 
	    file:close(File),
	    notfound;
	Line ->
	    ParamList=string:tokens(Line,";"),
	    case ParamList of
		[KeyInFile|_] when KeyInFile==Key ->
		    [KeyInFile,Arguments,DefaultValues,Code,_]=ParamList,
		    file:close(File),
		    {KeyInFile,Arguments,DefaultValues,Code};
		List->
		    find_in_file(File,Key)
	    end
    end.
