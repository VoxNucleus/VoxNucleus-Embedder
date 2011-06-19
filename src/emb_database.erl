%% Description : Embedder link to database
%% Author : Victor Kabdebon <victor.kabdebon@victorkabdebon.net>
%% Website : http://www.victorkabdebon.net
%% Website(2) : http://www.voxnucleus.fr
%% License : GPL v2
%% Created : 29 May 2011

-module(emb_database). 

-author("Victor Kabdebon").
-include("embedder.hrl").

-export([start/0]).
-export([insert/5,batch_insert/1]).
-export([retrieve/1,retrieve/2]).

-record(?TableName,{key,classification,params,default_values,embed_code}).

% records for v0.4
-record(stats_verif,{key,verif}).
-record(stats_requests,{key,request}).

%
% Start mnesia database. 
% Warning, may crash if database is already running
% Output -> success|fail
start()->
    mnesia:create_schema([node()]),
    ok=mnesia:start(),
    success=create(),
    success=batch_insert(emb_loadfromfile:read_lines()).

%
% Create the table
% Warning : May crash if table was already created !
% Output : success|fail
create()->
    case mnesia:create_table(?TableName, [{ram_copies,[node()]},{attributes,record_info(fields,?TableName)}]) of
	{aborted,{already_exists,?TableName}} ->
	    success;
	{atomic,ok} ->
	    success;
	_ ->
	    fail
    end.

%
% Insert a record in the database
% Output -> {ok,atomic}
insert(Key,Classif,Params,DefaultValues,Code)->
    Record = #?TableName{key = Key,classification=Classif, params = Params,default_values=DefaultValues,embed_code= Code},
    F= fun()->
	       mnesia:write(Record)
       end,
    mnesia:transaction(F).




%
% Insert a list of lines (likely from a file)
% Output-> fail| success
batch_insert([Data|RestList])->
    {SiteKey,Class,Params,DefaultValues,Code}=Data,
    case insert(SiteKey,Class,Params,DefaultValues,Code) of
	{atomic,ok}->
	    batch_insert(RestList);
	_->
	    fail
    end;
batch_insert([])->
    success.




%
% Get a key from the database
% Output : Data|[]
retrieve(Key)->
    F=fun()->
	      mnesia:read({?TableName,Key})
      end,
    {atomic,Data}=mnesia:transaction(F),
    case Data of
	[{?TableName,KeyInFile,_,Arguments,DefaultValues,Code}]->
	    {KeyInFile,Arguments,DefaultValues,Code};
	[]->
	    notfound
    end.

%
% Get a key from the database
% Output : Data|[]
retrieve(from,Key)->
    Record = #?TableName{key = Key,classification='from', params = '_',default_values='_',embed_code='_'},
    F= fun()->
	       mnesia:match_object(Record)
       end,
    {atomic,Data}=mnesia:transaction(F),
    case Data of
	[{?TableName,KeyInFile,_,Arguments,DefaultValues,Code}]->
	    {KeyInFile,Arguments,DefaultValues,Code};
	[]->
	    notfound
    end;
% Use for shortcode
retrieve(shortcode,Key) ->
    Record = #?TableName{key = Key,classification='_', params = '_',default_values='_',embed_code='_'},
    F= fun()->
	       mnesia:match_object(Record)
       end,
    {atomic,Data}=mnesia:transaction(F),
    case Data of
	[{?TableName,KeyInFile,_,Arguments,DefaultValues,Code}]->
	    {KeyInFile,Arguments,DefaultValues,Code};
	[]->
	    notfound
    end.
