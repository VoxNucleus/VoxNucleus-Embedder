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

% Regular non stat functions

-export([insert/5,batch_insert/1]).
-export([retrieve/1,retrieve/2]).
-export([retrieve_all/0]).


%stats functions

-export([insert_stats/1,batch_insert_stats/1]).
-export([update_stats/2]).
-export([retrieve_stats/2]).
-export([retrieve_all_stats/1]).

-record(?TableName,{key,classification,params,default_values,embed_code}).
-record(stats_verif,{key,verif}).
-record(stats_request,{key,request}).

%
% Start mnesia database. 
% Warning, may crash if database is already running
% Output -> success|fail
start()->
    mnesia:create_schema([node()]),
    ok=mnesia:start(),
    success=create(),
    ToInsert = emb_loadfromfile:read_lines(),
    success=batch_insert(ToInsert),
    success=batch_insert_stats(ToInsert).

%
% Create the table
% Warning : May crash if table was already created !
% Output : success|fail
create()->
    case {create_main_table(),create_verif_table(),create_request_table()} of
	{success,success,success}->
	    success;
	{_,_,_} ->
	    fail
    end.

% Creates the main table
create_main_table()->
    case mnesia:create_table(?TableName, [{ram_copies,[node()]},{attributes,record_info(fields,?TableName)}]) of
	{aborted,{already_exists,?TableName}} ->
	    success;
	{atomic,ok} ->
	    success;
	_ ->
	    fail
    end.
% Creates the request stats table
create_request_table()->
    case mnesia:create_table(stats_request, [{ram_copies,[node()]},{attributes,record_info(fields,stats_request)}]) of
	{aborted,{already_exists,stats_request}} ->
	    success;
	{atomic,ok} ->
	    success;
	_ ->
	    fail
    end.
% Creates the verif stats table
create_verif_table()->
    case mnesia:create_table(stats_verif, [{ram_copies,[node()]},{attributes,record_info(fields,stats_verif)}]) of
	{aborted,{already_exists,stats_verif}} ->
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

% Called only at the create of the stats table
% Output -> {atomic,ok}
insert_stats(Key)->
    VerifRec=#stats_verif{key=Key,verif=0},
    RequestRec=#stats_request{key=Key,request=0},
    F=fun()->
	      mnesia:write(VerifRec),
	      mnesia:write(RequestRec)
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
% Do a batch insert of all the stats
%
batch_insert_stats([Data|RestList])->
    {SiteKey,_,_,_,_}=Data,
    {atomic,ok}=insert_stats(SiteKey),
    batch_insert_stats(RestList);
batch_insert_stats([]) ->
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

retrieve_all()->
    Record = #?TableName{key = '_',classification='_', params = '_',default_values='_',embed_code='_'},
    F= fun()->
	       mnesia:match_object(Record)
       end,
    {atomic,Data}=mnesia:transaction(F).

%
% Add one to the counter. It *should* be atomic according to the description
%

update_stats(verif,Key)->
    mnesia:dirty_update_counter(stats_verif,Key,1);
update_stats(request,Key) ->
    mnesia:dirty_update_counter(stats_request,Key,1).
%
%
%
retrieve_stats(verif,Key)->
    VerifRecord = #stats_verif{key=Key, verif='_'},
    F=fun()->
	      mnesia:match_object(VerifRecord)		 
      end,
    {atomic,Data}=mnesia:transaction(F),
    case Data of
	[{stats_verif,Key,NbVerif}]->
	    {Key,NbVerif};
	[]->
	    notfound
    end;
retrieve_stats(request,Key) ->
    VerifRecord = #stats_request{key=Key, request='_'},
    F=fun()->
	      mnesia:match_object(VerifRecord)		 
      end,
    {atomic,Data}=mnesia:transaction(F),
    case Data of
	[{stats_verif,Key,NbVerif}]->
	    {Key,NbVerif};
	[]->
	    notfound
    end.


% Retrieve all the statistics in the table
% Output -> [Stat1|Rest] | none
% Stat1 -> {List,Integer}
retrieve_all_stats(verif)->
    AllRecordVer= #stats_verif{key='_',verif='_'},
    F=fun()->
	      mnesia:match_object(AllRecordVer)		 
      end,
    {atomic,Data}=mnesia:transaction(F),
    case Data of
	[_|_]->
	    stats_to_list(Data);
	[]->
	    none
    end;
retrieve_all_stats(request) ->
    AllRecordReq=#stats_request{key='_',request='_'},
    F=fun()->
	      mnesia:match_object(AllRecordReq)		 
      end,
    {atomic,Data}=mnesia:transaction(F),
    case Data of
	[_|_]->
	    stats_to_list(Data);
	[]->
	    none
    end.

stats_to_list([{_,Key,Nb}|Rest])->
    NewElem={Key,Nb},
    [NewElem,stats_to_list(Rest)];
stats_to_list([])->
    [].
