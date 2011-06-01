-module(embedder).
-author("Victor Kabdebon <victor@victorkabdebon.com>").

-export([start/0]).
-export([insertInLog/2,insertInLog/1]).
-export([terminate/0]).

%
% Start embedder (called at the begining)
%
start()->
    insertInLog(system,"Starting embedder..."),
    emb_database:start(),
    embedderserver:start([],[]).

%
% Terminate the program (Including http server)
%
terminate()->
    insertInLog(system,"Stopping embedder..."),
    embedderserver:stop([]).

% Insert in log
% For system calls
insertInLog(system,Message)->
    System_s = atom_to_list(system),
    ToInsert=["{{",System_s,"}}", " ",Message],
    insertInLog(ToInsert).
% Insert a message
insertInLog(Message) ->
    {{Year,Month,Day},{Hour,Minutes,Seconds}}=erlang:localtime(),
    Date = [io_lib:write(Year),"/",io_lib:write(Month),"/",io_lib:write(Day)," ",io_lib:write(Hour),":",io_lib:write(Minutes),":",io_lib:write(Seconds)," - "],
    ToInsert_f=string:concat(Date,Message),
    ToInsert_new_line=string:concat(ToInsert_f,"\n"),
    file:write_file("./embedder.log",ToInsert_new_line,[append]).
