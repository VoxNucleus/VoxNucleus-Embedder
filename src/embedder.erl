-module(embedder).
-author("Victor Kabdebon <victor@victorkabdebon.com>").

-export([start/0,stop/0]).
-export([insertInLog/2,insertInLog/1]).
-export([terminate/0]).
-export([init/0]).

%Start function
start()->
    insertInLog(system,"Starting embedder..."),
    register(mainProcess,spawn(embedder,init,[])),
    embedderserver:start([],[]).

% Stop function
% Called from the outside
stop()->
    mainProcess ! {stop}.
    %unregister(mainProcess),
    %insertInLog(system,"Embedder stopped").

% Terminate the program (Including http server) 
terminate()->
    insertInLog(system,"Stopping embedder..."),
    embedderserver:stop([]).

%
init()->
    receive
	{req,url}->
	    io:format("New req :");
	stop ->
	    terminate()
    end.

%
call_embedder({req,Address})->
    io:format("Request").

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
