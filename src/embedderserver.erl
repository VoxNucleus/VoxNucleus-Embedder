-module(embedderserver).
-author("Victor Kabdebon <victor@victorkabdebon.com>").
-include("yaws.hrl").

-behavior(application).

%% application callbacks
-export([start/2, 
         stop/1]).

%
% Use for outside call
% Starts yaws
%
start(_Type, Args) ->
    start_atom_yaws(Args),
    error_logger:info_msg("Starting atom_app~n",[]).
	
%Useless ?
	%atom_sup:start_link([]).


%
% Use for outside call 
% Closes yaws
%
stop(_State) ->
    stop_yaws(),
    ok.

%
% Start the server
%
start_atom_yaws(Args) ->
    error_logger:info_msg("Starting embedded yaws~n",[]),
    Id = "yawstest",
    Debug = true,
    ok = application:load(yaws),
    ok = application:set_env(yaws, embedded, true),
    ok = application:set_env(yaws, id, Id),
    application:start(yaws),
    DefaultGC = yaws_config:make_default_gconf(Debug, Id),
    GC = DefaultGC#gconf{logdir="./temp/logs"},
    %add ebin dir
    yaws:mkdir(GC#gconf.logdir),
    SC = #sconf{port = 4446,
		servername = "localhost",
		listen = {0,0,0,0},
		docroot = "www",
		appmods =[{"/",embedder_client}]
	       },
    Result = yaws_api:setconf(GC, [[SC]]),
    error_logger:info_msg("Finished starting embedded yaws~n",[]),
    Result.

%
%
stop_yaws() ->
    application:stop(yaws).
