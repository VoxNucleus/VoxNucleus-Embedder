%%
%% File: embedder.hrl
%% Description : Configuration. FOR MODIFICATIONS IN THIS FILE TO TAKE EFFECT YOU HAVE TO MAKE IT.
%% Author: Victor Kabdebon <victor.kabdebon@victorkabdebon.net>
%% License : GPL v2
%% Website : http://www.victorkabdebon.net
%% Website(2): http://www.voxnucleus.fr
%% Created: 11 May 2011

-author("victor.kabdebon@victorkabdebon.net").

%%%%%%%%%%%%%%%%%%%%%%
% General fields

% Owner of the Erlembedder name
-define(ErlEmbedderOwner,"Victor Kabdebon").

% Owner of the Erlembedder Website
-define(ErlEmbedderOwnerWebsite,"http://www.voxnucleus.fr").

% Version of ErlEmbedder
-define(ErlEmbedderVersion,"v0.4").



%%%%%%%%%%%%%%%%%%%%%%
%%% Server configuration

% Set if the server is in debug mode or not
% Accepted values : true|false
-define(ServerDebugMode,true).

% Server Id, by default : Embedder
-define(ServerId,"ErlEmbedder").

% Server port
-define(ServerPort,4446).

% Logging folder
-define(LoggingFolder,"./temp/logs").

% Control the stats priting
%
-define(ShowStatsPage,true).
-define(ShowGeneralInfos,true).
-define(ShowStats,true).

%%%%%%%%%%%%%%%%%%%%%%
%%% Database configuration

-define(TableName,erlembedder_table).

%%%%%%%%%%%%%%%%%
%%% Pattern / Regex

% Set where your equivalence file is
-define(PathToEmbList,"../embedded-equivalence").

% Regular expression
-define(URISlashKeyExtractor,"^((http|https|ftp)://)?(www\.)?[^/]*/").

%-define(URISlashKeyExtractor,"^(http://|https://|ftp://)?(www.)?.*/").
-define(URIRegex,"[^/]*").
% Regular expression for the beginning of the url
-define(URIBegin,"^((http|https|ftp)://)?(www\.)?").

% Param extractor
-define(ParamExtractor,"=[^&]*").

%% embedded-equivalence file
% Patterns that is around the number (used for replacement)
-define(ReplacePattern,"~").
-define(Separator,";;").
