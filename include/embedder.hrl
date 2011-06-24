%%
%% File: embedder.hrl
%% Description : Configuration. FOR MODIFICATIONS IN THIS FILE TO TAKE EFFECT YOU HAVE TO MAKE(./make.sh) IT.
%% Author: Victor Kabdebon <victor.kabdebon@victorkabdebon.net>
%% License : GPL v2
%% Website : http://www.victorkabdebon.net
%% Website(2): http://www.voxnucleus.fr
%% Created: 11 May 2011

-author("victor.kabdebon@victorkabdebon.net").

%%%%%%%%%%%%%%%%%%%%%%
% General fields

% Owner of the Erlembedder name
% Accepted values : list()
-define(ErlEmbedderOwner,"Victor Kabdebon").

% Owner of the Erlembedder Website
% Accepted values : list()
-define(ErlEmbedderOwnerWebsite,"http://www.voxnucleus.fr").

% Version of ErlEmbedder
% No need to be modified 
-define(ErlEmbedderVersion,"v0.4").



%%%%%%%%%%%%%%%%%%%%%%
%%% Server configuration

% Set if the server is in debug mode or not
% Accepted values : true|false
-define(ServerDebugMode,true).

% Server Id, by default : ErlEmbedder
% Accepted values : list()
-define(ServerId,"ErlEmbedder").

% Port used by ErlEmbedder.
% Accepted values : integer()
-define(ServerPort,4446).

% Logging folder
-define(LoggingFolder,"./temp/logs").

% Control the verification printing
% Accepted values : true|false

-define(ShowCompatibilityList,true).

% Control the stats priting
% Accepted values : true|false
-define(ShowStatsPage,true).
-define(ShowGeneralInfos,true).
-define(ShowStats,true).

%%%%%%%%%%%%%%%%%%%%%%
%%% Database configuration

-define(TableName,erlembedder_table).

%%%%%%%%%%%%%%%%%%%%%
%%% Pattern / Regex

% Set where your equivalence file is
% Accepted values list() : this string must be a valid path to a file
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
