%%
%% File: embedder.hrl
%% Description : 
%% Author: Victor Kabdebon <victor.kabdebon@victorkabdebon.net>
%% Website : http://www.victorkabdebon.net
%% Website(2): http://www.voxnucleus.fr
%% Created: 11 May 2011

-author("victor.kabdebon@victorkabdebon.net").

%%% Server configuration


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
