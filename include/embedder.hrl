%%
%% File: embedder.hrl
%% Description : 
%% Author: Victor Kabdebon <victor.kabdebon@victorkabdebon.net>
%% Website : http://www.victorkabdebon.net
%% Website(2): http://www.voxnucleus.fr
%% Created: 11 May 2011

-author("victor.kabdebon@victorkabdebon.net").
% Set where your equivalence file is
-define(PathToEmbList,"/home/victork/erlang/embedder/embedded-equivalence").

% Regular expression
-define(URISlashKeyExtractor,"^((http|https|ftp)://)?(www\.)?[^/]*/").

%-define(URISlashKeyExtractor,"^(http://|https://|ftp://)?(www.)?.*/").
-define(URIRegex,"[^/]*").
% Regular expression for the beginning of the url
-define(URIBegin,"^((http|https|ftp)://)?(www\.)?").

%
% Param extractor
-define(ParamExtractor,"=[^&]*").

% Patterns that is around the number (used for replacement)
-define(ReplacePattern,"~").
