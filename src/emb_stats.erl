%% Description: This module shows stats about ErlEmbedder

%% Created: 06/16/2011
%% Author: Victor Kabdebon 

-module(emb_stats).

-author("victor@victorkabdebon.com").

-export([out/1]).
-include("yaws_api.hrl").
-include("embedder.hrl").

%
% Called when user go to this page
% Output-> ehtml
out(A)->
     [{header, {content_type, "text/html"}},
	     {ehtml,display_infos()}].

%
% Show the infos
display_infos()->
    [{'h2',[],"ErlEmbedder stats & information"},display_general_infos(?ShowGeneralInfos),display_stats(?ShowStats)].

% Show the general infos, fed from the embedder.hrl
%
display_general_infos(true)->
    [{'h3',[],"General information"},
     {'em',[],"The information provided can be modified from the file located in $ErlembedderRoot/include/embedder.hrl. Be sure to remake ErlEmbedder for modifications to take place."},
     {'div',[],["Server id: ",?ServerId]},
     {'div',[],["Server port: ",integer_to_list(?ServerPort)]},
     {'div',[],["Server in debug-mode: ",atom_to_list(?ServerDebugMode)]},
     {'div',[],["Server Owner: ",?ErlEmbedderOwner]},
     {'div',[],["Server Owner Website: ",?ErlEmbedderOwnerWebsite]},
     {'div',[],["Version: ",?ErlEmbedderVersion]}];
display_general_infos(false)->
    [{'em',[],"Section General Infos deactivated."}].

%
% Display the stats. Can be turned of 
display_stats(true)->
    [{'h2',[],"Statistics"},{b,[],"Usage statistics. They do not represent ALL the statistics but only the ones that were successful. Future versions of ErlEmbedder might include the one that failed."},{'h3',[],"Verification statistics"},verif_stats(),{'h3',[],"Request statistics"},request_stats()];
display_stats(false)->
    {'em',[],"Section stats deactivated"}.

% Show the verif stats
%
verif_stats()->
    List = emb_database:retrieve_all_stats(verif),
    [{'em',[],"This table shows statistics of the verifications send to ErlEmbedder. Please note that it does not take into account all the verifications: only the ones that are found in the database are updated."},{'table',[],build_stats_table(List,0)}].

% Show the request stats
% Output -> ehtml
request_stats()->
    List = emb_database:retrieve_all_stats(request),
    [{'em',[],"This table shows statistics of the request send to ErlEmbedder. Everytime a user click on a video to use the embedded video it will update this table. Please note that it does not take into account all the verifications: only the ones that are found in the database are updated."},{'table',[],build_stats_table(List,0)}].

% Construct the stats table
% Output-> ehtml
build_stats_table([{Website,Nb},Tail],Total)->
    [{tr,[],
      [{td,[],Website},{td,[],integer_to_list(Nb)}]
     },build_stats_table(Tail,Total+Nb)];
build_stats_table([],Total) ->
    [{tr,[],
      [{td,[],"Total"},{td,[],integer_to_list(Total)}]
     }];
build_stats_table(none,_) ->
    [{tr,[],[{td,[],">No stats available at the moment."}]}].


