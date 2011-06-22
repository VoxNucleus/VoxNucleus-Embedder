% Description: This module allows the jquery scripts to verify quickly if an address can be embedded. It allows also to see the compatibility from the user.
% Author: Victor Kabdebon
% Website: http://www.victorkabdebon.net 
% Website(2): http://www.voxnucleus.fr
% License: GPL v2
%
-module(emb_verification).

-author("Victor Kabdebon victor.kabdebon@victorkabdebon.com").

-export([out/1]).
-include("yaws_api.hrl").
-include("embedder.hrl").

%
% Method called
%
out(A)->
    case yaws_api:getvar(A,"requesting") of
	undefined->
	    [{header, {content_type, "text/html"}},
	     {ehtml,display_compatible()}];
	{ok,"ispresent"} ->
	    try
		Json=json:encode(build_ispresent_json(ispresent(A))),
		return_json(Json)
	    catch
		throw:Error->
		    JsonError=json:encode(build_ispresent_json(error,Error)),
		    return_json(JsonError)
	    end
	    
    end.

%
% Show the compatible websites available as table 
% 
display_compatible()->
    Compatibility=emb_loadfromfile:get_compatibility_list(),
    List = lists:flatten(build_table_lines(Compatibility,1)),
    [{'h2',[],"Compatibility list "},
     {'div',[{style,"margin:0 0 5px 0;"}],"This is automatically generated content from the embedded-list at the root of Erlembedder."},
     {table,[{style,"border:1px solid black;"}],[{tr,[{style,"border-bottom:1px solid black;font-weight:bolder;"}],[{td,[],"#"},{td,[],"Site"},{td,[],"parametres disponibles"},{td,[],"Default values"}]},List]}].

%
%Build a table line
%
build_table_lines([],_)->
    [];
build_table_lines([{Key,Args,Default}|Rest],Number) ->
    [{tr,[],[{td,[], integer_to_list(Number)},{td,[],Key},{td,[],Args},{td,[],Default}]},build_table_lines(Rest,Number+1)].


%
% Check if an URL is present
%
ispresent(A)->
    case yaws_api:getvar(A,"from") of
	undefined->
	    throw("Parameter missing");
	{ok,From}->
	    ExtractedURL= embedder_engine:extract_url(From),
	    case emb_database:retrieve(ExtractedURL) of
		notfound->
		    false;
		 {_,Arguments,_,_}->
		    emb_database:update_stats(verif,ExtractedURL),
		    {true,"test"}
	    end
    end.


%tuple_to_string(tuple_to_list(Arguments))
tuple_to_string([NewArg,Rest])->
    [atom_to_list(NewArg),tuple_to_string(Rest)];
tuple_to_string([]) ->
    [].
%
% Build the JSON answers
%
build_ispresent_json({true,Params})->
    {struct,[{success,"true"},{present,"true"},{available_params,Params}]};
build_ispresent_json(false) ->
    {struct,[{success,"true"},{present,"false"}]}.
build_ispresent_json(error,Error) ->
    {struct,[{success,"false"},{error,Error}]}.



% (Header function)
% Return data as JSON
return_json(Json)->
    {content, 
    "application/json; charset=iso-8859-1", 
    Json}.
