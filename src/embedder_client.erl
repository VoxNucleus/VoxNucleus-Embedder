% Description: All the calls to get the embedded code are directed to this portion of the program.
% Author: Victor Kabdebon
% Website: http://www.victorkabdebon.com
% Website(2): http://www.voxnucleus.fr
% License: GPL v2

-module(embedder_client).
-author("Victor Kabdebon victor.kabdebon@victorkabdebon.com").

-export([out/1]).

-include("yaws_api.hrl").


%
% Accessed &  Send information to the browser once algorithm is completed
%
out(A)->
    case {yaws_api:getvar(A,"from"),yaws_api:getvar(A,"shortcode")} of
	{{ok,_},_}->
	    return_json(begin_embedder(from, A));
	{_,{ok,_}}->
	    return_json(begin_embedder(shortcode, A));
	{nomatch,nomatch}->
	    return_json(json:encode(build_json({error,"Parameter missing"})));
	{_,_} ->
	    return_json(json:encode(build_json({error,"Error"})))
    end.


	    


% Begin embedding, verify that the variables are present
begin_embedder(from,A)->
    case yaws_api:getvar(A,"from") of
	undefined->
	    json:encode(build_json({error,"Parameter missing"}));
	{ok,From}->
	    try 
		json:encode(build_json(fetch_infos(From,A)))
	    catch
		throw:Error->
		    json:encode(build_json({error,Error}))
	    end;
	{_,_} ->
	    json:encode(build_json({error,"Multiple parameters"}))
    end;
begin_embedder(shortcode,A) ->
    case {yaws_api:getvar(A,"website"),yaws_api:getvar(A,"key")} of
	{{ok,Website},{ok,Key}}->
	    try
		json:encode(build_json(fetch_infos(shortcode,Website,Key,A)))
	    catch
		throw:Error->
		    json:encode(build_json({error,Error}))
	    end;
	_ ->
	    json:encode(build_json({error,"Error in parameters"}))
    end.

fetch_infos(shortcode,Website,Key,A)->
    case find_url_key(shortcode,Website,Key,A)of 
	{Key,Arguments,DefaultValues,Code}->
	    [_|ArgList]= tuple_to_list(Arguments),
	    [_|DefValList]=tuple_to_list(DefaultValues),
	    Params=embedder_engine:find_param(A,ArgList,DefValList),
	    {found,embedder_engine:replace_embedder([{key,Key}|Params],Code)};
	notfound ->
	    {notfound};
	_ ->
	    throw("Erreur")
    end.

% 
% Find the informations
% Output -> {found,_}|{notfound}
fetch_infos(From,A)->
    case find_url_key(From,A) of
	{KeyInFile,Arguments,DefaultValues,Code}->
	    [_|ArgList]= tuple_to_list(Arguments),
	    [_|DefValList]=tuple_to_list(DefaultValues),
	    Params=embedder_engine:find_param(A,ArgList,DefValList),
	    {found,embedder_engine:replace_embedder([{key,KeyInFile}|Params],Code)};
	notfound->
	    {notfound}
    end.

%
% KeyInFile :
% Arguments :
% DefaultValues
% Code : Code found to be replaced
find_url_key(From,A) ->
    ExtractedURL= embedder_engine:extract_url(From),
    case emb_database:retrieve(from,ExtractedURL) of
	notfound ->
	    notfound;
	{_,Arguments,DefaultValues,Code} ->
	    emb_database:update_stats(request,ExtractedURL),
	    Key=embedder_engine:find_key(Arguments,DefaultValues,From,A),
	    {Key,Arguments,DefaultValues,Code};
	_ ->
	    throw("Error in file...")
    end.
find_url_key(shortcode,From,Key,A)->
    case emb_database:retrieve(shortcode,From) of
	notfound->
	    notfound;
	{_,Arguments,DefaultValues,Code} ->
	    emb_database:update_stats(request,From),
	    {Key,Arguments,DefaultValues,Code};
	_ ->
	    throw("Error in file...")
    end.



%
% Organize data for JSON conversion
%
build_json({found,EmbeddedCode})->
    {struct,[{status,"found"},{code,EmbeddedCode}]};
build_json({error,ErrorExplanation}) ->
    {struct,[{status, "error"},{explanation,ErrorExplanation}]};
build_json({notfound}) ->
    {struct,[{status,"notfound"}]}.


% (Header function)
% Return data as JSON
return_json(Json)->
    {content, 
    "application/json; charset=iso-8859-1", 
    Json}.
