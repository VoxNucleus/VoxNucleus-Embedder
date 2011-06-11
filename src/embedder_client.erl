-module(embedder_client).
-author("Victor Kabdebon victor.kabdebon@victorkabdebon.com").

-export([out/1]).

-include("yaws_api.hrl").



%
% Accessed &  Send information to the browser once algorithm is completed
%
out(A)->
%    From=yaws_api:getvar(A,"from"),
 %   ShortCode=yaws_api:getvar(A,"shortcode"),

    case {yaws_api:getvar(A,"from"),yaws_api:getvar(A,"shortcode")} of
	{{ok,_},_}->
	    return_json(begin_embedder(from, A));
	{_,{ok,_}}->
	    return_json(begin_embedder(shortcode, A));
	{nomatch,nomatch}->
	    return_json(json:encode(build_json({error,"Parameter missing"})))
    end.


	    

%
%
%
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
	{From1,From2,_} ->
	    json:encode(build_json({error,"Multiple parameters"}))
    end;
begin_embedder(shortcode,A) ->
    json:encode(build_json({error,"Not implemented"})).
% 
% Find the informations
% 
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
    

% Not used atm
find_url_key([])->
    {html,"Required values not found"}.
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
	    Key=embedder_engine:find_key(Arguments,DefaultValues,From,A),
	    {Key,Arguments,DefaultValues,Code};
	_ ->
	    "Error in the file..."
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
