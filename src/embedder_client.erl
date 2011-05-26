-module(embedder_client).
-author("Victor Kabdebon victor.kabdebon@victorkabdebon.com").

-export([out/1]).

-include("yaws_api.hrl").



%
% Send information to the browser
out(A)->
    case yaws_api:getvar(A,"from") of
	undefined->
	    return_json(json:encode(build_json({error,"Parameter missing"})));
	{ok,From}->
	    try 
		return_json(json:encode(build_json(fetch_infos(From,A))))
	    catch
		throw:Error->
		    return_json(json:encode(build_json({error,Error})))
	    end;
	{From1,From2,_} ->
	    return_json(json:encode(build_json({error,"Multiple parameters"})))	   
    end.

% 
% Find the informations 
% 
fetch_infos(From,A)->
    case find_url_key(From,A) of
	{KeyInFile,Arguments,DefaultValues,Code}->
	    [_|ArgList]= embedder_engine:string_to_list(Arguments),
	    [_|DefValList]=embedder_engine:string_to_list(DefaultValues),
	    Params=embedder_engine:find_param(A,ArgList,DefValList),
	    {found,embedder_engine:replace_embedder([{key,KeyInFile}|Params],Code)};
	notfound->
	    {notfound}
    end.
    

% Not used
find_url_key([])->
    {html,"Required values not found"}.
%
% KeyInFile :
% Arguments :
% DefaultValues
% Code : Code found to be replaced
find_url_key(From,A) ->
    ExtractedURL= embedder_engine:extract_url(From),
    
    case emb_loadfromfile:open_and_search(ExtractedURL) of
	notfound ->
	    notfound;
	{KeyInFile,Arguments,DefaultValues,Code} ->
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
