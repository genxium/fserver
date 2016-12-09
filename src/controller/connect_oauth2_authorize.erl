-module(connect_oauth2_authorize).

-export([init/2]).

-include("common.hrl").

%% http://127.0.0.1:8089/connect/oauth2/authorize?appid=app_id_1&connect_redirect=1&redirect_uri=http%3A%2F%2Fstaging.red0769.com%3A8001%2Fnode%2Fsync-cb%2Fwechat-pubsrv-login&response_type=code&scope=snsapi_user&state=dummy_var%3D1
init(Req, Opts) ->
    File = filename:join(app_misc:project_root_dir(), "web/views/connect_oauth2_authorize.mustache"),
    Template = bbmustache:parse_file(File),

		% Attain query parameters. 
    QS = cowboy_req:parse_qs(Req),
    AppId = proplists:get_value(<<"appid">>, QS),
		Scope = proplists:get_value(<<"scope">>, QS), % fixed to 'snsapi_user' temporarily 
		ResponseType = proplists:get_value(<<"response_type">>, QS), % fixed to 'code' temporarily 	
		RedirectUri = proplists:get_value(<<"redirect_uri">>, QS),
		ConnectRedirect = proplists:get_value(<<"connect_redirect">>, QS), % fixed to '1' temporarily 
		State = proplists:get_value(<<"state">>, QS), 

		% TODO: Attain `AccInfo` from preset file.
		AccInfo = [
			#{"fakeAccId" => "57ce328c66ad43c987155098"},
			#{"fakeAccId" => "57ce837866ad43c987155099"}
		], 

		% Dynamically render the webpage in mustache codec.
    Body = bbmustache:compile(Template, #{"accInfo" => AccInfo, "state" => State, "appid" => AppId, "redirectUri" => RedirectUri}),

	% Return the webpage in HTML codec. 
	{ok, cowboy_req:reply(200, #{}, Body, Req), Opts}.


timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
