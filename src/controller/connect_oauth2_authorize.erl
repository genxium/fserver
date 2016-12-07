-module(connect_oauth2_authorize).

-export([init/2]).

-include("common.hrl").

%% http://127.0.0.1:8089/connect/oauth2/authorize?appid=app_id_1&connect_redirect=1&redirect_uri=http%3A%2F%2Fstaging.red0769.com%3A8001%2Fnode%2Fsync-cb%2Fwechat-pubsrv-login&response_type=code&scope=snsapi_user&state=dummy_var%3D1
init(Req, Opts) ->
    File = filename:join(app_misc:project_root_dir(), "web/views/connect_oauth2_authorize.mustache"),
    Template = bbmustache:parse_file(File),
    Body = bbmustache:compile(Template, #{"timestamp" => timestamp()}),
	{ok, cowboy_req:reply(200, #{}, Body, Req), Opts}.


timestamp() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
