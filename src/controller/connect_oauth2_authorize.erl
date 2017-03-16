-module(connect_oauth2_authorize).

-export([init/2]).

-include("common.hrl").

%% http://127.0.0.1:8089/connect/oauth2/authorize?appid=app_id_1&connect_redirect=1&redirect_uri=http%3A%2F%2Fstaging.red0769.com%3A8001%2Fnode%2Fsync-cb%2Fwechat-pubsrv-login&response_type=code&scope=snsapi_user&state=dummy_var%3D1
init(Req, Opts) ->
    ?DEBUG("~s ~s", [maps:get(path, Req), maps:get(qs, Req)]),
    File = filename:join(app_misc:project_root_dir(), "web/views/connect_oauth2_authorize.mustache"),
    Template = bbmustache:parse_file(File),

    %% Attain query parameters.
    QS = cowboy_req:parse_qs(Req),
    AppId = proplists:get_value(<<"appid">>, QS),
    %Scope = proplists:get_value(<<"scope">>, QS), % fixed to 'snsapi_user' temporarily
    %ResponseType = proplists:get_value(<<"response_type">>, QS), % fixed to 'code' temporarily
    RedirectUri = proplists:get_value(<<"redirect_uri">>, QS),
    %ConnectRedirect = proplists:get_value(<<"connect_redirect">>, QS), % fixed to '1' temporarily
    State = proplists:get_value(<<"state">>, QS),
    Body = case data_misc:app_info(AppId) of               
               {ok, {Data}} ->
                   AccInfoList = proplists:get_value(<<"acc_info">>, Data, []),
                   AccInfoMap = lists:foldl(fun({AccInfo}, Acc) ->
                                                    FakeAccId = proplists:get_value(<<"fake_acc_id">>, AccInfo),
                                                    HeadImgUrl = proplists:get_value(<<"headimgurl">>, AccInfo),
                                                    Nickname = proplists:get_value(<<"nickname">>, AccInfo),
                                                    [#{"fakeAccId" => FakeAccId, "headimgurl" => HeadImgUrl, "nickname" => Nickname}|Acc]
                                            end, [], AccInfoList),
                   ?DEBUG("AccInfoMap ~p", [AccInfoMap]),
                   %% Dynamically render the webpage in mustache codec.
                   bbmustache:compile(Template, #{"accInfo" => AccInfoMap, "state" => State, "appid" => AppId, "redirectUri" => RedirectUri});

               %% Return the webpage in HTML codec.
               Ret ->
                   jiffy:encode(Ret)
           end,
    {ok, cowboy_req:reply(200, #{}, Body, Req), Opts}.

