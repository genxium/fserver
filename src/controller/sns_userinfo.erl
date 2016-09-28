-module(sns_userinfo).

-export([init/2]).

-include("common.hrl").

%% http://127.0.0.1:8089/sns/userinfo?appid=app_id_1&access_token=access_token&openid=openid
init(Req, Opts) ->
    QS = cowboy_req:parse_qs(Req),
    AppId = proplists:get_value(<<"appid">>, QS),
    ?DEBUG("QS ~p", [QS]),
    JSON = case data_misc:app_info(AppId) of               
               {ok, {Data}} ->
                   RecvAccessToken = proplists:get_value(<<"access_token">>, QS),
                   RecvOpenId = proplists:get_value(<<"openid">>, QS),

                   AccInfoList = proplists:get_value(<<"acc_info">>, Data, []),
                   case lists:filter(fun({AccInfo}) ->
                                             AccessToken = proplists:get_value(<<"access_token">>, AccInfo),
                                             OpenId = proplists:get_value(<<"openid">>, AccInfo),
                                             RecvAccessToken =:= AccessToken andalso
                                                 RecvOpenId =:= OpenId
                                     end, AccInfoList) of
                       [] ->
                           ?RET(?EC_ERROR_PARAMS);
                       [{Info}|_] ->
                           #{
                         nickname => proplists:get_value(<<"nickname">>, Info, "nickname"),
                         headimgurl => proplists:get_value(<<"headimgurl">>, Info, "headimgurl")
                        }
                   end;
               Ret ->
                   Ret
           end,
	{ok, cowboy_req:reply(200, #{}, jiffy:encode(JSON), Req), Opts}.
