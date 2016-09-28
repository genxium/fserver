-module(sns_oauth2_access_token).

-export([init/2]).

-include("common.hrl").

%% http://127.0.0.1:8089/sns/oauth2/access_token?appid=app_id_1&secret=secret&code=code&grant_type=authorization_code
init(Req, Opts) ->
    QS = cowboy_req:parse_qs(Req),
    AppId = proplists:get_value(<<"appid">>, QS),
    ?DEBUG("QS ~p", [QS]),
    JSON = case data_misc:app_info(AppId) of               
               {ok, {Data}} ->
                   RecvSecret = proplists:get_value(<<"secret">>, QS),
                   Code = proplists:get_value(<<"code">>, QS),
                   Secret = proplists:get_value(<<"secret">>, Data),
                   
                   if
                       RecvSecret =/= Secret ->
                           ?RET(?EC_ERROR_SECRET);
                       true -> 
                           AccInfoList = proplists:get_value(<<"acc_info">>, Data, []),
                           case lists:filter(fun({AccInfo}) ->
                                                     case proplists:get_value(<<"code">>, AccInfo) of
                                                         Code ->
                                                             true;
                                                         _ ->
                                                             false
                                                     end
                                             end, AccInfoList) of           
                               [] ->
                                   ?RET(?EC_ERROR_CODE);
                               [{Info}|_] ->
                                   #{
                                     access_token => proplists:get_value(<<"access_token">>, Info, "access_token"),
                                     openid => proplists:get_value(<<"openid">>, Info, "openid"),
                                     unionid => proplists:get_value(<<"unionid">>, Info, "unionid")
                                   }
                           end     
                   end;
               Ret ->
                   Ret
           end,
	{ok, cowboy_req:reply(200, #{}, jiffy:encode(JSON), Req), Opts}.
