-module(auth_fake_login).

-export([init/2]).

-include("common.hrl").

%% http://127.0.0.1:8089/auth/fake/login?fake_acc_id=111&appid=app_id_1
init(Req, Opts) ->
    QS = cowboy_req:parse_qs(Req),
    FakeAccId = proplists:get_value(<<"fake_acc_id">>, QS),
    AppId = proplists:get_value(<<"appid">>, QS),
    ?DEBUG("QS ~p", [QS]),
    JSON = case data_misc:app_info(AppId) of               
               {ok, {Data}} ->
                   AccInfoList = proplists:get_value(<<"acc_info">>, Data, []),
                   case lists:filter(fun({AccInfo}) ->
                                             case proplists:get_value(<<"fake_acc_id">>, AccInfo) of
                                                 FakeAccId ->
                                                     true;
                                                 _ ->
                                                     false
                                             end
                                     end, AccInfoList) of           
                       [] ->
                           ?RET(?EC_ERROR_ACC_ID);
                       [{Info}|_] ->
                           Code = proplists:get_value(<<"code">>, Info, "code"),
                           #{code => Code}
                   end;
               Ret ->
                   Ret
           end,
	{ok, cowboy_req:reply(200, #{}, jiffy:encode(JSON), Req), Opts}.
