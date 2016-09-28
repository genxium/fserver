-module(auth_fake_login).

-export([init/2]).

-include("common.hrl").
-include("define_error_code.hrl").

%% http://127.0.0.1:8089/auth/fake/login?fake_acc_id=111
init(Req, Opts) ->
    QS = cowboy_req:parse_qs(Req),
    JSON = case lists:keyfind(<<"fake_acc_id">>, 1, QS) of
               false ->
                   ?RET(?EC_ERROR_PARAMS);
               {_, FakeAccId} ->
                   ?DEBUG("FakeAccId ~p", [FakeAccId]),
                   case data_misc:read(?AUTH_FAKE_LOGIN_DATA) of
                       {error, _} ->
                           ?RET(?EC_ERROR_JSON);
                       {ok, Data} ->
                          case [Info || {[{<<"fake_acc_id">>,FakeAccId0}|_]}=Info <- Data, 
                                        FakeAccId0=:=FakeAccId] of
                              [] ->
                                  ?RET(?EC_ERROR_ACC_ID);
                              [{Info}|_] ->
                                  Code = proplists:get_value(<<"code">>, Info, "code"),
                                  #{code => Code}
                          end
                   end
           end,
	{ok, cowboy_req:reply(404, #{}, jiffy:encode(JSON), Req), Opts}.
