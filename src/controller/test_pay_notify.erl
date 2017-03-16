-module(test_pay_notify).

-export([init/2]).

-include("common.hrl").

init(Req, Opts) ->
    ?DEBUG("~p", [Req]),
    {Proplists, Req1} = case cowboy_req:has_body(Req) of
                             true ->
                                 {ok, Body, BodyReq} = cowboy_req:read_body(Req),
                                 ?DEBUG("Body ~s", [Body]),
                                 {[{return_code, <<"SUCCESS">>}], BodyReq};
                             false ->
                                 {[{return_code, <<"FAIL">>}], Req}
                         end,
	{ok, cowboy_req:reply(200, #{}, wechat_misc:proplists2xml(Proplists), Req1), Opts}.
