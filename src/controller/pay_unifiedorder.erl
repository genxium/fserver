-module(pay_unifiedorder).

-export([init/2]).

-export([msg/1]).


-include("common.hrl").
-include("define_pay.hrl").

%% http://127.0.0.1:8089/pay/unifiedorder
init(Req, Opts) ->
    {Reply, Req1} = 
        case cowboy_req:method(Req) of        
            <<"POST">> ->
                case cowboy_req:has_body(Req) of
                    true ->
                        {ok, XML, BodyReq} = cowboy_req:read_body(Req),
                        RecvProplists = wechat_misc:xml2proplists(XML),
                        ?DEBUG("~p", [RecvProplists]),
                        {handle(RecvProplists), BodyReq};
                    false ->
                        {?FAIL(<<"POST_DATA_EMPTY">>), Req}
                end;
            OtherMethod ->
                ?WARNING_MSG("Other Method ~ts~n", [OtherMethod]),
                {?FAIL(<<"REQUIRE_POST_METHOD">>), Req}
        end,
    case Reply of
        ?FAIL_REASON ->
            Proplists = [{return_code, <<"FAIL">>}, {return_msg, msg(Reason)}];            
        {ok, Proplists} ->
            ignore
    end,
	{ok, cowboy_req:reply(200, #{}, wechat_misc:proplists2xml(Proplists), Req1), Opts}.

handle(Proplists) ->
    case verify_data(Proplists) of
        ?FAIL_REASON ->
            ?FAIL_REASON;
        {Ret, AppData, AppId, MCHId, MCHSecret, OutTradeNo, TradeType} ->
            Reply1 = [{appid, AppId},
                     {mch_id, MCHId},
                     {device_info, proplists:get_value(device_info, Proplists)},
                     {nonce_str, ds_misc:rand_str(16)}],
            Reply2 = case Ret of
                         ok ->
                             PrePayId = erlang:unique_integer([positive, monotonic]),
                             ets:insert(prepay, #prepay{
                                                   id = {AppId, MCHId, OutTradeNo},
                                                   req_proplists = Proplists,
                                                   prepay_id = PrePayId,
                                                   timestamp = time_misc:unixtime()
                                                  }),
                             Domain = app_misc:get_env(domain, "127.0.0.1"),
                             Port = app_misc:get_env(port, "8089"),
                             Q = mochiweb_util:urlencode([{appid, AppId},{mch_id,MCHId},
                                                          {out_trade_no, OutTradeNo},
                                                          {prepay_id, PrePayId}]),
                             Url = mochiweb_util:urlunsplit_path({"http://" ++ Domain ++ ":" ++ integer_to_list(Port) ++ "/payment/confirmation", Q, []}),            
                             [{result_code, <<"SUCCESS">>},
                              {trade_type, TradeType},
                              {prepay_id, PrePayId},
                              {code_url, Url}];
                         {result_code, Code} ->
                             [{result_code, <<"FAIL">>}, {err_code, Code}]
                     end,
            Reply = Reply1 ++ Reply2,
            Sign = wechat_misc:sign(MCHSecret, Reply),
            {ok, [{sign, Sign}|Reply]}
    end.

verify_data(Proplists) ->
    case verify_data_type(Proplists, req_spec()) of
        false ->
            ?FAIL(<<"LACK_PARAMS">>);
        true ->            
            AppId = proplists:get_value(appid, Proplists),
            case data_misc:app_info(AppId) of
                {ok, AppData} ->
                    MCHSecret = ds_misc:json_get_v(AppData, <<"mch_secret">>),
                    ServerSign = wechat_misc:sign(MCHSecret, Proplists),
                    ?DEBUG("server sign ~s", [ServerSign]),
                    ClientSign =  proplists:get_value(sign, Proplists),
                    ?DEBUG("client sign ~s", [ClientSign]),
                    if
                        ServerSign =/= ClientSign ->
                           ?FAIL(<<"SIGNERROR">>);
                        true ->
                            ClientMCHId = proplists:get_value(mch_id, Proplists),
                            ServerMCHId = ds_misc:json_get_v(AppData, <<"mch_id">>),
                            TradeType = proplists:get_value(trade_type, Proplists),
                            OutTradeNo = proplists:get_value(out_trade_no, Proplists),
                            
                            Ret = if
                                      ClientMCHId =/= ServerMCHId ->
                                          {result_code, <<"MCHID_NOT_EXIST">>};
                                      true ->
                                          if
                                              TradeType =/= <<"NATIVE">> ->
                                                  {result_code, <<"NOT_SUPPORT_FUNC">>};
                                              true ->
                                                  case re:run(OutTradeNo, "^[a-zA-Z0-9_-]{0,32}$") of
                                                      {match, _} ->
                                                          case ets:lookup(prepay, {AppId, ClientMCHId, OutTradeNo}) of
                                                              [_|_] ->
                                                                  {result_code, <<"OUT_TRADE_NO_USED">>};
                                                              [] ->
                                                                  ok
                                                          end;
                                                      nomatch ->
                                                          {result_code, <<"OUT_TRADE_NO_ERR">>}
                                                  end
                                          end
                                  end,
                            {Ret, AppData, AppId, ClientMCHId, MCHSecret, OutTradeNo, TradeType}
                    end;
                _ ->
                    ?FAIL(<<"APPID_NOT_EXIST">>)
            end
    end.

req_spec() ->
    [
     {appid, {string, 32}},
     {mch_id, {string, 32}},
     {device_info, {optional_string, 32}},
     {nonce_str, {string, 32}},
     {sign, {string, 32}},
     {sign_type, {optional_string, 32}},
     {body, {string, 128}},
     {detail, {optional_string, 6000}},
     {attach, {optional_string, 127}},
     {out_trade_no, {string, 32}},
     {fee_type, {optional_string, 16}},
     {total_fee, int},
     {spbill_create_ip, {string, 16}},
     {time_start, {optional_string, 14}},
     {time_expire, {optional_string, 14}},
     {goods_tag, {optional_string, 32}},
     {notify_url, {string, 256}},
     {trade_type, {string, 16}},
     {product_id, {optional_string, 32}},
     {limit_pay, {optional_string, 32}},
     {openid, {optional_string, 128}}     
    ].

is_int(String) ->
    case catch binary_to_integer(String) of
        {'EXIT', _} ->
            false;
        _ ->
            true
    end.

verify_data_type(Proplists, Spec) ->
    lists:all(fun({Field, Type}) ->
                      Value = proplists:get_value(Field, Proplists),
                      Bool = case Type of
                                 int ->
                                     is_int(Value);
                                 {string, Len} ->
                                     is_binary(Value) andalso
                                         size(Value) =< Len;
                                 optional_int ->
                                     if
                                         Value =:= undefined ->
                                             true;
                                         true ->
                                             is_int(Value)
                                     end;
                                 {optional_string, Len} ->
                                     Value =:= undefined orelse
                                         is_binary(Value) andalso
                                         size(Value) =< Len
                             end,
                      if
                          Bool =:= false ->
                              ?DEBUG("k=~p v=~p", [Field, Value]);
                          true ->
                              ignore
                      end,
                      Bool
              end, Spec).

msg(<<"NOT_SUPPORT_FUNC">>) ->
    <<"尚未支持的功能"/utf8>>;
msg(<<"SUCCESS">>) ->
    <<"SUCCESS">>;
msg(<<"NOAUTH">>) ->
	<<"商户无此接口权限"/utf8>>;
msg(<<"NOTENOUGH">>) ->
	<<"余额不足"/utf8>>;
msg(<<"ORDERPAID">>) ->
	<<"商户订单已支付"/utf8>>;
msg(<<"ORDERCLOSED">>) ->
	<<"订单已关闭"/utf8>>;
msg(<<"SYSTEMERROR">>) ->
	<<"系统错误"/utf8>>;
msg(<<"APPID_NOT_EXIST">>) ->
	<<"APPID不存在"/utf8>>;
msg(<<"MCHID_NOT_EXIST">>) ->
	<<"MCHID不存在"/utf8>>;
msg(<<"APPID_MCHID_NOT_MATCH">>) ->
	<<"appid和mch_id不匹配"/utf8>>;
msg(<<"LACK_PARAMS">>) ->
	<<"缺少参数"/utf8>>;
msg(<<"OUT_TRADE_NO_USED">>) ->
	<<"商户订单号重复"/utf8>>;
msg(<<"SIGNERROR">>) ->
	<<"签名错误"/utf8>>;
msg(<<"XML_FORMAT_ERROR">>) ->
	<<"XML格式错误"/utf8>>;
msg(<<"REQUIRE_POST_METHOD">>) ->
	<<"请使用post方法"/utf8>>;
msg(<<"POST_DATA_EMPTY">>) ->
	<<"post数据为空"/utf8>>;
msg(<<"NOT_UTF8">>) ->
	<<"编码格式错误"/utf8>>;	
msg(<<"OUT_TRADE_NO_ERR">>) ->
	<<"订单号格式错误"/utf8>>.
