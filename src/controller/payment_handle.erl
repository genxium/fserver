-module(payment_handle).

-export([init/2]).

-include("common.hrl").
-include("define_pay.hrl").
-include("define_time.hrl").


init(Req, Opts) ->
    #{
      bindings := Bindings
     } = Req,
    Act = proplists:get_value(act, Bindings),
    NewReq = handle(Act, Req),
	{ok, NewReq, Opts}.

handle(<<"confirmation">>, Req) ->
    QS = cowboy_req:parse_qs(Req),
    ?DEBUG("confirmation qs ~p", [QS]),
    AppId = proplists:get_value(<<"appid">>, QS),
    MCHId = proplists:get_value(<<"mch_id">>, QS),
    OutTradeNo = proplists:get_value(<<"out_trade_no">>, QS),
    PrePayId = proplists:get_value(<<"prepay_id">>, QS),

    Body = case data_misc:app_info(AppId) of               
               {ok, AppData} ->
                   case ets:lookup(prepay, {AppId, MCHId, OutTradeNo}) of
                       [] ->
                           <<"订单已关闭"/utf8>>;
                       [#prepay{
                           req_proplists = ReqProplists
                          }] ->
                           File = filename:join(app_misc:project_root_dir(), "web/views/payment_confirmation.mustache"),
                           Template = bbmustache:parse_file(File),
                           AccInfoList = ds_misc:json_get_v(AppData, <<"acc_info">>),
                           AccInfoMap = lists:foldl(fun(AccInfo, Acc) ->
                                                            OpenId = ds_misc:json_get_v(AccInfo, <<"openid">>),
                                                            HeadImgUrl= ds_misc:json_get_v(AccInfo, <<"headimgurl">>),
                                                            Nickname = ds_misc:json_get_v(AccInfo, <<"nickname">>),
                                                            [#{"openid" => OpenId, "headimgurl" => HeadImgUrl, "nickname" => Nickname}|Acc]
                                                    end, [], AccInfoList),
                           
                           bbmustache:compile(Template, #{"accInfo" => AccInfoMap,
                                                          "app_id" => AppId,
                                                          "mch_id" => MCHId,
                                                          "out_trade_no" => OutTradeNo,
                                                          "prepay_id" => PrePayId,
                                                          "goods_desc" => proplists:get_value(body, ReqProplists)})
                   end;
               _Ret ->
                   <<"APPID不存在"/utf8>>
           end,
    cowboy_req:reply(200, #{}, Body, Req);
handle(<<"authorization">>, Req) ->
    {Reply, Req1} = 
                case cowboy_req:has_body(Req) of
                    true ->
                        {ok, Body, BodyReq} = cowboy_req:read_body(Req),
                        JSON = jiffy:decode(Body),                        
                        Map = authorization(JSON),
                        {Map, BodyReq};
                    false ->
                        {?RET(?EC_ERROR_PARAMS), Req}
                end,
    cowboy_req:reply(200, #{}, jiffy:encode(Reply), Req1).

authorization(JSON) ->
    ?DEBUG("pay auth JSON ~p", [JSON]),
    AppId = ds_misc:json_get_v(JSON, <<"app_id">>),
    MCHId = ds_misc:json_get_v(JSON, <<"mch_id">>),
    OutTradeNo = ds_misc:json_get_v(JSON, <<"out_trade_no">>),
    PrePayId = ds_misc:json_get_v(JSON, <<"prepay_id">>),
    OpenId = ds_misc:json_get_v(JSON, <<"openid">>),
    ReturnCode = ds_misc:json_get_v(JSON, <<"indended_return_code">>, <<"SUCCESS">>),
    ResultCode = ds_misc:json_get_v(JSON, <<"indended_result_code">>),
    ErrorCode = ds_misc:json_get_v(JSON, <<"intended_err_code">>),
    
    FailF = fun (ErrorCode0) ->
                    #{
                  return_code => ReturnCode,
                  result_code => <<"FAIL">>,
                  err_code  => ErrorCode0
                 }                      
            end,
    case ets:lookup(prepay, {AppId, MCHId, OutTradeNo}) of
        [] ->
            FailF(<<"ORDERCLOSED">>);
        [#prepay{
            req_proplists = Proplists
           } = PrePay] ->
            Map = if
                      ResultCode =:= <<"SUCCESS">> ->
                          Now = time_misc:unixtime(),
                          if                        
                              PrePay#prepay.timestamp + ?TWO_HOUR_SECONDS < Now ->
                                  FailF(<<"ORDERCLOSED">>);
                              PrePayId#prepay.prepay_id =/= PrePayId ->
                                  FailF(<<"ORDERCLOSED">>);
                              true ->
                                  ets:delete(prepay, {AppId, MCHId, OutTradeNo}),
                                  #{
                                     return_code => ReturnCode,
                                     result_code => ResultCode
                                   }
                          end;                
                      true ->
                          #{
                        return_code => ReturnCode,
                        result_code => ResultCode,
                        err_code  => ErrorCode
                       }
                  end,
            Map2 = #{
              appid => proplists:get_value(appid, Proplists),
              mch_id => proplists:get_value(mch_id, Proplists),
              device_info => proplists:get_value(device_info, Proplists),
              nonce_str => ds_misc:rand_str(16),
              openid => OpenId,
              trade_type => proplists:get_value(trade_type, Proplists),
              bank_type => <<"CMC">>,
              total_fee => proplists:get_value(total_fee, Proplists),
              fee_type => <<"CNY">>,
              cash_fee => 0,
              transaction_id => ds_misc:rand_str(16),
              out_trade_no => proplists:get_value(out_trade_no, Proplists),
              attach => proplists:get_value(attach, Proplists),
              time_end => yyyyMMddHHmmss()
             },
            SendProplists = maps:to_list(maps:merge(Map, Map2)),
            case data_misc:app_info(AppId) of
                {ok, AppData} ->
                    MCHSecret = ds_misc:json_get_v(AppData, <<"mch_secret">>),
                    Sign = wechat_misc:sign(MCHSecret, SendProplists),
                    Body = wechat_misc:proplists2xml([{sign, Sign}|SendProplists]),
                    URL = proplists:get_value(notify_url, Proplists),
                    pay_notify:start(binary_to_list(URL), Body);
                _ ->
                    ?WARNING_MSG("not app id ~p", [AppId]),
                    ignore
            end,
            Map
    end.
    
yyyyMMddHHmmss() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(os:timestamp()),
    <<(integer_to_binary(Year))/binary,  
      (one_to_two(Month))/binary,  
      (one_to_two(Day))/binary, 
      (one_to_two(Hour))/binary, 
      (one_to_two(Minute))/binary,
      (one_to_two(Second))/binary>>.

one_to_two(One) 
  when One < 10 ->
    <<"0", (integer_to_binary(One))/binary>>;
one_to_two(Two) ->
    integer_to_binary(Two).


