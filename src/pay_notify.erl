-module(pay_notify).
-export([start/2]).

-include("common.hrl").

test() ->
    start("http://127.0.0.1:8089/test/pay_notify", "http://127.0.0.1:8089/test/pay_notify").

start(Url, Body) ->
    proc_lib:spawn(fun() ->
                           loop(Url, Body, 1)
                   end).
                
%% 15/15/30/180/1800/1800/1800/1800/3600          
sec(1) ->
    15;
sec(2) ->
    15;
sec(3) ->
    30;
sec(4) ->
    180;
sec(5) ->
    1800;
sec(6) ->
    1800;
sec(7) ->
    1800;
sec(8) ->
    1800;
sec(9) ->
    3600;
sec(_) ->
    stop.

loop(Url, Body, Times) ->
    ?DEBUG("Url ~s, Body ~s, Times ~p", [Url, Body, Times]),
    Ret = case ibrowse:send_req(Url, [], post, Body, [], 5000) of
              {ok, "200", _, RecBody} ->
                  case catch wechat_misc:xml2proplists(RecBody) of
                      {'EXIT', _} ->
                          fail;
                      Proplists ->
                          case proplists:get_value(return_code, Proplists) of
                              <<"SUCCESS">> ->
                                  ok;
                              _ ->
                                  fail
                          end
                  end;
              Other ->
                  ?DEBUG("Other ~p", [Other]),
                  fail
          end,
    if
        Ret =:= ok ->
            stop;
        true ->
            case sec(Times) of
                stop ->
                    stop;
                Sec -> 
                    timer:sleep(Sec*1000),
                    loop(Url, Body, Times+1)
            end
    end.
    

