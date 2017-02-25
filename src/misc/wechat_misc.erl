-module(wechat_misc).

-include_lib("xmerl/include/xmerl.hrl").
-include("define_logger.hrl").


-export([xml2proplists/1, proplists2xml/1,
         sign/2]).

xml2proplists(Binary) when is_binary(Binary) ->
    xml2proplists(binary_to_list(Binary));
xml2proplists(String) ->
    {Xml, _} = xmerl_scan:string(String),
    [val(E) || #xmlElement{}=E <- Xml#xmlElement.content].

val(#xmlElement{
       name = N, 
       content = [#xmlText{value = V}|_]}) ->    
    {N, unicode:characters_to_binary(V)}.

proplists2xml(Proplists) ->
    Body = lists:foldl(fun({_Name0, undefined}, Acc) ->
                               Acc;
                          ({Name0, Value0}, Acc) ->
                               Name = thing_to_binary(Name0),
                               Value = thing_to_binary(Value0),
                               B = <<"<", Name/binary, "><![CDATA[", Value/binary, "]]></", Name/binary, ">" >>,
                               <<Acc/binary, B/binary>>
                       end, <<>>, Proplists),
    <<"<xml>", Body/binary, "</xml>">>.

thing_to_binary(X) when is_list(X) -> unicode:characters_to_binary(X);
thing_to_binary(X) when is_integer(X) -> integer_to_binary(X);
thing_to_binary(X) when is_float(X)   -> float_to_binary(X);
thing_to_binary(X) when is_atom(X)    -> atom_to_binary(X, utf8);
thing_to_binary(X) when is_binary(X)    -> X.	%Assumed to be a string

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_list(X)    -> X.	%Assumed to be a string

sign(Key, Proplists0) ->    
    Proplists1 = lists:foldl(fun({sign, _V}, Acc) ->
                                     Acc;
                                ({K, V}, Acc) ->
                                     if
                                         V =:= undefined orelse
                                         V =:= "" orelse
                                         V =:= <<>> ->
                                             Acc;
                                         true ->
                                             [{atom_to_binary(K, utf8), V}|Acc]
                                     end
                             end, [], Proplists0),
    Proplists2 = lists:keysort(1, Proplists1),

    Bin0 = lists:foldl(fun({K, V0}, Acc) ->
                               V = thing_to_binary(V0),
                               <<Acc/binary, K/binary, "=", V/binary, "&">>
                       end, <<>>, Proplists2),

    Bin = <<Bin0/binary, "key=", Key/binary>>,
    Md5 = list_to_binary(string:to_upper(crypto_misc:str_md5(Bin))),
    Md5.

%% t() ->
%%     Proplists = [{appid,	<<"wxd930ea5d5a258f4f">>},    
%%                  {mch_id,	<<"10000100">>},
%%                  {device_info,	<<"1000">>},
%%                  {body,	<<"test">>},
%%                  {nonce_str,	<<"ibuaiVcKdpRxkhJA">>}],
%%     proplists2xml(add_sign(<<"192006250b4c09247ec02edce69f6a2d">>, Proplists)).
