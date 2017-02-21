-module(wechat_xml).

-include_lib("xmerl/include/xmerl.hrl").

-export([to_proplists/1, to_xml/1]).

to_proplists(Binary) when is_binary(Binary) ->
    to_proplists(binary_to_list(Binary));
to_proplists(String) ->
    {Xml, _} = xmerl_scan:string(String),
    [val(E) || #xmlElement{}=E <- Xml#xmlElement.content].

val(#xmlElement{
       name = N, 
       content = [#xmlText{value = V}|_]}) ->    
    {N, V}.

to_xml(Proplists) ->
    Body = lists:foldl(fun({Name0, Value0}, Acc) ->
                               Name = thing_to_binary(Name0),
                               Value = thing_to_binary(Value0),
                               B = <<"<", Name/binary, "><![CDATA[", Value/binary, "]]></", Name/binary, ">" >>,
                               <<Acc/binary, B/binary>>
                       end, <<>>, Proplists),
    <<"<xml>", Body/binary, "</xml>">>.


thing_to_binary(X) when is_integer(X) -> integer_to_binary(X);
thing_to_binary(X) when is_float(X)   -> float_to_binary(X);
thing_to_binary(X) when is_atom(X)    -> atom_to_binary(X, utf8);
thing_to_binary(X) when is_binary(X)    -> X.	%Assumed to be a string

