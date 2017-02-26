-module(cowboy_misc).

-export([not_qs_empty/1]).
-export([any_qs/1]).

-export([verify_type/1]).


-export([to_str/1]).

%% -export([parse_parameter/2]).

-include("define_logger.hrl").

not_qs_empty(List) ->
    lists:all(fun (<<>>) ->
                      false;
                  (undefined)->
                      false;
                  (_) ->
                      true
              end, List).

any_qs(List) ->
    lists:any(fun (<<>>) ->
                      false;
                  (undefined)->
                      false;
                  (_) ->
                      true
              end, List).

to_str(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_str({Unit, X}) when (Unit==bytes orelse Unit==time_ms) andalso is_list(X) ->
    try list_to_integer(X) of
        B -> 
            to_str({Unit,B})
    catch error:badarg -> 
            X
    end;
to_str({bytes, B}) ->
    KB = B div 1024,
    MB = KB div 1024,
    GB = MB div 1024,
    if
        GB > 10 -> 
            integer_to_list(GB) ++ " GB";
        MB > 10 -> 
            integer_to_list(MB) ++ " MB";
        KB > 0 -> 
            integer_to_list(KB) ++ " kB";
        true -> 
            integer_to_list(B) ++ " B"
    end;
to_str({time_ms, MS}) ->
    S = MS div 1000,
    Min = S div 60,
    Hours = Min div 60,
    Days = Hours div 24,
    if
        Days > 0 ->
            integer_to_list(Days) ++ " Days, " ++ integer_to_list(Hours rem 24) ++ " Hours ";
        Hours > 0 -> 
            integer_to_list(Hours) ++ " Hours, " ++ integer_to_list(Min rem 60) ++ " Mins";
        Min > 0 -> 
            integer_to_list(Min) ++ " Mins";
        true -> 
            integer_to_list(S) ++ " Secs"
    end;

to_str({func, {F,A}}) when is_atom(F), is_integer(A) ->
    lists:concat([F, "/", A]);
to_str({func, {F,'_'}}) when is_atom(F) ->
    atom_to_list(F);
to_str({{format,Fun},Value}) when is_function(Fun) ->
    Fun(Value);
to_str({A, B}) when is_atom(A), is_atom(B) ->
    lists:concat([A, ":", B]);
to_str({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
    lists:concat([M, ":", F, "/", A]);
to_str(Value) when is_list(Value) ->
    case lists:all(fun(X) -> is_integer(X) end, Value) of
        true -> Value;
        false ->
            lists:foldl(fun(X, Acc) ->
                                to_str(X) ++ " " ++ Acc end,
                        "", Value)
    end;
to_str(Port) when is_port(Port) ->
    erlang:port_to_list(Port);
to_str(Pid) when is_pid(Pid) ->
    pid_to_list(Pid);
to_str(No) when is_integer(No) ->
    integer_to_list(No);
to_str(Term) ->
    io_lib:format("~w", [Term]).

%% 给各种支付回调使用
%% parse_parameter(KeyValues, ParameterList) ->
%%     parse_parameter(KeyValues, ParameterList, []).

%% parse_parameter(_, [], Acc) ->
%%     {ok, lists:reverse(Acc)};
%% parse_parameter(KeyValues, [{Parameter, DataType} | ParameterList], Acc) ->
%%     Value = proplists:get_value(Parameter, KeyValues),
%%     if
%%         (Value =:= undefined orelse
%%          Value =:= <<>>) ->
%%             ?DEBUG("Key ~p null~n", [Parameter]),
%%             ?FAIL(?INFO_PARAMETER_MISS);
%%         true ->
%%             parse_parameter(KeyValues, ParameterList, [decoder(DataType, Value)|Acc])            
%%     end.

decoder(int, Value) ->
    erlang:binary_to_integer(Value);
decoder(atom, Value) ->
    erlang:binary_to_existing_atom(Value, utf8);
decoder(string, Value) ->
    erlang:binary_to_list(Value);
decoder(json, Value) ->
    jiffy:decode(Value);
decoder(_, Value) ->
    Value.

verify_type(List) ->
    lists:all(fun({Value, Type}) ->
                      case Type of
                          optional_int ->
                              Value =:= undefined orelse
                                  is_integer(Value);
                          int ->
                              is_integer(Value);
                          string ->
                              is_binary(Value);
                          int_list ->
                              is_list(Value) andalso
                                  lists:all(fun(E) ->
                                                    is_integer(E)
                                            end, Value);
                          list ->
                              is_list(Value);
                          Other ->
                              ?DEBUG("Other ~p", [Other]),
                              false
                      end
              end, List).
