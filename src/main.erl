-module(main).

-include("common.hrl"). 

-export([
         start/0,
         started/0,
         is_running/0, is_running/1,
         stop_and_halt/0
        ]).

start() ->
    application:load('fakeoauth2server'),
    log_misc:start_lager(app_misc:get_env(log_level, info)),
    application:start(reloader),
    ?DEBUG("start app fakeoauth2server", []),
    app_misc:start(?APP_NAME).

started() -> 
    {ok, is_running()}.

is_running() -> 
    is_running(node()).

is_running(Node) -> 
    case rpc:call(Node, erlang, whereis, ['fakeoauth2server']) of
        {badrpc, _} -> 
            false;
        undefined -> 
            false;
        P when is_pid(P) -> 
            true
    end.
    
stop_and_halt() ->
    local_info_msg("Halting Erlang VM~n", []),
    init:stop().

local_info_msg(Format, Args) ->
    GL = group_leader(),
    group_leader(whereis(user), self()),
    try
        error_logger:info_msg(Format, Args)
    after
        group_leader(GL, self())
    end.
