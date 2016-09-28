-module(main_control).

-export([start/0]).

-include("common.hrl").

commands_desc() ->
    [
     %% 供main显示使用
     {"-c live", "交互式启动进程"},
     {"-c start", "守护形式启动进程"},
     {"-c attach", "attach到已启动的节点"},     
     {"-c stop", "停止进程"},
     {"-c started", "节点进程是否存在"}].
opt_spec_list() ->
    [
     {cmd, $c, "cmd", {atom, undefined}, "操作指令"},
     {help, $h, "help", undefined, "显示帮助，然后退出"},
     {nodename, undefined, "nodename", {atom, 'fakeoauth2server@127.0.0.1'}, "管理节点"},
     {log_level, undefined, "log_level", {atom, info}, "日志级别"},
     {config, undefined, "config", {string, ""}, "Config配置"}
    ].
usage() ->
    getopt:usage(opt_spec_list(), "fakeoauth2server", "<command> [<args>]", commands_desc()),
    halt().

parse_arguments(CmdLine) ->
    %% io:format("~p~n", [CmdLine]),
    case getopt:parse(opt_spec_list(), CmdLine) of
        {ok, {Opts, []}} ->
            case proplists:get_bool(help, Opts) of
                true ->
                    usage();
                false ->                    
                    {ok, Opts}
            end;
        Error ->
            io:format("Error ~p~n", [Error]),
            no_command
    end.

start() ->
    ok = io:setopts([{encoding, unicode}]),
    Opts = case parse_arguments(init:get_plain_arguments()) of
               {ok, Res}  -> 
                   Res;
               no_command ->
                   usage()
           end,
    Command = proplists:get_value(cmd, Opts),
    NodeName = proplists:get_value(nodename, Opts),
    case net_adm:ping(NodeName) of
        pong ->
            case catch action(Command, NodeName, Opts) of
                ok ->
                    io:format("done.~n", []),
                    halt(0);
                {ok, Info} ->
                    io:format("~p~n", [Info]),
                    halt(0);        
                Other ->
                    io:format("other result ~~>> ~ts~n", [Other]),
                    halt(1)
            end;
        pang ->
            io:format("~p nodedown~n", [NodeName]),
            halt(0)
    end.

action(stop, NodeName, _Opts) ->
    io:format("Stopping and halting node ~p~n", [NodeName]),
    call(NodeName, {main, stop_and_halt, []});
action(started, NodeName, _Opts) ->
    call(NodeName, {main, started, []});
action(reload, NodeName, _Opts) ->
    {ok, call(NodeName, {reloader, reload_all_changed, []})};
action(log_level, NodeName, Opts) ->
    LogLevel = proplists:get_value(log_level, Opts),
    {ok, call(NodeName, {log_misc, set_loglevel, [LogLevel]})};
action(Command, _NodeName, Opts) ->
    io:format("Command: ~p Opts: ~p~n", [Command, Opts]),
    invalid_command.

call(Node, {Mod, Fun, Args}) ->
    rpc_call(Node, Mod, Fun, Args).

rpc_call(Node, Mod, Fun, Args) ->
    rpc:call(Node, Mod, Fun, Args, infinity).
