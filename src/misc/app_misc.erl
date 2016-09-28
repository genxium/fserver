-module(app_misc).

-export([start/1]).

-export([start_ok/2]).

-export([ensure_application_loaded/0]).
-export([get_module/1, version/0]).

-export([project_root_dir/0]).

-export([get_env/1, get_env/2]).

-include("common.hrl").

start(App) ->
    start_ok(App, application:start(App)).

start_ok(_App, ok) -> ok;
start_ok(_App, {error, {already_started, _App}}) -> ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = start(Dep),
    start(App);
start_ok(App, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).

ensure_application_loaded() ->
    case application:load(?APP_NAME) of
        ok ->
            ok;
        {error, {already_loaded, _}} ->
            ok
    end.

get_module(App) ->
    application:get_key(App, modules).

get_vsn(App) ->
    application:get_key(App, vsn).

version() ->
    {ok, Vsn} = get_vsn(?APP_NAME),
    Vsn.

project_root_dir() ->
    CodeDir = code:lib_dir('fakeoauth2server'),
    Dir = filename:dirname(CodeDir),
    case filename:basename(Dir) of
        "lib" ->
            filename:dirname(filename:dirname(filename:dirname(Dir)));
        "apps" ->
            filename:dirname(Dir)
    end.        

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Def) ->
    ProjectRootDir = project_root_dir(),
    Opts = case file:consult(filename:join(ProjectRootDir, "config/app.config")) of
               {error,enoent} ->
                   [];
               {ok, Opts0} ->
                   Opts0
           end,
   case lists:keyfind(Key, 1, Opts) of
       false ->
           Def;
       {_, Value} ->
           Value
   end.

