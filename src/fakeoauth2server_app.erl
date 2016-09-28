-module('fakeoauth2server_app').

-behaviour(application).

-export([start/2
        ,stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Pid} = 'fakeoauth2server_sup':start_link(),
    register('fakeoauth2server', self()),
    start_http(),
    {ok, Pid}.

start_http() ->
    Port = app_misc:get_env(port, 8089), 
    PathsList = [                 
                 {"/auth/fake/login", auth_fake_login, []},
                 {"/sns/oauth2/access_token", sns_oauth2_access_token, []},
                 {"/sns/userinfo", sns_userinfo, []},
                 {"/[...]", http_handler_404, []}
                ],
    Dispatch = cowboy_router:compile([{'_', PathsList}]),
    {ok, _} = cowboy:start_clear(http, 4,
                                 [{port, Port}],
                                 #{
                                   env => #{
                                     dispatch => Dispatch                                             
                                    }
                                  }
                                ),
    ok.
stop(_State) ->
    ok.

