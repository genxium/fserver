-module(http_handler_404).

-export([init/2]).

-include("common.hrl").

init(Req, Opts) ->
    #{
      path := Path
     } = Req,
    Body = ["404 Not Found: \"", Path,
            "\" is not the path you are looking for.\n"],
	{ok, cowboy_req:reply(404, #{}, Body, Req), Opts}.
