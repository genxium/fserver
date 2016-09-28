-module(data_misc).

-export([read/1]).

read(File) ->
    ProjectRootDir = app_misc:project_root_dir(),
    DataFile = filename:join([ProjectRootDir, "data", File]),
    {ok, Binary} = file:read_file(DataFile),
   case catch jiffy:decode(Binary) of
       {error, _} = Error ->
           Error;
       JSON ->
           {ok, JSON}
   end.
    

    

