-module(data_misc).

-export([app_info/1]).

-include("common.hrl").

app_info(AppId) ->
    ProjectRootDir = app_misc:project_root_dir(),
    DataFile = filename:join([ProjectRootDir, "data/data.json"]),
    {ok, Binary} = file:read_file(DataFile),
    case catch jiffy:decode(Binary) of
        {error, _} ->
            ?RET(?EC_ERROR_JSON);
        Array ->
            case ds_misc:json_filter(Array, <<"app_id">>, AppId) of                
                [] ->
                    ?RET(?EC_ERROR_APP_ID);
                [Obj|_] ->
                    {ok, Obj}
            end                   
    end.
