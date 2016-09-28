-ifndef(DEFINE_LOGGER_HRL).
-define(DEFINE_LOGGER_HRL, true).

-define(PRINT(Format, Args),
        io:format("(~s:~p:~p:~p) : " ++ Format ++ "~n", 
                  [time_misc:now_format(), self(), ?MODULE, ?LINE] ++ Args)).
%% -define(PRINT(Format, Args),
%%         io:format(Format, Args)).
-define(DEBUG,
        lager:debug).
-define(INFO_MSG,
        lager:info).
-define(NOTICE,
        lager:notice).
-define(WARNING_MSG,
        lager:warning).
-define(ERROR_MSG,
        lager:error).
-define(CRITICAL,
        lager:critical).
-define(ALERT,
        lager:alert).
-define(EMERGENCY,
        lager:emergency).
-endif.

