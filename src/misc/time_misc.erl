-module(time_misc).
-export([
         unixtime/0, milli_seconds/0,
         now_format/0,
         timestamp_format/1,
         current/0
        ]).

-export([datetime_to_timestamp/1, 
         datetime_to_timestamp/2,
         yyyymmdd/0, yyyymmdd/1, yyyymm/1]).

-export([db_datetime_to_timestamp/1, 
         db_timestamp_format/1]).

-export([day_diff_by_timestamp/2]).

-export([is_same_date/2,
         is_same_week/2,
         is_same_month/2
        ]).

-export([get_seconds_to_tomorrow/0, get_seconds_to_tomorrow_4/0]).

-export([today_0h_timestamp/0, timestamp_0h/1]).

-export([day_of_the_week/0, monday_0h_timestamp/0]).

-export([datetime_format/1]).

-export([time_list_to_timestamp/1]).

-export([timestamp_to_localtime/1]).

-export([date_add/3]).

-export([day_begin_end/1,
         five_min_interval_ago/1]).

-include("define_time.hrl").
-include("define_logger.hrl").

current() -> 
    os:timestamp().

now_format() ->
    datetime_format(calendar:now_to_local_time(current())).

timestamp_format(TimeStamp) ->
    datetime_format(timestamp_to_localtime(TimeStamp)).

%% 取得当前的unix时间戳，秒级
unixtime() ->
    {M, S, _} = current(),
    M * 1000000 + S.

timestamp_to_localtime(Seconds) ->
    DateTime = calendar:gregorian_seconds_to_datetime(
                 Seconds + ?DIFF_SECONDS_0000_1900),
    calendar:universal_time_to_local_time(DateTime).

datetime_to_timestamp(Date, Time) ->
    datetime_to_timestamp({Date, Time}).

%% 由于时间{Hour, Min, Sec}本身具有时区，所以转化成时间戳的时候，要减掉东八区
datetime_to_timestamp({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    OrealTime =  calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
    ZeroSecs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}),
    ZeroSecs - OrealTime - ?TIME_ZONE_SECONDS.

%% 2014-09-09 16:14:43	
db_timestamp_format({datetime, DateTime}) ->
    datetime_format(DateTime).

datetime_format({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    <<(integer_to_binary(Year))/binary, "-", 
      (one_to_two(Month))/binary, "-", 
      (one_to_two(Day))/binary, " ",
      (one_to_two(Hour))/binary, ":", 
      (one_to_two(Minute))/binary,  ":", 
      (one_to_two(Second))/binary>>.

%% emysql的timestamp数据
db_datetime_to_timestamp({datetime, DateTime}) ->
    datetime_to_timestamp(DateTime).

one_to_two(One) 
  when One < 10 ->
    <<"0", (integer_to_binary(One))/binary>>;
one_to_two(Two) ->
    integer_to_binary(Two).

%% 判断是否同一天
is_same_date(undefined, _) ->
    false;
is_same_date(_, undefined) ->
    false;
is_same_date(TimeStamp1, TimeStamp2) ->
    %% 需要考虑时区问题
    Day1 = (TimeStamp1 + ?TIME_ZONE_SECONDS) div ?ONE_DAY_SECONDS,
    Day2 = (TimeStamp2 + ?TIME_ZONE_SECONDS) div ?ONE_DAY_SECONDS,
    Day1 =:= Day2.

is_same_week(TimeStamp1, TimeStamp2) ->
    {Date1, _} = timestamp_to_localtime(TimeStamp1),
    {Date2, _} = timestamp_to_localtime(TimeStamp2),    
    calendar:iso_week_number(Date1) =:= calendar:iso_week_number(Date2).

is_same_month(TimeStamp1, TimeStamp2) ->
    {{Year1, Month1, _}, _} = timestamp_to_localtime(TimeStamp1),
    {{Year2, Month2, _}, _} = timestamp_to_localtime(TimeStamp2),    
    (Year1 =:= Year2 andalso Month1 =:= Month2).


day_diff_by_timestamp(TimeStamp1, TimeStamp2) ->
    %% 需要考虑时区问题
    Day1 = (TimeStamp1 + ?TIME_ZONE_SECONDS) div ?ONE_DAY_SECONDS,
    Day2 = (TimeStamp2 + ?TIME_ZONE_SECONDS) div ?ONE_DAY_SECONDS,
    ?DEBUG("Day1 ~p, Day2 ~p~n", [Day1, Day2]),
    Day1 - Day2.

%% 获取距离明天0点的秒数
get_seconds_to_tomorrow() ->
    Now = unixtime(),
    PassSeconds = (Now + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS,
    ?ONE_DAY_SECONDS - PassSeconds.

get_seconds_to_tomorrow_4() ->
    %% 将东八区，4点平移到0点，然后4-28就是一个周期
    Now = unixtime(),
    PassSeconds = (Now + ?TIME_ZONE_SECONDS - 4*?ONE_HOUR_SECONDS) rem ?ONE_DAY_SECONDS,
    ?ONE_DAY_SECONDS - PassSeconds.

%% 今天0点时间戳
today_0h_timestamp() ->
    Now = unixtime(),
    timestamp_0h(Now).

timestamp_0h(Timestamp) ->
    PassSeconds = (Timestamp + ?TIME_ZONE_SECONDS) rem ?ONE_DAY_SECONDS,
    Timestamp - PassSeconds.

%% 这周一
monday_0h_timestamp() ->
    today_0h_timestamp() - (day_of_the_week() - 1)*?ONE_DAY_SECONDS.

yyyymmdd() ->
    {Date, _} = calendar:now_to_local_time(current()),
    yyyymmdd(Date).

yyyymmdd({Y, M, D}) ->
    integer_to_list(Y*10000 + M * 100 + D).
    
yyyymm({Y, M, _D}) ->
    integer_to_list(Y*100 + M).

day_of_the_week() ->
    {Date, _} = calendar:now_to_local_time(current()),
    calendar:day_of_the_week(Date).

time_list_to_timestamp([Y, M, D, HH, MM, SS]) ->
    datetime_to_timestamp({{Y, M, D}, {HH, MM, SS}});
time_list_to_timestamp(_) ->
    undefined.

milli_seconds() ->
    erlang:system_time(milli_seconds).

%% stolen from edate
date_add(YMD, N, day) ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(YMD) + N).

day_begin_end(Date) ->
    DayBegin = datetime_to_timestamp(Date, {0,0,0}),
    DayEnd = DayBegin + ?ONE_DAY_SECONDS - 1,
    {DayBegin, DayEnd}.

five_min_interval_ago(Timestamp) ->
    {Timestamp - ?FIVE_MINITE_SECONDS,
     Timestamp - 1}.

