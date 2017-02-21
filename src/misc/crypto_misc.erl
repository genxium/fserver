-module(crypto_misc).
-export([sha256/1]).

-export([md5/1, str_md5/1, md5_file/1]).

-export([hmac_sha1/2]).

sha256(Data) ->
    ds_misc:to_hex(crypto:hash(sha256, io_lib:format("~w", [Data]))).

md5(S) ->
    %% io:format("~w~n", [S]),
    ds_misc:to_hex(erlang:md5(S)).

md5_file(File) ->
    %% io:format("md5 ~s~n", [File]),
    {ok, Bin} = file:read_file(File),
    md5(Bin).

str_md5(S) ->
    %% io:format("~w~n", [S]),
    ds_misc:to_hex(binary_to_list(erlang:md5(S))).

hmac_sha1(Key, S) ->
    ds_misc:to_hex(crypto:hmac(sha, Key, S)).
