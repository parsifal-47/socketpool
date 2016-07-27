-module(tcp_echo).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _Args) ->
    {ok, erlang:spawn(socketpool, start_listener,
        [5555, 100, fun(_State, Bin) -> {ok, Bin} end, fun() -> ok end])}.

stop(_State) -> ok.