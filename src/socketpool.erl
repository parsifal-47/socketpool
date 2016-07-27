-module(socketpool).

-export([start_listener/4]).

% "reader" is internal function
-export([reader/2]).

-define(Options, [
    binary,
    {backlog, 128},
    {active, false},
    {buffer, 65536},
    {keepalive, true},
    {reuseaddr, true}
]).

-define(SmallTimeout, 50).

start_listener(Port, PoolSize, ProtocolFun, InitialStateFun) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, ?Options),
    Readers = [erlang:spawn(?MODULE, reader, [ProtocolFun, InitialStateFun]) || _X <- lists:seq(1, PoolSize)],
    accept(ListenSocket, Readers, []).

accept(ListenSocket, [], Reversed) -> accept(ListenSocket, lists:reverse(Reversed), []);
accept(ListenSocket, [Reader | Rest], Reversed) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} -> Reader ! Socket, accept(ListenSocket, Rest, [Reader | Reversed]);
        {error, closed} -> ok
    end.

reader(ProtocolFun, InitialStateFun) -> reader(ProtocolFun, InitialStateFun, []).

read_sockets([], Rev, _) -> lists:reverse(Rev);
read_sockets([{S, State} | T], Rev, ProtocolFun) ->
    case gen_tcp:recv(S, 0, 0) of
        {ok, Binary} ->
            {State2, Reply} = ProtocolFun(State, Binary),
            gen_tcp:send(S, Reply), read_sockets(T, [{S, State2} | Rev], ProtocolFun);
        {error, timeout} -> read_sockets(T, [{S, State} | Rev], ProtocolFun);
        _ -> gen_tcp:close(S), read_sockets(T, Rev, ProtocolFun)
    end.

reader(ProtocolFun, InitialStateFun, Sockets) ->
    Sockets2 = read_sockets(Sockets, [], ProtocolFun),
    receive
        S -> reader(ProtocolFun, InitialStateFun, [{S, InitialStateFun()} | Sockets2])
    after ?SmallTimeout -> reader(ProtocolFun, InitialStateFun, Sockets)
    end.
