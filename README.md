# Library for fast tcp reads

To implement echo protocol:

	socketpool:start_listener(5555, 100, fun(_State, Bin) -> {ok, Bin} end, fun() -> ok end)

50% faster than straight reads.

To check echo protocol example:

	cd examples/tcp_echo
	make start
	telnet 5555