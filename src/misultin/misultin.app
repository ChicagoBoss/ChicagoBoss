{application, misultin,
[
	{description, "Lightweight HTTP(s) and Websockets Server Library"},
	{vsn, "0.6.0"},
	{modules, [misultin, misultin_req, misultin_socket, misultin_utility, misultin_websocket, misultin_ws]},
	{registered, [misultin]},
	{env, []},
	{applications, [kernel, stdlib]}
]}.
