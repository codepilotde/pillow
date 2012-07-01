{ application, pillow, [
  { description, "Lightweight Erlang TCP server to buffer/cushion volatile key-value streams" },
  { vsn,"0.0.1" },
  { modules, [
	  pillow,
	  pillow_cushion,
	  pillow_server,
	  pillow_storage
	]},
  { registered, [pillow_cushion, pillow_storage] },
  { applications, [kernel, stdlib] },
  { mod, { pillow, [7000, 7001] } },
  { env, [] }
]}.