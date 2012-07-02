{ application, pillow, [
  { description, "Lightweight Erlang TCP server to buffer/cushion volatile key-value streams" },
  { vsn, "0.0.1" },
  { modules,
    [ pillow,
      pillow_dumper,
      pillow_pusher,
	    pillow_server
	  ]
	},
  { registered, [] },
  { applications, [kernel, stdlib] },
  { mod, { pillow, [7000, 7001] } },
  { env, [] }
]}.