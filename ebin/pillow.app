{application,pillow,
             [{description,"Lightweight TCP server for cushioning high-volume key-value streams"},
              {vsn,"0.0.1"},
              {modules,[pillow,pillow_export,pillow_inflow,pillow_server,
                        pillow_stream]},
              {registered,[]},
              {applications,[kernel,stdlib,statsd]},
              {mod,{pillow,[4096,4097,4098]}},
              {env,[]}]}.
