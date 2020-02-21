{application,erlNpc,
             [{description,"erlNpc: Erlang Native Compiler"},
              {vsn,"0.1.0"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {modules,[erlNpc,rebar,rebarConfig,rebarNpCompiler,rebarUtils]},
              {licenses,["Apache 2.0"]},
              {links,[]},
              {env,[{log_level,warn}]}]}.
