{application, eNpc,
   [
      {description, "eNpc: Erlang Native Compiler"},
      {vsn, "0.1.0"},
      {registered, []},
      {applications, [kernel, stdlib]},
      {modules, [eNpc, npRMain, npRConfig, npRCompiler, npRUtils]},
      {licenses, ["Apache 2.0"]},
      {links, []},
      {env, [{log_level, warn}]}
   ]
}.
