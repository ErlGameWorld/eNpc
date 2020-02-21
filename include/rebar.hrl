-define(FAIL, rebarUtils:abort()).
-define(ABORT(Str, Args), rebarUtils:abort(Str, Args)).

-define(INFO(Str, Args), rebar:log(info, Str, Args)).
-define(WARN(Str, Args), rebar:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar:log(error, Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
