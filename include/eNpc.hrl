-define(FAIL, npRUtils:abort()).
-define(ABORT(Str, Args), npRUtils:abort(Str, Args)).

-define(INFO(Str, Args), npRMain:log(info, Str, Args)).
-define(WARN(Str, Args), npRMain:log(warn, Str, Args)).
-define(ERROR(Str, Args), npRMain:log(error, Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
