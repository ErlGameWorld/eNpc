-module(rebar).

-export([
   main/1,
   log/3
]).

-include("rebar.hrl").

-ifndef(BUILD_TIME).
-define(BUILD_TIME, "undefined").
-endif.

-ifndef(VCS_INFO).
-define(VCS_INFO, "undefined").
-endif.

-ifndef(OTP_INFO).
-define(OTP_INFO, "undefined").
-endif.

-define(DEFAULT_JOBS, 3).

main(Args) ->
   case catch (run(Args)) of
      ok ->
         ok;
      rebar_abort ->
         rebarUtils:delayedHalt(1);
      Error ->
         %% Nothing should percolate up from rebar_core;
         %% Dump this error to console
         io:format("Uncaught error in rebar_core: ~p\n", [Error]),
         rebarUtils:delayedHalt(1)
   end.

log(Level, Format, Args) ->
   {ok, LimitLevel} = application:get_env(erlNpc, log_level),
   case levelInt(LimitLevel) >= levelInt(Level) of
      true ->
         io:format(destination(Level), Format, Args);
      false ->
         ok
   end.

levelInt(info) -> 2;
levelInt(warn) -> 1;
levelInt(error) -> 0.

destination(error) -> standard_error;
destination(_) -> group_leader().

run(["help"]) ->
   usage(),
   help(compile);
run(["help" | RawCommands]) ->
   lists:foreach(fun help/1, [list_to_atom(C) || C <- RawCommands]);
run(["version"]) ->
   ok = loadRebarApp(),
   %% Display vsn and build time info
   version();
run(RawArgs) ->
   ok = loadRebarApp(),
   Args = parseArgs(RawArgs),
   BaseConfig = initConfig(Args),
   {BaseConfig1, Cmds} = saveOptions(BaseConfig, Args),
   runAux(BaseConfig1, Cmds).

loadRebarApp() ->
   %% Pre-load the rebar app so that we get default configuration
   case application:load(erlNpc) of
      ok ->
         ok;
      {error, {already_loaded,erlNpc}} ->
         ok;
      _ ->
         rebarUtils:delayedHalt(1)
   end.

help(compile) ->
   rebarNpCompiler:info(help, compile);
help(clean) ->
   rebarNpCompiler:info(help, clean);
help(Command) ->
   ?CONSOLE("erlNpc no help available for \"~p\"~n", [Command]).

parseArgs([]) ->
   {[], []};
parseArgs(["-h" | _]) ->
   usage(),
   help(compile),
   rebarUtils:delayedHalt(0);
parseArgs(["--help" | _]) ->
   usage(),
   help(compile),
   rebarUtils:delayedHalt(0);
parseArgs(["-v" | _]) ->
   version(),
   rebarUtils:delayedHalt(0);
parseArgs(["--version" | _]) ->
   version(),
   rebarUtils:delayedHalt(0);
parseArgs(["-c", FileName | Rest]) ->
   {Opts, NonOpts} = parseArgs(Rest),
   {[{config, FileName} | Opts], NonOpts};
parseArgs(["--config", FileName | Rest]) ->
   parseArgs(["-c", FileName | Rest]);
parseArgs([NonOpt | Rest]) ->
   {Opts, NonOpts} = parseArgs(Rest),
   {Opts, [NonOpt | NonOpts]}.

usage() ->
   ?CONSOLE("erlNpc [-hv] [-c CONFIG_FILE] COMMAND [COMMAND ...]~n~n", []).

initConfig({Options, _NonOptArgs}) ->
   %% If $HOME/.rebar/config exists load and use as global config
   GlobalConfigFile = filename:join([os:getenv("HOME"), ".rebar", "config"]),
   GlobalConfig =
      case filelib:is_regular(GlobalConfigFile) of
         true ->
            rebarConfig:new(GlobalConfigFile);
         false ->
            rebarConfig:new()
      end,

   %% Set the rebar config to use
   GlobalConfig1 =
      case proplists:get_value(config, Options) of
         undefined ->
            GlobalConfig;
         Conf ->
            rebarConfig:setGlobal(GlobalConfig, config, Conf)
      end,

   BaseConfig = rebarConfig:baseConfig(GlobalConfig1),

   %% Keep track of how many operations we do, so we can detect bad commands
   BaseConfig1 = rebarConfig:setXconf(BaseConfig, operations, 0),
   %% Initialize vsn cache
   rebarUtils:initVsnCache(BaseConfig1).

initConfig_1(BaseConfig) ->
   %% Determine the location of the rebar executable; important for pulling
   %% resources out of the escript
   ScriptName = filename:absname(escript:script_name()),
   BaseConfig1 = rebarConfig:setXconf(BaseConfig, escript, ScriptName),
   %% Note the top-level directory for reference
   AbsCwd = filename:absname(rebarUtils:getCwd()),
   rebarConfig:setXconf(BaseConfig1, base_dir, AbsCwd).

runAux(BaseConfig, Commands) ->
   %% Make sure crypto is running
   case crypto:start() of
      ok -> ok;
      {error, {already_started, crypto}} -> ok
   end,

   %% Convert command strings to atoms
   CommandAtoms = [list_to_atom(C) || C <- Commands],

   BaseConfig1 = initConfig_1(BaseConfig),

   %% Make sure we're an app directory
    AppFile = "",
   %%    case rebarUtils:isAppDir() of
   %%       {true, AppFile0} ->
   %%          AppFile0;
   %%       false ->
   %%          rebarUtils:delayedHalt(1)
   %%    end,

   % Setup our environment
   BaseConfig2 = setupEnvs(BaseConfig1, [rebarNpCompiler]),
   %% Process each command, resetting any state between each one
   lists:foreach(
      fun(Command) ->
         %processCommand(Command, BaseConfig2, AppFile)""
         processCommand(Command, BaseConfig2, AppFile)
      end, CommandAtoms).

setupEnvs(Config, Modules) ->
   lists:foldl(
      fun(Module, CfgAcc) ->
         Env = Module:setupEnv(CfgAcc),
         rebarConfig:saveEnv(CfgAcc, Module, Env)
      end, Config, Modules).

processCommand(compile, Config, AppFile) ->
   rebarNpCompiler:compile(Config, AppFile);

processCommand(clean, Config, AppFile) ->
   rebarNpCompiler:clean(Config, AppFile);

processCommand(Other, _, _) ->
   ?CONSOLE("Unknown command: ~s~n", [Other]),
   rebarUtils:delayedHalt(1).

saveOptions(Config, {Options, NonOptArgs}) ->
   GlobalDefines = proplists:get_all_values(defines, Options),
   Config1 = rebarConfig:setXconf(Config, defines, GlobalDefines),
   filterFlags(Config1, NonOptArgs, []).

%% show version information and halt
version() ->
   {ok, Vsn} = application:get_key(erlNpc, vsn),
   ?CONSOLE("erlNpc ~s ~s ~s ~s\n", [Vsn, ?OTP_INFO, ?BUILD_TIME, ?VCS_INFO]).

%% Seperate all commands (single-words) from flags (key=value) and store
%% values into the rebar_config global storage.
filterFlags(Config, [], Commands) ->
   {Config, lists:reverse(Commands)};
filterFlags(Config, [Item | Rest], Commands) ->
   case string:tokens(Item, "=") of
      [Command] ->
         filterFlags(Config, Rest, [Command | Commands]);
      [KeyStr, RawValue] ->
         Key = list_to_atom(KeyStr),
         Value =
            case Key of
               verbose ->
                  list_to_integer(RawValue);
               _ ->
                  RawValue
            end,
         Config1 = rebarConfig:setGlobal(Config, Key, Value),
         filterFlags(Config1, Rest, Commands);
      Other ->
         ?CONSOLE("Ignoring command line argument: ~p\n", [Other]),
         filterFlags(Config, Rest, Commands)
   end.
