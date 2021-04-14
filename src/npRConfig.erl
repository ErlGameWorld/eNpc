-module(npRConfig).

-include("eNpc.hrl").

-export([
   new/0,
   new/1,
   baseConfig/1,
   consultFile/1,
   get/3,
   getLocal/3,
   getList/3,
   setGlobal/3,
   getGlobal/3,
   saveEnv/3,
   getEnv/2,
   setXconf/3,
   getXconf/2,
   getXconf/3
]).

-type key() :: atom().
-type rebarDict() :: dict:dict(term(), term()).

-record(config, {
   dir :: file:filename(),
   opts = [] :: list(),
   globals = newGlobals() :: rebarDict(),
   envs = newEnv() :: rebarDict(),
   %% cross-directory/-command config
   skipDirs = newSkipDirs() :: rebarDict(),
   xconf = newXconf() :: rebarDict()
}).

-opaque config() :: #config{}.
-export_type([config/0]).

-define(DEFAULT_NAME, "rebar.config").

-spec baseConfig(config()) -> config().
baseConfig(GlobalConfig) ->
   ConfName = npRConfig:getGlobal(GlobalConfig, config, ?DEFAULT_NAME),
   new(GlobalConfig, ConfName).

-spec new() -> config().
new() ->
   #config{dir = npRUtils:getCwd()}.

-spec new(file:filename() | config()) -> config().
new(ConfigFile) when is_list(ConfigFile) ->
   case consultFile(ConfigFile) of
      {ok, Opts} ->
         #config{dir = npRUtils:getCwd(),
            opts = Opts};
      Other ->
         ?ABORT("Failed to load ~s: ~p~n", [ConfigFile, Other])
   end;
new(#config{opts = Opts0, globals = Globals, skipDirs = SkipDirs, xconf = Xconf}) ->
   new(#config{opts = Opts0, globals = Globals, skipDirs = SkipDirs, xconf = Xconf},
      ?DEFAULT_NAME).

-spec get(config(), key(), term()) -> term().
get(Config, Key, Default) ->
   proplists:get_value(Key, Config#config.opts, Default).

-spec getList(config(), key(), term()) -> term().
getList(Config, Key, Default) ->
   get(Config, Key, Default).

-spec getLocal(config(), key(), term()) -> term().
getLocal(Config, Key, Default) ->
   proplists:get_value(Key, localOpts(Config#config.opts, []), Default).

-spec setGlobal(config(), key(), term()) -> config().
setGlobal(Config, jobs = Key, Value) when is_list(Value) ->
   setGlobal(Config, Key, list_to_integer(Value));
setGlobal(Config, jobs = Key, Value) when is_integer(Value) ->
   NewGlobals = dict:store(Key, erlang:max(1, Value), Config#config.globals),
   Config#config{globals = NewGlobals};
setGlobal(Config, Key, Value) ->
   NewGlobals = dict:store(Key, Value, Config#config.globals),
   Config#config{globals = NewGlobals}.

-spec getGlobal(config(), key(), term()) -> term().
getGlobal(Config, Key, Default) ->
   case dict:find(Key, Config#config.globals) of
      error ->
         Default;
      {ok, Value} ->
         Value
   end.

-spec consultFile(file:filename()) -> term().
consultFile(File) ->
   case filename:extension(File) of
      ".script" ->
         consultAndEval(removeScriptExt(File), File);
      _ ->
         Script = File ++ ".script",
         case filelib:is_regular(Script) of
            true ->
               consultAndEval(File, Script);
            false ->
               file:consult(File)
         end
   end.

-spec saveEnv(config(), module(), nonempty_list()) -> config().
saveEnv(Config, Mod, Env) ->
   NewEnvs = dict:store(Mod, Env, Config#config.envs),
   Config#config{envs = NewEnvs}.

-spec getEnv(config(), module()) -> term().
getEnv(Config, Mod) ->
   dict:fetch(Mod, Config#config.envs).

-spec setXconf(config(), term(), term()) -> config().
setXconf(Config, Key, Value) ->
   NewXconf = dict:store(Key, Value, Config#config.xconf),
   Config#config{xconf = NewXconf}.

-spec getXconf(config(), term()) -> term().
getXconf(Config, Key) ->
   {ok, Value} = dict:find(Key, Config#config.xconf),
   Value.

-spec getXconf(config(), term(), term()) -> term().
getXconf(Config, Key, Default) ->
   case dict:find(Key, Config#config.xconf) of
      error ->
         Default;
      {ok, Value} ->
         Value
   end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec new(config(), file:filename()) -> config().
new(ParentConfig, ConfName) ->
   %% Load terms from rebar.config, if it exists
   Dir = npRUtils:getCwd(),
   ConfigFile = filename:join([Dir, ConfName]),
   Opts0 = ParentConfig#config.opts,
   Opts = case consultFile(ConfigFile) of
             {ok, Terms} ->
                %% Found a config file with some terms. We need to
                %% be able to distinguish between local definitions
                %% (i.e. from the file in the cwd) and inherited
                %% definitions. To accomplish this, we use a marker
                %% in the proplist (since order matters) between
                %% the new and old defs.
                Terms ++ [local] ++
                   [Opt || Opt <- Opts0, Opt /= local];
             {error, enoent} ->
                [local] ++
                [Opt || Opt <- Opts0, Opt /= local];
             Other ->
                ?ABORT("Failed to load ~s: ~p\n", [ConfigFile, Other])
          end,

   ParentConfig#config{dir = Dir, opts = Opts}.

-spec consultAndEval(file:filename(), file:filename()) -> {ok, term()}.
consultAndEval(File, Script) ->
   ConfigData = tryConsult(File),
   file:script(Script, bs([{'CONFIG', ConfigData}, {'SCRIPT', Script}])).

-spec removeScriptExt(file:filename()) -> file:filename().
removeScriptExt(F) ->
      "tpircs." ++ Rev = lists:reverse(F),
   lists:reverse(Rev).

-spec tryConsult(file:filename()) -> term().
tryConsult(File) ->
   case file:consult(File) of
      {ok, Terms} ->
         Terms;
      {error, enoent} ->
         [];
      {error, Reason} ->
         ?ABORT("Failed to read config file ~s: ~p~n", [File, Reason])
   end.

-type bs_vars() :: [{term(), term()}].
-spec bs(bs_vars()) -> bs_vars().
bs(Vars) ->
   lists:foldl(fun({K, V}, Bs) ->
      erl_eval:add_binding(K, V, Bs)
               end, erl_eval:new_bindings(), Vars).

-spec localOpts(list(), list()) -> list().
localOpts([], Acc) ->
   lists:reverse(Acc);
localOpts([local | _Rest], Acc) ->
   lists:reverse(Acc);
localOpts([Item | Rest], Acc) ->
   localOpts(Rest, [Item | Acc]).

-spec newGlobals() -> rebarDict().
newGlobals() -> dict:new().

-spec newEnv() -> rebarDict().
newEnv() -> dict:new().

-spec newSkipDirs() -> rebarDict().
newSkipDirs() -> dict:new().

-spec newXconf() -> rebarDict().
newXconf() -> dict:new().
