-module(rebarNpCompiler).

-include("rebar.hrl").

-export([
   compile/2,
   clean/2

]).

%% for internal use only
-export([
   setupEnv/1,
   info/2
]).

-record(spec, {
   type :: 'drv' | 'exe',
   link_lang :: 'cc' | 'cxx',
   target :: file:filename(),
   sources = [] :: [file:filename(), ...],
   objects = [] :: [file:filename(), ...],
   opts = [] :: list() | []
}).

compile(Config, AppFile) ->
   case getSpecs(Config, AppFile) of
      [] ->
         ok;
      Specs ->
         SharedEnv = rebarConfig:getEnv(Config, ?MODULE),

         %% Compile each of the sources
         NewBins = compileSources(Config, Specs, SharedEnv),

         %% Make sure that the target directories exist

         ?INFO("Using specs ~p\n", [Specs]),
         lists:foreach(
            fun(#spec{target = Target}) ->
               ok = filelib:ensure_dir(Target)
            end, Specs),

         %% Only relink if necessary, given the Target
         %% and list of new binaries
         lists:foreach(
            fun(#spec{target = Target, objects = Bins, opts = Opts, link_lang = LinkLang}) ->
               AllBins = [sets:from_list(Bins), sets:from_list(NewBins)],
               Intersection = sets:intersection(AllBins),
               case needsLink(Target, sets:to_list(Intersection)) of
                  true ->
                     LinkTemplate = selectLinkTemplate(LinkLang, Target),
                     Env = proplists:get_value(env, Opts, SharedEnv),
                     Cmd = expandCommand(LinkTemplate, Env, string:join(Bins, " "), Target),
                     rebarUtils:sh(Cmd, [{env, Env}]);
                  false ->
                     ?INFO("Skipping relink of ~s\n", [Target]),
                     ok
               end
            end, Specs)
   end.

clean(Config, AppFile) ->
   case getSpecs(Config, AppFile) of
      [] ->
         ok;
      Specs ->
         lists:foreach(
            fun(#spec{target = Target, objects = Objects}) ->
               rebarUtils:deleteEach([Target]),
               rebarUtils:deleteEach(Objects),
               rebarUtils:deleteEach(portDeps(Objects))
            end, Specs)
   end,
   ok.

setupEnv(Config) ->
   setupEnv(Config, defaultEnv(Config)).

%% ===================================================================
%% Internal functions
%% ===================================================================

info(help, compile) ->
   infoHelp("Build port sources");
info(help, clean) ->
   infoHelp("Delete port build results").

infoHelp(Description) ->
   ?CONSOLE(
      "~s.~n"
      "~n"
      "Valid rebar.config options:~n"
      "port_specs - Erlang list of tuples of the forms~n"
      "             {ArchRegex, TargetFile, Sources, Options}~n"
      "             {ArchRegex, TargetFile, Sources}~n"
      "             {TargetFile, Sources}~n"
      "~n"
      "             Examples:~n"
      "             ~p~n"
      "~n"
      "port_env - Erlang list of key/value pairs which will control~n"
      "           the environment when running the compiler and linker.~n"
      "           Variables set in the surrounding system shell are taken~n"
      "           into consideration when expanding port_env.~n"
      "~n"
      "           By default, the following variables are defined:~n"
      "           CC       - C compiler~n"
      "           CXX      - C++ compiler~n"
      "           CFLAGS   - C compiler~n"
      "           CXXFLAGS - C++ compiler~n"
      "           LDFLAGS  - Link flags~n"
      "           ERL_CFLAGS  - default -I paths for erts and ei~n"
      "           ERL_LDFLAGS - default -L and -lerl_interface -lei~n"
      "           DRV_CFLAGS  - flags that will be used for compiling~n"
      "           DRV_LDFLAGS - flags that will be used for linking~n"
      "           EXE_CFLAGS  - flags that will be used for compiling~n"
      "           EXE_LDFLAGS - flags that will be used for linking~n"
      "           ERL_EI_LIBDIR - ei library directory~n"
      "           DRV_CXX_TEMPLATE      - C++ command template~n"
      "           DRV_CC_TEMPLATE       - C command template~n"
      "           DRV_LINK_TEMPLATE     - C Linker command template~n"
      "           DRV_LINK_CXX_TEMPLATE - C++ Linker command template~n"
      "           EXE_CXX_TEMPLATE      - C++ command template~n"
      "           EXE_CC_TEMPLATE       - C command template~n"
      "           EXE_LINK_TEMPLATE     - C Linker command template~n"
      "           EXE_LINK_CXX_TEMPLATE - C++ Linker command template~n"
      "~n"
      "           Note that if you wish to extend (vs. replace) these variables,~n"
      "           you MUST include a shell-style reference in your definition.~n"
      "           e.g. to extend CFLAGS, do something like:~n"
      "~n"
      "           {port_env, [{\"CFLAGS\", \"$CFLAGS -MyOtherOptions\"}]}~n"
      "~n"
      "           It is also possible to specify platform specific options~n"
      "           by specifying a triplet where the first string is a regex~n"
      "           that is checked against Erlang's system architecture string.~n"
      "           e.g. to specify a CFLAG that only applies to x86_64 on linux~n"
      "           do:~n"
      "           {port_env, [{\"x86_64.*-linux\", \"CFLAGS\",~n"
      "                        \"$CFLAGS -X86Options\"}]}~n"
      "~n"
      "Cross-arch environment variables to configure toolchain:~n"
      "  REBAR_TARGET_ARCH to set the tool chain name to use~n"
      "  REBAR_TARGET_ARCH_WORDSIZE (optional - "
      "if CC fails to determine word size)~n"
      "  fallback word size is 32~n"
      "  REBAR_TARGET_ARCH_VSN (optional - "
      "if a special version of CC/CXX is requested)~n",
      [
         Description,
         {port_specs, [{"priv/so_name.so", ["c_src/*.c"]},
            {"linux", "priv/hello_linux", ["c_src/hello_linux.c"]},
            {"linux", "priv/hello_linux", ["c_src/*.c"], [{env, []}]}]}
      ]).

%% set REBAR_DEPS_DIR and ERL_LIBS environment variables
defaultEnv(Config) ->
   BaseDir = rebarUtils:baseDir(Config),
   DepsDir0 = rebarConfig:getXconf(Config, deps_dir, "deps"),
   DepsDir = filename:dirname(filename:join([BaseDir, DepsDir0, "dummy"])),

   %% include rebar's DepsDir in ERL_LIBS
   Separator = case os:type() of
                  {win32, nt} -> ";";
                  _ -> ":"
               end,

   ERL_LIBS = case os:getenv("ERL_LIBS") of
                 false ->
                    {"ERL_LIBS", DepsDir};
                 PrevValue ->
                    {"ERL_LIBS", DepsDir ++ Separator ++ PrevValue}
              end,
   [
      {"REBAR_DEPS_DIR", DepsDir},
      ERL_LIBS
   ].

setupEnv(Config, ExtraEnv) ->
   %% Extract environment values from the config (if specified) and
   %% merge with the default for this operating system. This enables
   %% max flexibility for users.
   DefaultEnv = filterEnv(defaultEnv(), []),

   %% Get any port-specific envs; use port_env first and then fallback
   %% to port_envs for compatibility
   RawPortEnv = rebarConfig:getList(
      Config,
      port_env,
      rebarConfig:getList(Config, port_envs, [])),

   PortEnv = filterEnv(RawPortEnv, []),
   Defines = getDefines(Config),
   OverrideEnv = Defines ++ PortEnv ++ filterEnv(ExtraEnv, []),
   RawEnv = applyDefaults(osEnv(), DefaultEnv) ++ OverrideEnv,
   expandVarsLoop(mergeEachVar(RawEnv, [])).

getDefines(Config) ->
   RawDefines = rebarConfig:getXconf(Config, defines, []),
   Defines = string:join(["-D" ++ D || D <- RawDefines], " "),
   [{"ERL_CFLAGS", "$ERL_CFLAGS " ++ Defines}].

replaceExtension(File, NewExt) ->
   OldExt = filename:extension(File),
   replaceExtension(File, OldExt, NewExt).

replaceExtension(File, OldExt, NewExt) ->
   filename:rootname(File, OldExt) ++ NewExt.

%%
%% == compile and link ==
%%

compileSources(Config, Specs, SharedEnv) ->
   {NewBins, Db} =
      lists:foldl(
         fun(#spec{sources = Sources, type = Type, opts = Opts}, Acc) ->
            Env = proplists:get_value(env, Opts, SharedEnv),
            compileEach(Config, Sources, Type, Env, Acc)
         end, {[], []}, Specs),
   %% Rewrite clang compile commands database file only if something
   %% was compiled.
   case NewBins of
      [] ->
         ok;
      _ ->
         {ok, ClangDbFile} = file:open("compile_commands.json", [write]),
         ok = io:fwrite(ClangDbFile, "[~n", []),
         lists:foreach(fun(E) -> ok = io:fwrite(ClangDbFile, E, []) end, Db),
         ok = io:fwrite(ClangDbFile, "]~n", []),
         ok = file:close(ClangDbFile)
   end,
   NewBins.

compileEach(_Config, [], _Type, _Env, {NewBins, CDB}) ->
   {lists:reverse(NewBins), lists:reverse(CDB)};
compileEach(Config, [Source | Rest], Type, Env, {NewBins, CDB}) ->
   Ext = filename:extension(Source),
   Bin = replaceExtension(Source, Ext, ".o"),
   Template = selectCompileTemplate(Type, compiler(Ext)),
   Cmd = expandCommand(Template, Env, Source, Bin),
   CDBEnt = cdbEntry(Source, Cmd, Rest),
   NewCDB = [CDBEnt | CDB],
   case needsCompile(Source, Bin) of
      true ->
         ShOpts = [{env, Env}, return_on_error, {use_stdout, false}],
         execCompiler(Config, Source, Cmd, ShOpts),
         compileEach(Config, Rest, Type, Env, {[Bin | NewBins], NewCDB});
      false ->
         ?INFO("Skipping ~s\n", [Source]),
         compileEach(Config, Rest, Type, Env, {NewBins, NewCDB})
   end.

%% Generate a clang compilation db entry for Src and Cmd
cdbEntry(Src, Cmd, SrcRest) ->
   %% Omit all variables from cmd, and use that as cmd in
   %% CDB, because otherwise clang-* will complain about it.
   CDBCmd = string:join(
      lists:filter(
         fun("$" ++ _) -> false;
            (_) -> true
         end,
         string:tokens(Cmd, " ")),
      " "),

   Cwd = rebarUtils:getCwd(),
   %% If there are more source files, make sure we end the CDB entry
   %% with a comma.
   Sep = case SrcRest of
            [] -> "~n";
            _ -> ",~n"
         end,
   %% CDB entry
   ?FMT("{ \"file\"      : ~p~n"
   ", \"directory\" : ~p~n"
   ", \"command\"   : ~p~n"
   "}~s",
      [Src, Cwd, CDBCmd, Sep]).

execCompiler(Config, Source, Cmd, ShOpts) ->
   case rebarUtils:sh(Cmd, ShOpts) of
      {error, {_RC, RawError}} ->
         AbsSource =
            case rebarUtils:processingBaseDir(Config) of
               true ->
                  Source;
               false ->
                  filename:absname(Source)
            end,
         ?CONSOLE("Compiling ~s\n", [AbsSource]),
         Error = re:replace(RawError, Source, AbsSource,
            [{return, list}, global]),
         ?CONSOLE("~s", [Error]),
         ?FAIL;
      {ok, Output} ->
         ?CONSOLE("Compiling ~s\n", [Source]),
         ?CONSOLE("~s", [Output])
   end.

needsCompile(Source, Bin) ->
   needsLink(Bin, [Source | binDeps(Bin)]).

%% NOTE: This relies on -MMD being passed to the compiler and returns an
%% empty list if the .d file is not available.  This means header deps are
%% ignored on win32.
binDeps(Bin) ->
   [DepFile] = portDeps([Bin]),
   case file:read_file(DepFile) of
      {ok, Deps} ->
         Ds = parseBinDeps(list_to_binary(Bin), Deps),
         Ds;
      {error, _Err} ->
         []
   end.

parseBinDeps(Bin, Deps) ->
   Sz = size(Bin),
   <<Bin:Sz/binary, ": ", X/binary>> = Deps,
   Ds = re:split(X, "\\s*\\\\\\R\\s*|\\s+", [{return, binary}]),
   [D || D <- Ds, D =/= <<>>].

needsLink(SoName, []) ->
   filelib:last_modified(SoName) == 0;
needsLink(SoName, NewBins) ->
   MaxLastMod = lists:max([filelib:last_modified(B) || B <- NewBins]),
   case filelib:last_modified(SoName) of
      0 ->
         true;
      Other ->
         MaxLastMod >= Other
   end.

%%
%% == port_specs ==
%%

getSpecs(Config, AppFile) ->
   Specs =
      case rebarConfig:getLocal(Config, port_specs, []) of
         [] ->
            %% No spec provided. Construct a spec
            %% from old-school so_name and sources
            [portSpecFromLegacy(Config, AppFile)];
         PortSpecs ->
            Filtered = filterPortSpecs(PortSpecs),
            OsType = os:type(),
            [getPortSpec(Config, OsType, Spec) || Spec <- Filtered]
      end,
   [S || S <- Specs, S#spec.sources /= []].

portSpecFromLegacy(Config, AppFile) ->
   %% Get the target from the so_name variable
   Target =
      case rebarConfig:get(Config, so_name, undefined) of
         undefined ->
            %% Generate a sensible default from app file
            {_, AppName} = rebarUtils:appName(Config, AppFile),
            filename:join("priv", lists:concat([AppName, "_drv.so"]));
         AName ->
            %% Old form is available -- use it
            filename:join("priv", AName)
      end,
   %% Get the list of source files from port_sources
   Sources = portSources(rebarConfig:getList(Config, port_sources, ["c_src/*.c"])),
   #spec{
      type = targetType(Target),
      link_lang = cc,
      target = maybeSwitchExtension(os:type(), Target),
      sources = Sources,
      objects = portObjects(Sources)
   }.

filterPortSpecs(Specs) ->
   [S || S <- Specs, filterPortSpec(S)].

filterPortSpec({ArchRegex, _, _, _}) ->
   rebarUtils:isArch(ArchRegex);
filterPortSpec({ArchRegex, _, _}) ->
   rebarUtils:isArch(ArchRegex);
filterPortSpec({_, _}) ->
   true.

getPortSpec(Config, OsType, {Target, Sources}) ->
   getPortSpec(Config, OsType, {undefined, Target, Sources, []});
getPortSpec(Config, OsType, {Arch, Target, Sources}) ->
   getPortSpec(Config, OsType, {Arch, Target, Sources, []});
getPortSpec(Config, OsType, {_Arch, Target, Sources, Opts}) ->
   SourceFiles = portSources(Sources),
   LinkLang =
      case lists:any(
         fun(Src) -> compiler(filename:extension(Src)) == "$CXX" end,
         SourceFiles)
      of
         true -> cxx;
         false -> cc
      end,
   ObjectFiles = portObjects(SourceFiles),
   #spec{
      type = targetType(Target),
      target = maybeSwitchExtension(OsType, Target),
      link_lang = LinkLang,
      sources = SourceFiles,
      objects = ObjectFiles,
      opts = portOpts(Config, Opts)
   }.

portSources(Sources) ->
   lists:flatmap(fun filelib:wildcard/1, Sources).

portObjects(SourceFiles) ->
   [replaceExtension(O, ".o") || O <- SourceFiles].

portDeps(SourceFiles) ->
   [replaceExtension(O, ".d") || O <- SourceFiles].

portOpts(Config, Opts) ->
   [portOpt(Config, O) || O <- Opts].

portOpt(Config, {env, Env}) ->
   {env, setupEnv(Config, Env)};
portOpt(_Config, Opt) ->
   Opt.

maybeSwitchExtension({win32, nt}, Target) ->
   switchToDllOrExe(Target);
maybeSwitchExtension(_OsType, Target) ->
   Target.

switchToDllOrExe(Target) ->
   case filename:extension(Target) of
      ".so" -> filename:rootname(Target, ".so") ++ ".dll";
      [] -> Target ++ ".exe";
      _Other -> Target
   end.


%% == port_env ==
%% Choose a compiler variable, based on a provided extension

compiler(".cc") -> "$CXX";
compiler(".cp") -> "$CXX";
compiler(".cxx") -> "$CXX";
compiler(".cpp") -> "$CXX";
compiler(".CPP") -> "$CXX";
compiler(".c++") -> "$CXX";
compiler(".C") -> "$CXX";
compiler(_) -> "$CC".


%% Given a list of {Key, Value} variables, and another list of default
%% {Key, Value} variables, return a merged list where the rule is if the
%% default is expandable expand it with the value of the variable list,
%% otherwise just return the value of the variable.

applyDefaults(Vars, Defaults) ->
   dict:to_list(
      dict:merge(
         fun(Key, VarValue, DefaultValue) ->
            case isExpandable(DefaultValue) of
               true ->
                  rebarUtils:expandEnvVariable(DefaultValue,
                     Key,
                     VarValue);
               false -> VarValue
            end
         end,
         dict:from_list(Vars),
         dict:from_list(Defaults))).

%%
%% Given a list of {Key, Value} environment variables, where Key may be defined
%% multiple times, walk the list and expand each self-reference so that we
%% end with a list of each variable singly-defined.
%%
mergeEachVar([], Vars) ->
   Vars;
mergeEachVar([{Key, Value} | Rest], Vars) ->
   Evalue =
      case orddict:find(Key, Vars) of
         error ->
            %% Nothing yet defined for this key/value.
            %% Expand any self-references as blank.
            rebarUtils:expandEnvVariable(Value, Key, "");
         {ok, Value0} ->
            %% Use previous definition in expansion
            rebarUtils:expandEnvVariable(Value, Key, Value0)
      end,
   mergeEachVar(Rest, orddict:store(Key, Evalue, Vars)).

%%
%% Give a unique list of {Key, Value} environment variables, expand each one
%% for every other key until no further expansions are possible.
%%
expandVarsLoop(Vars) ->
   expandVarsLoop(Vars, [], dict:from_list(Vars), 10).

expandVarsLoop(_Pending, _Recurse, _Vars, 0) ->
   ?ABORT("Max. expansion reached for ENV vars!\n", []);
expandVarsLoop([], [], Vars, _Count) ->
   lists:keysort(1, dict:to_list(Vars));
expandVarsLoop([], Recurse, Vars, Count) ->
   expandVarsLoop(Recurse, [], Vars, Count - 1);
expandVarsLoop([{K, V} | Rest], Recurse, Vars, Count) ->
   %% Identify the variables that need expansion in this value
   ReOpts = [global, {capture, all_but_first, list}, unicode],
   case re:run(V, "\\\${?(\\w+)}?", ReOpts) of
      {match, Matches} ->
         %% Identify the unique variables that need to be expanded
         UniqueMatches = lists:usort([M || [M] <- Matches]),

         %% For each variable, expand it and return the final
         %% value. Note that if we have a bunch of unresolvable
         %% variables, nothing happens and we don't bother
         %% attempting further expansion
         case expandKeysInValue(UniqueMatches, V, Vars) of
            V ->
               %% No change after expansion; move along
               expandVarsLoop(Rest, Recurse, Vars, Count);
            Expanded ->
               %% Some expansion occurred; move to next k/v but
               %% revisit this value in the next loop to check
               %% for further expansion
               NewVars = dict:store(K, Expanded, Vars),
               expandVarsLoop(Rest, [{K, Expanded} | Recurse],
                  NewVars, Count)
         end;

      nomatch ->
         %% No values in this variable need expansion; move along
         expandVarsLoop(Rest, Recurse, Vars, Count)
   end.

expandKeysInValue([], Value, _Vars) ->
   Value;
expandKeysInValue([Key | Rest], Value, Vars) ->
   NewValue =
      case dict:find(Key, Vars) of
         {ok, KValue} ->
            rebarUtils:expandEnvVariable(Value, Key, KValue);
         error ->
            Value
      end,
   expandKeysInValue(Rest, NewValue, Vars).

expandCommand(TmplName, Env, InFiles, OutFile) ->
   Cmd0 = proplists:get_value(TmplName, Env),
   Cmd1 = rebarUtils:expandEnvVariable(Cmd0, "PORT_IN_FILES", InFiles),
   rebarUtils:expandEnvVariable(Cmd1, "PORT_OUT_FILE", OutFile).

%%
%% Given a string, determine if it is expandable
%%
isExpandable(InStr) ->
   case re:run(InStr, "\\\$", [{capture, none}]) of
      match -> true;
      nomatch -> false
   end.

%%
%% Filter a list of env vars such that only those which match the provided
%% architecture regex (or do not have a regex) are returned.
%%
filterEnv([], Acc) ->
   lists:reverse(Acc);
filterEnv([{ArchRegex, Key, Value} | Rest], Acc) ->
   case rebarUtils:isArch(ArchRegex) of
      true ->
         filterEnv(Rest, [{Key, Value} | Acc]);
      false ->
         filterEnv(Rest, Acc)
   end;
filterEnv([{Key, Value} | Rest], Acc) ->
   filterEnv(Rest, [{Key, Value} | Acc]).

ertsDir() ->
   lists:concat([code:root_dir(), "/erts-", erlang:system_info(version)]).

osEnv() ->
   ReOpts = [{return, list}, {parts, 2}, unicode],
   Os = [list_to_tuple(re:split(S, "=", ReOpts)) ||
      S <- lists:filter(fun discardDepsVars/1, os:getenv())],
   %% Drop variables without a name (win32)
   [T1 || {K, _V} = T1 <- Os, K =/= []].

%%
%% To avoid having multiple repetitions of the same environment variables
%% (ERL_LIBS), avoid exporting any variables that may cause conflict with
%% those exported by the rebar_deps module (ERL_LIBS, REBAR_DEPS_DIR)
%%
discardDepsVars("ERL_LIBS=" ++ _Value) -> false;
discardDepsVars("REBAR_DEPS_DIR=" ++ _Value) -> false;
discardDepsVars(_Var) -> true.

selectCompileTemplate(drv, Compiler) ->
   selectCompileDrvTemplate(Compiler);
selectCompileTemplate(exe, Compiler) ->
   selectCompileExeTemplate(Compiler).

selectCompileDrvTemplate("$CC") -> "DRV_CC_TEMPLATE";
selectCompileDrvTemplate("$CXX") -> "DRV_CXX_TEMPLATE".

selectCompileExeTemplate("$CC") -> "EXE_CC_TEMPLATE";
selectCompileExeTemplate("$CXX") -> "EXE_CXX_TEMPLATE".

selectLinkTemplate(LinkLang, Target) ->
   case {LinkLang, targetType(Target)} of
      {cc, drv} -> "DRV_LINK_TEMPLATE";
      {cxx, drv} -> "DRV_LINK_CXX_TEMPLATE";
      {cc, exe} -> "EXE_LINK_TEMPLATE";
      {cxx, exe} -> "EXE_LINK_CXX_TEMPLATE"
   end.

targetType(Target) -> targetType_1(filename:extension(Target)).

targetType_1(".so") -> drv;
targetType_1(".dll") -> drv;
targetType_1("") -> exe;
targetType_1(".exe") -> exe.

erlInterfaceDir(Subdir) ->
   case code:lib_dir(erl_interface, Subdir) of
      {error, bad_name} ->
         throw({error, {erl_interface, Subdir, "code:lib_dir(erl_interface)"
         "is unable to find the erl_interface library."}});
      Dir -> Dir
   end.

defaultEnv() ->
   Arch = os:getenv("REBAR_TARGET_ARCH"),
   Vsn = os:getenv("REBAR_TARGET_ARCH_VSN"),
   [
      {"CC", getTool(Arch, Vsn, "gcc", "cc")},
      {"CXX", getTool(Arch, Vsn, "g++", "c++")},
      {"AR", getTool(Arch, "ar", "ar")},
      {"AS", getTool(Arch, "as", "as")},
      {"CPP", getTool(Arch, Vsn, "cpp", "cpp")},
      {"LD", getTool(Arch, "ld", "ld")},
      {"RANLIB", getTool(Arch, Vsn, "ranlib", "ranlib")},
      {"STRIP", getTool(Arch, "strip", "strip")},
      {"NM", getTool(Arch, "nm", "nm")},
      {"OBJCOPY", getTool(Arch, "objcopy", "objcopy")},
      {"OBJDUMP", getTool(Arch, "objdump", "objdump")},

      {"DRV_CXX_TEMPLATE",
         "$CXX -c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
      {"DRV_CC_TEMPLATE",
         "$CC -c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
      {"DRV_LINK_TEMPLATE",
         "$CC $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS -o $PORT_OUT_FILE"},
      {"DRV_LINK_CXX_TEMPLATE",
         "$CXX $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS -o $PORT_OUT_FILE"},
      {"EXE_CXX_TEMPLATE",
         "$CXX -c $CXXFLAGS $EXE_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
      {"EXE_CC_TEMPLATE",
         "$CC -c $CFLAGS $EXE_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
      {"EXE_LINK_TEMPLATE",
         "$CC $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS -o $PORT_OUT_FILE"},
      {"EXE_LINK_CXX_TEMPLATE",
         "$CXX $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS -o $PORT_OUT_FILE"},
      {"DRV_CFLAGS", "-g -Wall -fPIC -MMD $ERL_CFLAGS"},
      {"DRV_LDFLAGS", "-shared $ERL_LDFLAGS"},
      {"EXE_CFLAGS", "-g -Wall -fPIC -MMD $ERL_CFLAGS"},
      {"EXE_LDFLAGS", "$ERL_LDFLAGS"},

      {"ERL_CFLAGS", lists:concat(
         [
            " -I\"", erlInterfaceDir(include),
            "\" -I\"", filename:join(ertsDir(), "include"),
            "\" "
         ])},
      {"ERL_EI_LIBDIR", lists:concat(["\"", erlInterfaceDir(lib), "\""])},
      {"ERL_LDFLAGS", " -L$ERL_EI_LIBDIR -lerl_interface -lei"},
      {"ERLANG_ARCH", rebarUtils:wordsize()},
      {"ERLANG_TARGET", rebarUtils:getArch()},

      {"darwin", "DRV_LDFLAGS",
         "-bundle -flat_namespace -undefined suppress $ERL_LDFLAGS"},

      %% Solaris specific flags
      {"solaris.*-64$", "CFLAGS", "-D_REENTRANT -m64 $CFLAGS"},
      {"solaris.*-64$", "CXXFLAGS", "-D_REENTRANT -m64 $CXXFLAGS"},
      {"solaris.*-64$", "LDFLAGS", "-m64 $LDFLAGS"},

      %% OS X Leopard flags for 64-bit
      {"darwin9.*-64$", "CFLAGS", "-m64 $CFLAGS"},
      {"darwin9.*-64$", "CXXFLAGS", "-m64 $CXXFLAGS"},
      {"darwin9.*-64$", "LDFLAGS", "-arch x86_64 -flat_namespace -undefined suppress $LDFLAGS"},

      %% OS X Lion onwards flags for 64-bit
      {"darwin1[0-4].*-64$", "CFLAGS", "-m64 $CFLAGS"},
      {"darwin1[0-4].*-64$", "CXXFLAGS", "-m64 $CXXFLAGS"},
      {"darwin1[0-4].*-64$", "LDFLAGS", "-arch x86_64 -flat_namespace -undefined suppress $LDFLAGS"},

      %% OS X Snow Leopard, Lion, and Mountain Lion flags for 32-bit
      {"darwin1[0-2].*-32", "CFLAGS", "-m32 $CFLAGS"},
      {"darwin1[0-2].*-32", "CXXFLAGS", "-m32 $CXXFLAGS"},
      {"darwin1[0-2].*-32", "LDFLAGS", "-arch i386 -flat_namespace -undefined suppress $LDFLAGS"},

      %% Windows specific flags
      %% add MS Visual C++ support to rebar on Windows
      {"win32", "CC", "cl.exe"},
      {"win32", "CXX", "cl.exe"},
      {"win32", "LINKER", "link.exe"},
      {"win32", "DRV_CXX_TEMPLATE",
         %% DRV_* and EXE_* Templates are identical
         "$CXX /c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
      {"win32", "DRV_CC_TEMPLATE",
         "$CC /c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
      {"win32", "DRV_LINK_TEMPLATE",
         "$LINKER $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS /OUT:$PORT_OUT_FILE"},
      {"win32", "DRV_LINK_CXX_TEMPLATE",
         "$LINKER $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS /OUT:$PORT_OUT_FILE"},
      %% DRV_* and EXE_* Templates are identical
      {"win32", "EXE_CXX_TEMPLATE",
         "$CXX /c $CXXFLAGS $EXE_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
      {"win32", "EXE_CC_TEMPLATE",
         "$CC /c $CFLAGS $EXE_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
      {"win32", "EXE_LINK_TEMPLATE",
         "$LINKER $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS /OUT:$PORT_OUT_FILE"},
      {"win32", "EXE_LINK_CXX_TEMPLATE",
         "$LINKER $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS /OUT:$PORT_OUT_FILE"},
      %% ERL_CFLAGS are ok as -I even though strictly it should be /I
      {"win32", "ERL_LDFLAGS",
         " /LIBPATH:$ERL_EI_LIBDIR erl_interface.lib ei.lib"},
      {"win32", "DRV_CFLAGS", "/Zi /Wall $ERL_CFLAGS"},
      {"win32", "DRV_LDFLAGS", "/DLL $ERL_LDFLAGS"},
      %% Provide some default Windows defines for convenience
      {"win32", "CFLAGS", "/Wall /DWIN32 /D_WINDOWS /D_WIN32 /DWINDOWS /Ic_src $CFLAGS"},
      {"win32", "CXXFLAGS", "/Wall /DWIN32 /D_WINDOWS /D_WIN32 /DWINDOWS /Ic_src $CXXFLAGS"}
   ].

getTool(Arch, Tool, Default) ->
   getTool(Arch, false, Tool, Default).

getTool(false, _, _, Default) -> Default;
getTool("", _, _, Default) -> Default;
getTool(Arch, false, Tool, _Default) -> Arch ++ "-" ++ Tool;
getTool(Arch, "", Tool, _Default) -> Arch ++ "-" ++ Tool;
getTool(Arch, Vsn, Tool, _Default) -> Arch ++ "-" ++ Tool ++ "-" ++ Vsn.
