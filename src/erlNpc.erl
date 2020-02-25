-module(erlNpc).

-export([
   main/1
]).

main(Args) ->
   file:set_cwd("c_src"),
   {ok, Dir} = file:get_cwd(),
   io:format("erlNpc begin compile pwd:~15.p ~n", [Dir]),
   FunCom =
      fun(File) ->
         case filelib:is_dir(File) == true andalso lists:nth(1, File) =/= 46 andalso filename:basename(File) =/= "include" of
            true ->
               {ok, CurDir} = file:get_cwd(),
               io:format("erlNpc cur ~p: ~-18.s,  cur pwd:~p ~n", [Args, File, CurDir]),
               file:set_cwd(File),
               rebar:main(Args),
               file:set_cwd("..");
            _ ->
               ignore
         end
      end,
   case file:list_dir(".") of
      {ok, Files} ->
         lists:foreach(FunCom, Files);
      _Err ->
         rebar:log(error, "erlNpc start compile error ~p ~n", [_Err])

   end.

