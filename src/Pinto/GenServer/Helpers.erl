-module(pinto_genServer_helpers@foreign).

-export([exit_/2]).

exit_(Name0, Reason) ->
  Name = translate_name(Name0),
  fun() -> gen_server:stop(Name, Reason, timer:seconds(5)) end.

translate_name({local, Name}) ->
  Name;
translate_name(Name) ->
  Name.
