-module(chatterbox_utilities@foreign).

-export([create_unsafe_start_result/1, translate_start_link_error/1]).

create_unsafe_start_result({right, Pid}) ->
  {ok, Pid};
create_unsafe_start_result({left, Reason}) ->
  {error, Reason}.

translate_start_link_error({ignore}) ->
  ignore;
translate_start_link_error({alreadyStarted, Pid}) ->
  {already_started, Pid};
translate_start_link_error({failed, Reason}) ->
  {failed, Reason}.

