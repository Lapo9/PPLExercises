%20/07/2017
-module(dlist).
-export([dlist_create/1, dlist_store/1, dlist_to_list/1, dlist_map/2, dlist_get/1]).

dlist_create(0) -> [];
dlist_create(N) ->
  [spawn(?MODULE, dlist_store, [0])] ++ dlist_create(N-1).

dlist_store(X) ->
  receive
    {get, Asker} -> Asker ! X, dlist_store(X);
    {set, New} -> dlist_store(New)
  end.

dlist_to_list(Dlist) ->
  [dlist_get(E) || E <- Dlist].

dlist_get(Pid) ->
  Pid ! {get, self()},
  receive
    X -> X
  end.

dlist_map(Func, Dlist) ->
  [E ! {set, Func(dlist_get(E))} || E <- Dlist],
  done.