%03/09/2020
-module(split).
-export([split/2, splitImpl/5, splitMap/3, mapList/4]).


splitImpl([Car | Cdr], N, Curr, Pre, Post) ->
  if 
    Cdr == [] ->
      if
        Curr > N -> {Pre, Post ++ [Car]};
        true -> {Pre ++ [Car], Post}
      end;
    true ->
      if
        Curr > N -> splitImpl(Cdr, N, Curr + 1, Pre, Post ++ [Car]);
        true -> splitImpl(Cdr, N, Curr + 1, Pre ++ [Car], Post)
      end
  end.


split(List, N) -> splitImpl(List, N, 0, [], []).


splitMap(Func, List, N) ->
  {Pre, Post} = split(List, N),
  spawn(?MODULE, mapList, [Func, Pre, pre, self()]),
  spawn(?MODULE, mapList, [Func, Post, post, self()]),
  receive
    {pre, MappedPre} -> received
  end,
  receive
    {post, MappedPost} -> received
  end,
  {MappedPre, MappedPost}.


mapList(Func, List, PrePost, Sender) ->
  Out = lists:map(Func, List),
  Sender ! {PrePost, Out}.