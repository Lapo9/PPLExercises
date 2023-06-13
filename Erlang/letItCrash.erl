-module(letItCrash).
-export([letItCrash/0, createWorkers/1, serverMain/1, workerMain/2]).

createWorkers(0) -> ok;
createWorkers(N) ->
  Worker = spawn_link(?MODULE, workerMain, [30, 0]),
  io:format("Worker ~p created~n", [Worker]),
  createWorkers(N-1).


serverMain(ActiveReplicas) ->
  if 
    ActiveReplicas > 0 ->
      receive
        {'EXIT', Worker, normal} ->
          io:format("Worker ~p exited~n", [Worker]),
          serverMain(ActiveReplicas - 1);
        {'EXIT', Worker, _} ->
          io:format("Worker ~p crashed~n", [Worker]),
          createWorkers(1),
          serverMain(ActiveReplicas);
        {workerReady, Worker} ->
          Worker ! {add, rand:uniform(10)},
          serverMain(ActiveReplicas)
      end;
    true -> stop
  end.
     

workerMain(Max, Count) ->
  receive
    {add, Increase} -> 
      NewCount = Count + Increase,
      Crashed = rand:uniform(10) < 2,
      io:format("Worker ~p increased by ~p: ~p~n", [self(), Increase, NewCount]),
      if 
        Crashed -> error("Crash");
        Max > NewCount -> workerMain(Max, NewCount);
        true -> stop
      end
  after
    5 -> 
      server ! {workerReady, self()},
      workerMain(Max, Count)
  end.


letItCrash() -> 
  register(server, self()),
  Replicas = 4,
  io:format("Server started~n"),
  createWorkers(Replicas),
  serverMain(Replicas),
  io:format("Server stopped~n"),
  unregister(server).