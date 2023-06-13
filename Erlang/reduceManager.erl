%07/02/2020
-module(reduceManager).
-export([reduceManager/2, createReducer/1, reducer/3]).

reduceManager(Function, Map) ->
  receive
    {reduce, Key, Value} -> 
      Tuple = maps:find(Key, Map),
      if
        Tuple == error -> 
          NewReducer = spawn(?MODULE, reducer, [Function, Value, self()]),
          reduceManager(Function, Map#{Key => NewReducer});
        true -> 
          {_, Reducer} = Tuple,
          Reducer ! {reduce, Value},
          reduceManager(Function, Map)
      end;
    
    printResults ->
      maps:foreach(
        fun(Key, Reducer) -> 
          Reducer ! print,
          receive
            {fromReducer, Acc} -> io:format("~p: ~p~n", [Key, Acc])
          end
      end, Map),
      reduceManager(Function, Map)
  end.


reducer(Function, Acc, Manager) ->
  receive
    {reduce, NewValue} -> reducer(Function, Function(NewValue, Acc), Manager);
      
    print -> 
      Manager ! {fromReducer, Acc},
      reducer(Function, Acc, Manager)
  end.

createReducer(Function) ->
  spawn(?MODULE, reduceManager, [Function, #{}]).