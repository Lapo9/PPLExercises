-module(main).
-export([start/0]).

start() ->
  Program = "Dlist",
  case Program of
    "LetItCrash" -> letItCrash:letItCrash();
    
    "ReduceManager" -> 
      Reducer = reduceManager:createReducer(fun(X, Acc) -> X + Acc end),
      Reducer ! {reduce, "Ciao", 1},
      Reducer ! {reduce, "Ciao", 5},
      Reducer ! {reduce, "Hello", 2},
      Reducer ! printResults,
      Reducer ! {reduce, "Ciao", 2},
      Reducer ! printResults;

    "PythonRanges" ->
      Range = pythonRanges:range(11,2,-2),
      io:format("~p~n", [pythonRanges:next(Range)]),
      io:format("~p~n", [pythonRanges:next(Range)]),
      io:format("~p~n", [pythonRanges:next(Range)]),
      io:format("~p~n", [pythonRanges:next(Range)]),
      io:format("~p~n", [pythonRanges:next(Range)]),
      io:format("~p~n", [pythonRanges:next(Range)]);

    "Split" -> io:format("~p~n", [split:splitMap(fun(X) -> X*100 end, [7, 2, 2, 9, 11], 2)]);

    "Dlist" -> 
      Dlist = dlist:dlist_create(5),
      dlist:dlist_map(fun(X) -> X+100 end, Dlist),
      io:format("~w~n", [dlist:dlist_to_list(Dlist)]);
    
    _ -> "Program " ++ Program ++ " doesn't exist"
  end.
