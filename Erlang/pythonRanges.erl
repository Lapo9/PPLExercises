%15/01/2020
-module(pythonRanges).
-export([range/3, rangeImpl/4, next/1]).


range(Start, End, Step) ->
  spawn(?MODULE, rangeImpl, [Start, End, Step, Start]).


rangeImpl(Start, End, Step, Curr) ->
  receive
    {next, Asker} -> 
      case End > Start of
        true -> 
          if 
            Curr > End -> Asker ! stopIteration;
            true ->
              Asker ! Curr,
              rangeImpl(Start, End, Step, Curr + Step)
          end;
        false ->
          if 
            End > Curr -> Asker ! stopIteration;     
            true -> 
              Asker ! Curr,
              rangeImpl(Start, End, Step, Curr + Step)
          end
      end
  end.


next(Range) ->
  Range ! {next, self()},
  receive
    Num -> Num
  end.