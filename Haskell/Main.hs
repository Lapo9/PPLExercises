import BinaryTree
import Bmi
import CashRegister
import Control.Monad.State.Lazy
import KnightQuest
import PriceList
import ReversePolishCalculator
import RightTriangles
import SumOfMultiples
import SumOfRoots
import Tril
import Nfsa
import UnderstandingConcatMap
import Tritree
import BfList
import Dupl
import DeepList
import Blob
import StateMonad

main =
  let program = "StateMonadDo"
   in case program of
        "RightTriangles" -> print (triangles 10 24)
        "Bmi" -> print (bmi [(75, 1.80), (80, 1.85), (52, 1.87)])
        "Bmi2" -> print (bmi2 [(75, 1.80), (80, 1.85), (52, 1.87)])
        "SumOfMultiples" -> print (sumOfMultiples 3 4 20)
        "SumOfRoots" -> print (sumOfRoots 1000)
        "BinaryTree" ->
          let nums = [5, 3, 4, 1, 9]
              tree = foldl insertTree EmptyTree nums
           in print (searchTree tree 4, tree, fmap (* 2) tree)
        "ReversePolishCalculator" -> print (reversePolishCalculator "5 2 3 + 9 - *") -- 5 * (9 - (2 + 3))
        "KnightQuest" -> print (canReachIn 4 (4, 1) [(2, 3)])
        "PriceList" ->
          let funcs = PriceList [(("nice " ++), 0.5), (("good " ++), 0.4)]
              items = PriceList [("pen", 4.5), ("pencil", 2.8), ("rubber", 0.8)]
           in print (funcs <*> items)
        "CashRegister" ->
          let cr1 = CashRegister ("apple", 2.5)
           in print (cr1 >>= (\s -> CashRegister (s ++ " banana", 3)))
        "Tril" -> print (list2tril [1] 1 3)
        "Testing" ->
          let ex = runState (do x <- return 5; return (x + 1)) 333
           in print ex
        "Nfsa" -> print (accepts "aaaabdbdbffghghfghffg" start)
        "UnderstandingConcatMap" -> print test
        "Tritree" -> 
          let funcs = TtNode (+10) (TtNode (*10) TtEmpty TtEmpty TtEmpty) (TtNode (+3) TtEmpty TtEmpty TtEmpty)(TtNode (*100) TtEmpty TtEmpty TtEmpty)
              items = TtNode (3) TtEmpty TtEmpty TtEmpty
           in print (show (funcs <*> items))
        "BfList" ->
          let funcsBfl = BfList True [(+1), (*10), (*100)]
              itemsBfl1 = BfList False [3, 5, 6, 11]
              itemsBfl2 = BfList True [2, 15]
            in print (show (funcsBfl <*> (itemsBfl1 <++> itemsBfl2)))
        "Dupl" ->
          let funcsDupl = Dupl [(+1), (+10)] [(*1), (*10), (*100)]
              itemsDupl = Dupl [1, 2, 3, 4] [20, 31]
            in print (show (funcsDupl <*> itemsDupl))
        "DeepList" -> print (show (fmap (*100) (fep [1, 2, 3, 4])))
        "Blob" -> print (show (Blob 3 (*100)))
        "StateMonad" -> let fs1 = (\(s:ss) -> (s, ss))
                            fs2 = (\(s1:s2:ss) -> (s1+s2, ss))
                            chooseState = (\o -> if o < 3 then StateProc fs1 else StateProc fs2)
                            initState = [1..20]
                         in print (runStateProc ((StateProc fs1) >>= chooseState >>= chooseState >>= chooseState >>= chooseState) initState)
        "StateMonadDo" -> let fs1 = (\(s:ss) -> (s, ss))
                              fs2 = (\(s1:s2:ss) -> (s1+s2, ss))
                              chooseState = (\o -> if o < 3 then StateProc fs1 else StateProc fs2)
                              initState = [1..20] 
                          in print $ runStateProc (do
                              o1 <- StateProc fs1
                              o2 <- chooseState o1
                              o3 <- chooseState o2
                              o4 <- chooseState o3
                              o5 <- chooseState o4
                              chooseState o5) initState
        otherwise -> error ("Program '" ++ program ++ "' doesn't exist")