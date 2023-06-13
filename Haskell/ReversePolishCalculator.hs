module ReversePolishCalculator where

operators :: [(Char, Int -> Int -> Int)]
operators = [('+', (+)), ('-', (-)), ('*', (*)), ('/', div)]

numbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

reversePolishCalculator :: String -> Int
reversePolishCalculator expr = reversePolishCalculatorInternals (words expr) []

-- this function receives the expression (tokenized) and the stack. It returns the result in a recursive way
reversePolishCalculatorInternals :: [String] -> [Int] -> Int
-- If we have no more terms, the stack should contain just one element, the final result
reversePolishCalculatorInternals [] (s0 : []) = s0
-- General case
reversePolishCalculatorInternals (t : terms) stack@(s0 : s1 : sx)
  | isNumber t = reversePolishCalculatorInternals terms ((read t :: Int) : stack) -- if the term is a number, add to the stack and go to next term
  | isOperator t = reversePolishCalculatorInternals terms (calc s0 s1 (head t) : sx) -- if the term is an operator, pop the top 2 elements of the stack, apply the operator and go to next term
  | otherwise = error ("Term '" ++ t ++ "' not valid!")
-- Case where the stack has < 2 elements (we can only push numbers, not do operations)
reversePolishCalculatorInternals (t : terms) stack
  | isNumber t = reversePolishCalculatorInternals terms ((read t :: Int) : stack)
  | otherwise = error ("Term '" ++ t ++ "' not valid (too few elements on the stack to perform an operation)")
-- Any other case is for sure an error
reversePolishCalculatorInternals _ _ = error "Expression not valid!"

-- given 2 numbers and a string representing an operator, apply the operator
calc :: Int -> Int -> Char -> Int
calc x y op = snd (head (filter (\pair -> fst pair == op) operators)) x y

-- is x one of the base 4 operators?
isOperator :: String -> Bool
isOperator (x : [])
  | x `elem` map fst operators = True
  | otherwise = False
isOperator _ = False

-- is x a number?
isNumber :: String -> Bool
isNumber [] = False
isNumber (x : [])
  | x `elem` numbers = True
  | otherwise = False
isNumber (x : xs)
  | x `elem` numbers = isNumber xs
  | otherwise = False
