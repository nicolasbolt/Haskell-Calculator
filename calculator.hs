import Data.Char
import Data.Fixed
import Data.List
import Data.List.Split

expressionTypes :: [[Char]]
expressionTypes = ["+", "-", "*", "/", "f", "c", "r", "^", "%", "h"]

splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

getOperands :: [Char] -> [[Char]]
getOperands expr = removeWhitespace (splitOnAnyOf expressionTypes expr)

getOperator :: [Char] -> [Char]
getOperator [] = []
getOperator (x : xs) = do
  if [x] `elem` expressionTypes
    then [x]
    else getOperator xs

expressionType :: p -> [[Char]] -> [[Char]]
expressionType expr = do
  filter (`elem` expressionTypes)

parseDouble :: String -> Double
parseDouble x = read x :: Double

removeWhitespace :: [[Char]] -> [[Char]]
removeWhitespace [] = []
removeWhitespace (x : xs) = filter (/= ' ') x : xs

showHelp :: [Char]
showHelp = do
  "Here are the valid operators: \n\
  \\t+ -> addition \n\
  \\t- -> subtraction \n\
  \\t/ -> division (no rounding) \n\
  \\t* -> multiplication \n\
  \\tc -> ceiling division (rounds up) \n\
  \\tf -> floor division (rounds down) \n\
  \\t^ -> exponentiation \n\
  \\t% -> modulus \n\
  \NOTE: chained expressions are not currently supported.\n"

calculate :: [Char] -> [[Char]] -> String
calculate "+" operands = show (parseDouble (head operands) + parseDouble (last operands))
calculate "-" operands = show (parseDouble (head operands) - parseDouble (last operands))
calculate "*" operands = show (parseDouble (head operands) * parseDouble (last operands))
calculate "/" operands = show (parseDouble (head operands) / parseDouble (last operands))
calculate "c" operands = show (ceiling (parseDouble (head operands) / parseDouble (last operands)))
calculate "f" operands = show (floor (parseDouble (head operands) / parseDouble (last operands)))
calculate "^" operands = show (parseDouble (head operands) ** parseDouble (last operands))
calculate "%" operands = show (parseDouble (head operands) `mod'` parseDouble (last operands))
calculate _ operands = "Invalid operator"

printCalc :: IO ()
printCalc = do
  userExpression <- getLine
  let operator = getOperator userExpression
  let operands = getOperands userExpression
  if userExpression == "h"
    then putStr showHelp
    else print (calculate operator operands)

main :: IO ()
main = do
  putStrLn "Welcome!"
  putStrLn "Enter h for help."
  putStrLn "Enter an expression..."
  printCalc
  putStrLn "Evaluate another expression? (y/n)"
  input <- getLine
  if input == "y" || input == "Y"
    then main
    else putStrLn "Quitting..."