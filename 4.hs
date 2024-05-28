import Data.Char (isLower, isUpper)
import Data.List (nub, intersect)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Rule = (String, [String])
type Grammar = [Rule]
type First = Map.Map String [String]
type Follow = Map.Map String [String]
type ParseTable = Map.Map (String, String) [String]

main :: IO ()
main = do
    putStrLn "Input number of rules:"
    n <- readLn
    grammar <- mapM (\_ -> getRule) [1..n]
    
    putStrLn "Input First sets:"
    first <- getFirstFollowSets
    
    putStrLn "Input Follow sets:"
    follow <- getFirstFollowSets
    
    let parseTable = buildParseTable grammar first follow
    
    putStrLn "Input string to parse:"
    inputString <- getLine
    let result = parseInput grammar parseTable (words inputString)
    putStrLn $ "Parsing result: " ++ show result

getRule :: IO Rule
getRule = do
    putStrLn "Input rule:"
    rule <- getLine
    let (lhs:rhs) = words rule
    return (lhs, rhs)

getFirstFollowSets :: IO (Map.Map String [String])
getFirstFollowSets = do
    putStrLn "Enter number of sets:"
    n <- readLn
    sets <- mapM (\_ -> do
                    putStrLn "Enter non-terminal and its set:"
                    line <- getLine
                    let (nt:set) = words line
                    return (nt, set)
                ) [1..n]
    return $ Map.fromList sets

buildParseTable :: Grammar -> First -> Follow -> ParseTable
buildParseTable grammar first follow = foldl addRule Map.empty grammar
  where
    addRule table (lhs, rhs) =
      let firstSet = firstOfRHS rhs
          followSet = fromMaybe [] (Map.lookup lhs follow)
          entries = if "ε" `elem` firstSet
                    then [(lhs, t, rhs) | t <- firstSet, t /= "ε"] ++ [(lhs, t, rhs) | t <- followSet]
                    else [(lhs, t, rhs) | t <- firstSet]
      in foldl (\tbl (l, t, r) -> Map.insert (l, t) r tbl) table entries
    firstOfRHS [] = ["ε"]
    firstOfRHS (x:xs)
      | isNonTerminal x =
          let firstSet = fromMaybe [] (Map.lookup x first)
          in if "ε" `elem` firstSet
             then nub (firstSet ++ firstOfRHS xs)
             else firstSet
      | otherwise = [x]

parseInput :: Grammar -> ParseTable -> [String] -> Bool
parseInput grammar parseTable input = parse ["S"] input
  where
    parse [] [] = True
    parse (x:xs) input@(i:is)
      | isNonTerminal x = case Map.lookup (x, i) parseTable of
                            Just rule -> parse (rule ++ xs) input
                            Nothing   -> False
      | x == "ε"        = parse xs input
      | x == i          = parse xs is
      | otherwise       = False
    parse _ _ = False
    
    isNonTerminal x = not (null x) && isUpper (head x)

isNonTerminal :: String -> Bool
isNonTerminal x = not (null x) && isUpper (head x)