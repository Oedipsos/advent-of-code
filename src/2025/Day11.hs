{-# LANGUAGE OverloadedStrings #-}

{- cabal:
build-depends:
  base,
  text,
  containers,
  megaparsec,
  mtl
-}

import Control.Monad.State
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec (parse, errorBundlePretty, Parsec, sepBy1, sepEndBy1, some)
import Text.Megaparsec.Char (eol, string, letterChar, hspace)

type Parser = Parsec Void T.Text
type Node   = String
type Route  = [Node]
type Graph  = M.Map Node [Node]
type Cache  = M.Map Node Int

pName :: Parser Node
pName = some letterChar

pConnections :: Parser [Node]
pConnections = pName `sepBy1` hspace

pLine :: Parser (Node, [Node])
pLine = (,) <$> pName <* string ": " <*> pConnections

parser :: Parser [(Node, [Node])]
parser = pLine `sepEndBy1` eol


nextNodes :: Node -> Graph -> [Node]
nextNodes key = fromMaybe [] . M.lookup key

buildRoute :: Graph -> Node -> Route -> [Route]
buildRoute _ _ [] = [] 
buildRoute graph dest rt@(r:_) 
    | r == dest = [rt]
    | otherwise = concat [buildRoute graph dest (r':rt) | r' <- nextNodes r graph, r' `notElem` rt]

-- Version without any memoisation
countPaths' :: Graph -> Node -> Node -> Int
countPaths' graph from to = length $ buildRoute graph to [from]

-- Implemented cache using Map
countPaths :: Graph -> Node -> Node -> Int
countPaths graph from to = flip evalState M.empty $ seekNext from
    where
        seekNext :: Node -> State Cache Int
        seekNext node =
            if node == to
                then return 1
            else do
                cache <- gets (M.lookup node)
                case cache of
                    Just result ->
                        return result
                    Nothing -> do
                        result <- sum <$> traverse seekNext (nextNodes node graph)
                        modify $ M.insert node result
                        return result

solve1 :: Graph -> Int
solve1 graph = countPaths graph "you" "out"

solve2 :: Graph -> Int
solve2 graph = countSubRoute "dac" "fft" + countSubRoute "fft" "dac"
    where
        countSubRoute nodeA nodeB = product
            [ countPaths graph "svr" nodeA
            , countPaths graph nodeA nodeB
            , countPaths graph nodeB "out"
            ]

main = do
    content <- T.pack <$> readFile "inputs/Day11.txt"
    case parse parser "" content of
        Left err -> putStrLn $ errorBundlePretty err
        Right input -> do
            let graph = M.fromList input
            let result1 = solve1 graph
            print result1
            let result2 = solve2 graph
            print result2
