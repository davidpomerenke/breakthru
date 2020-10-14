module AlphaBeta (super) where

import Ai
import Data.List.Safe (foldl1)
import Data.Maybe
import Flow ((|>))
import Game
import System.Random
import Prelude hiding (foldl1, max, min)
import qualified Prelude
import Data.Tree


reducetree :: (t1 -> t2 -> t3) -> (t3 -> t2 -> t2) -> t2 -> Tree t1 -> t3
reducetree f g x (Node label subtrees) = f label (reducetree' f g x subtrees)

reducetree' :: (t1 -> p -> t) -> (t -> p -> p) -> p -> [Tree t1] -> p
reducetree' f g x (subtree : rest) = g (reducetree f g x subtree) (reducetree' f g x rest)
reducetree' _ _ x [] = x

maptree :: (t -> a) -> Tree t -> Tree a
maptree f = reducetree (\label subtrees -> Node (f label) subtrees) (:) []

reptree :: (a -> [a]) -> a -> Tree a
reptree f a = Node a (map (reptree f) (f a))

moves_ :: State -> [State]
moves_ state =
  (actions breakthru) state
    |> map ((result breakthru) state)
    |> catMaybes

gametree :: State -> Tree State
gametree p =
  reptree moves_ p

static :: State -> Utility
static state =
  utility breakthru state
    |> fmap (\f -> f Gold)
    |> fromMaybe (heuristic state Gold)

max = foldl Prelude.max (Utility 0)

min = foldl Prelude.min (Utility 1)

maximise :: Tree Utility -> Utility
maximise (Node n []) = n
maximise (Node _ sub) = max (map minimise sub)

minimise :: Tree Utility -> Utility
minimise (Node n []) = n
minimise (Node _ sub) = min (map maximise sub)

prune :: Int -> Tree a -> Tree a
prune 0 (Node a _) = Node a []
prune n (Node a x) = Node a (map (prune (n -1)) x)

evaluate :: State -> Utility
evaluate = maximise . maptree static . prune 2 . gametree

maximise' :: Tree Utility -> [Utility]
maximise' (Node n []) = [n]
maximise' (Node _ sub) = max' (map minimise' sub)

minimise' :: Tree Utility -> [Utility]
minimise' (Node n []) = [n]
minimise' (Node _ sub) = min' (map maximise' sub)

max' :: [[Utility]] -> [Utility]
max' (first : rest) = min first : betterthan (min first) rest

min' :: [[Utility]] -> [Utility]
min' (first : rest) = max first : betterthan (min first) rest

betterthan :: Utility -> [[Utility]] -> [Utility]
betterthan _ [] = []
betterthan a (next : rest) =
  if minleq next a
    then betterthan a rest
    else min next : betterthan (min next) rest

worsethan :: Utility -> [[Utility]] -> [Utility]
worsethan _ [] = []
worsethan a (next : rest) =
  if maxgeq next a
    then worsethan a rest
    else max next : worsethan (max next) rest

minleq :: [Utility] -> Utility -> Bool
minleq [] _ = False
minleq (num : rest) pot =
  if num <= pot
    then True
    else minleq rest pot

maxgeq :: [Utility] -> Utility -> Bool
maxgeq [] _ = False
maxgeq (num : rest) pot =
  if num >= pot
    then True
    else maxgeq rest pot

evaluate' :: State -> Utility
evaluate' = max . maximise' . maptree static . prune 2 . gametree

super :: Int -> StdGen -> Ai
super n _ state =
  let evaluate' = max . maximise' . maptree static . prune n . gametree
   in fmap fst $
        argmax snd $
          catMaybes $
            map (\action -> fmap (\state -> (action, evaluate' state)) ((result breakthru) state action)) $
              (actions breakthru) state

argmax :: (Ord b) => (a -> b) -> [a] -> Maybe a
argmax f = foldl1 (\a b -> if f a >= f b then a else b)