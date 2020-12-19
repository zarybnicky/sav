{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Lib where

{-@ LIQUID "--no-termination" @-}

import Prelude hiding (replicate, take, length)

{-@ type Pos = {v:Int | 0 < v} @-}

{-@ incr :: Pos -> Pos @-}
incr :: Int -> Int
incr 1 = 1
incr x = x - 1


data SList a = SL { size :: Int, elems :: [a] }
{-@ size :: q:SList a -> {v:Nat | v = size q} @-}
--{-@ invariant {v:SList a | size v >= 0} @-}

{-@ die :: {v:String | false} -> a @-}
die x = error x

replicate :: Int -> a -> Queue a

{-@ measure realSize @-}
realSize :: [a] -> Int
realSize []     = 0
realSize (_:xs) = 1 + realSize xs

{-@ data SList a = SL {
      size  :: Nat
    , elems :: {v:[a] | realSize v = size}
    }
@-}
{-@ type SListN a N = {v:SList a | size v = N} @-}
{-@ type NESList a  = {v:SList a | size v > 0} @-}

{-@ nil :: SListN a 0 @-}
nil = SL 0 []

{-@ cons :: a -> xs:SList a -> SListN a {size xs + 1} @-}
cons x (SL n xs) = SL (n+1) (x:xs)

{-@ tl           :: xs:NESList a -> SListN a {size xs - 1}  @-}
tl (SL n (_:xs)) = SL (n-1) xs
tl _             = error "empty SList"

{-@ hd           :: xs:NESList a -> a @-}
hd (SL _ (x:_))  = x
hd _             = error "empty SList"

{-@ okList :: SListN String 1 @-}
okList  = SL 1 ["cat"]    -- accepted
--badList = SL 1 []         -- rejected

okHd  = hd okList       -- accepted
--badHd = hd (tl okList)  -- rejected

{-@ data Queue a = Q {
      front :: SList a
    , back  :: SListLE a (size front)
    }
@-}
data Queue a = Q
  { front :: SList a
  , back  :: SList a
  }

-- | Queues of size `N`
{-@ type QueueN a N = {v:Queue a | queueSize v = N} @-}
{-@ type NEQueue a = {v:Queue a | queueSize v > 0} @-}
{-@ type SListLE a N = {v:SList a | size v <= N} @-}

{-@ measure queueSize @-}
queueSize :: Queue a -> Int
queueSize (Q f b) = size f + size b

okQ  = Q okList nil  -- accepted, |front| > |back|
--badQ = Q nil okList  -- rejected, |front| < |back|

{-@ emp :: QueueN _ 0 @-}
emp = Q nil nil

{-@ remove :: q:NEQueue a -> (a, QueueN a {queueSize q - 1})  @-}
remove (Q f b) = (hd f, makeq (tl f) b)


okRemove  = remove example2Q   -- accept
--badRemove = remove example0Q   -- reject

{-@ example2Q :: QueueN _ 2 @-}
example2Q = Q (1 `cons` (2 `cons` nil)) nil

{-@ example0Q :: QueueN _ 0 @-}
example0Q = Q nil nil

{-@ makeq :: f:SList a -> b:SListLE a {size f + 1} -> QueueN a {size f + size b} @-}
makeq f b
  | size b <= size f = Q f b
  | otherwise        = Q (rot f b nil) nil

{-@ rot :: l:SList a -> r:SListN a {size l + 1} -> a:SList a -> SListN a {size l + size r + size a} @-}
rot f b acc
  | size f == 0 = hd b `cons` acc
  | otherwise   = hd f `cons` rot (tl f) (tl b) (hd b `cons` acc)

{-@ insert :: a -> q: Queue a -> QueueN a {queueSize q + 1} @-}
insert e (Q f b) = makeq f (e `cons` b)

{-@ replicate :: n:Nat -> a -> QueueN a n @-}
replicate 0 _ = emp
replicate n x = insert x (replicate (n-1) x)

{-@ okReplicate :: QueueN _ 3 @-}
okReplicate = replicate 3 "Yeah!"  -- accept

{-@ badReplicate :: QueueN _ 3 @-}
badReplicate = replicate 1 "No!"   -- reject


{-@ exampleQ :: QueueN _ 3 @-}
exampleQ = insert "nal" $ insert "bob" $ insert "alice" $ emp





data Heap a = Empty | Node { left :: (Heap a), el :: a, right :: (Heap a) }
{-@ type HeapN a N = {h:Heap a | heapSize h = N} @-}
{-@ type NEHeap a = {h:Heap a | heapSize h > 0} @-}
{-@ invariant {v:Heap a | heapSize v >= 0} @-}
{-@ data Heap a = Empty | Node
    { left :: Heap a
    , el :: a
    , right :: {h:Heap a | heapSize h = heapSize left || heapSize h = heapSize left + 1}
    }
@-}

{-@ measure heapSize @-}
heapSize :: Heap a -> Int
heapSize = \case
  Empty -> 0
  Node l _ r -> 1 + heapSize l + heapSize r

{-@ empH :: {h:Heap a| heapSize h = 0} @-}
empH = Empty

{-@ singleH :: a -> {h:Heap a|heapSize h = 1} @-}
singleH x = Node Empty x Empty

{-@ addH :: x:a -> h:Heap a -> HeapN a {heapSize h + 1} @-}
addH x Empty = Node Empty x Empty
addH x (Node l y r)
  | x <= y = Node (addH y r) x l
  | otherwise = Node (addH x r) y l

{-@ merge :: h:Heap a -> {j:Heap a | heapSize j = heapSize h || heapSize j = heapSize h + 1} -> HeapN a {heapSize h + heapSize j} @-}
merge l r
  | heapSize l == heapSize r = case (l, r) of
      (Empty, _) -> Empty
      (_, Empty) -> Empty
      (Node ll lx lr, Node _ ly _) ->
        if | lx <= ly -> Node r lx (merge ll lr)
           | otherwise -> let (x, l') = extract l in Node (replaceMin x r) ly l'
  | otherwise = case (l, r) of
      (Node ll lx lr, Node rl ly rr) ->
        if | lx <= ly -> Node r lx (merge ll lr)
           | otherwise -> let (x, l') = extract l in Node (replaceMin x r) ly l'

{-@ pop :: h:Heap a -> (a, HeapN a {heapSize h - 1}) @-}
pop (Node l y r) = (y, merge l r)
pop Empty = error "Empty heap"


{-@ extract :: h:NEHeap a -> (a, HeapN a {heapSize h - 1}) @-}
extract (Node l y r)
  | heapSize l == heapSize r = case l of
      Empty -> (y, Empty)
      Node{} -> let (x, l') = extract l in (x, Node r y l')
  | otherwise = let (x, l') = extract l in (x, Node r y l')

{-@ replaceMin :: a -> h:NEHeap a -> HeapN a {heapSize h} @-}
replaceMin _ Empty = error "Empty heap"
replaceMin a (Node Empty _ r) = Node Empty a r
replaceMin a (Node l@(Node _ lx _) _ Empty)
  | lx <= a = Node l a Empty
  | otherwise = Node (replaceMin a l) lx Empty
replaceMin a (Node l@(Node _ lx _) _ r@(Node _ rx _))
  | lx <= a, rx <= a = Node l a r
  | rx <= lx = Node (replaceMin a l) lx r
  | otherwise = Node l rx (replaceMin a r)

{-@ exampleH :: HeapN Int 4 @-}
exampleH :: Heap Int
exampleH = addH 5 $ addH 4 $ addH 1 $ singleH 9
