----------------------------------------------------------------------
-- |
-- Module: RunLength
--
--
-- Run-length encoding.
--
----------------------------------------------------------------------

module RunLength
  ( decode
  , encode
  )
  where

import Data.List

-- |
--
-- Decode a run-length encoded list to a list.
--
-- >>> decode [(5,'a'),(4,'e'),(3,'i'),(2,'o'),(1,'u')]
-- "aaaaaeeeeiiioou"

decode
  :: [(Int, a)]
  -> [a]
decode =
  foldl tupAppend []

tupAppend :: [a] -> (Int, a) -> [a]
tupAppend as tup = as ++ tupExpand tup

tupExpand :: (Int, a) -> [a]
tupExpand (num, val) = take num $ repeat val


-- |
--
-- Encode a list to a run-length encoded list.
--
-- >>> encode "aaaaaeeeeiiioou"
-- [(5,'a'),(4,'e'),(3,'i'),(2,'o'),(1,'u')]

encode
  :: Eq a
  => [a]
  -> [(Int, a)]
encode as = foldl appendTup [] $ group as

appendTup :: Eq a => [(Int, a)] -> [a] -> [(Int, a)]
appendTup acc []   = acc
appendTup acc next = acc ++ [nextTup next]

nextTup :: Eq a => [a] -> (Int, a)
nextTup as = (length (takeWhile (== val) as), val)
               where val = head as
