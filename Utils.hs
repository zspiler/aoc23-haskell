module Utils (readInt, indices, firstAndLast) where

readInt s = read s :: Int

indices s = take (length s) [0..]


firstAndLast :: [a] -> [a]
firstAndLast [] = []
firstAndLast [x] = [x, x]
firstAndLast x = [head x, last x]