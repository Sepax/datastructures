{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree
import System.IO

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents

  -- split the data into words and build an AA tree
  -- use foldl
  -- Split the text into words

  -- Insert the words into the AA tree
  let tree = foldl (\acc x -> insert x acc) emptyTree (words contents)
  
  -- calculate and print statistics
  let s  = fromIntegral ( size tree )
  let h  = fromIntegral ( height tree )
  let h' = fromIntegral ( ceiling ( logBase 2 (s+1) ) - 1 )
  let r  = h / h' :: Double

  -- Test with checkTree and print result
  let invariant = undefined

  -- use fromIntegral/ceiling/logBase
  undefined



--------------------------------------------------------------------------------

