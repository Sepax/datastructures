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
  -- Read contens from textfile
  contents <- getContents

  -- Builds AATree from contents using foldl
  let tree = foldl (flip insert) emptyTree (words contents)
  -- Store statistics in variables
  let s  = fromIntegral ( size tree )
  let h  = fromIntegral ( height tree )
  let h' = fromIntegral ( ceiling ( logBase 2 (s+1) ) - 1 )
  let r  = h / h' :: Double

  -- Prints given statistics 
  putStrLn $ "size: " ++ show s
  putStrLn $ "height: " ++ show h
  putStrLn $ "optimal height': " ++ show h'
  putStrLn $ "ratio: " ++ show r
  putStrLn $ "Invariant: " ++ show (checkTree tree)
  putStrLn $ "First 20 words: " ++ unwords (take 20 $ inorder tree)

--------------------------------------------------------------------------------

