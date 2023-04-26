import Control.Applicative
import System.Environment
import System.IO
import Prelude
import Data.List
import SkewHeap

-- | Bids.

data Bid
  = Buy Person Price           -- Person offers to buy share
  | Sell Person Price          -- Person offers to sell share
  | NewBuy Person Price Price  -- Person changes buy bid
  | NewSell Person Price Price -- Person changes sell bid
  deriving Show

type Person = String
type Price = Integer

-- | Parses a bid. Incorrectly formatted bids are returned verbatim
-- (tagged with 'Left').

parseBid :: String -> Either String Bid
parseBid s = case words s of
  name : kind : prices ->
    case (kind, mapM readInteger prices) of
      ("K",  Just [price])              -> Right (Buy name price)
      ("S",  Just [price])              -> Right (Sell name price)
      ("NK", Just [oldPrice, newPrice]) -> Right (NewBuy name oldPrice newPrice)
      ("NS", Just [oldPrice, newPrice]) -> Right (NewSell name oldPrice newPrice)
      _ -> Left s
  _ -> Left s
  where
  readInteger :: String -> Maybe Integer
  readInteger s = case filter (null . snd) $ reads s of
    [(x, _)] -> Just x
    _        -> Nothing

-- | Parses a sequence of bids. Correctly formatted bids are returned
-- (in the order encountered), and an error message is printed for
-- each incorrectly formatted bid.

parseBids :: String -> IO [Bid]
parseBids s = concat <$> mapM (check . parseBid) (lines s)
  where
  check (Left bid)  = do
    hPutStrLn stderr $ "Malformed bid: " ++ bid
    return []
  check (Right bid) = return [bid]

-- | The main function of the program.

main :: IO ()
main = do
  args <- getArgs
  case args of
    []  -> process stdin
    [f] -> process =<< openFile f ReadMode
    _   -> hPutStr stderr $ unlines
      [ "Usage: ./Lab2 [<file>]"
      , "If no file is given, then input is read from standard input."
      ]
  where
  process h = trade =<< parseBids =<< hGetContents h

-- | The core of the program. Takes a list of bids and executes them.

trade :: [Bid] -> IO ()
trade bids = do
  let (buyBids, sellBids) = partitionBids bids
  let (buyHeap, sellHeap) = (toSkew buyBids, toSkew sellBids)
  trade' buyHeap sellHeap
  return ()

trade' :: Skew Bid -> Skew Bid -> IO ()
trade' buyHeap sellHeap = do
  case (getMin buyHeap, getMin sellHeap) of
    (Just (Buy _ buyPrice), Just (Sell _ sellPrice)) -> do
      if buyPrice >= sellPrice
        then do
          putStrLn $ "Trade: " ++ show buyPrice
          trade' (delMin buyHeap) (delMin sellHeap)
        else return ()
    _ -> return ()

partitionBids :: [Bid] -> ([Bid], [Bid])
partitionBids = foldr partitionBids' ([], [])
  where
  partitionBids' bid (buyBids, sellBids) = case bid of
    Buy _ _ -> (bid : buyBids, sellBids)
    Sell _ _ -> (buyBids, bid : sellBids)
    NewBuy _ _ _ -> (bid : buyBids, sellBids)
    NewSell _ _ _ -> (buyBids, bid : sellBids)



