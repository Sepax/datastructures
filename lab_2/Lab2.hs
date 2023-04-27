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

trade' :: Skew BuyBid -> Skew SellBid -> IO ()
trade' buyHeap sellHeap = do
  case (getMin buyHeap, getMin sellHeap) of
    (Just (BuyBid _ buyPrice), Just (SellBid _ sellPrice)) -> do
      if buyPrice >= sellPrice
        then do
          putStrLn $ "Trade: " ++ show buyPrice
          trade' (delMin buyHeap) (delMin sellHeap)
        else return ()
    _ -> return ()

partitionBids :: [Bid] -> ([BuyBid], [SellBid])
partitionBids = foldr partitionBids' ([], [])
  where
    partitionBids' bid (buyBids, sellBids) = case bid of
      Buy n p -> ((BuyBid n p)  : buyBids, sellBids)
      Sell n p -> (buyBids, (SellBid n p) : sellBids)
      NewBuy n _ p -> ((BuyBid n p) : buyBids, sellBids)
      NewSell n _ p -> (buyBids, (SellBid n p) : sellBids)


-- SellBid data type
data SellBid = SellBid Person Price

instance Eq SellBid where
  b == b' = name b == name b'
    where
      name (SellBid person price) = person

instance Ord SellBid where
  b <= b' = price b <= price b'
    where
      price (SellBid person p) = p

-- BuyBid data type
data BuyBid = BuyBid Person Price

instance Eq BuyBid where
  b == b' = name b == name b'
    where
      name (BuyBid person _) = person

instance Ord BuyBid where
  b <= b' = price b >= price b'
    where
      price (BuyBid person p) = p