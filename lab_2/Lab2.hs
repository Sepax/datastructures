import Control.Applicative
import Data.Function (on)
import System.Environment
import System.IO
import SkewHeap
import Control.Monad

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
trade = undefined

data BuyBid = Buyer {buyer :: Person , buyTheBid ::  Price} deriving Show

instance Ord BuyBid where
    (<=) = (>=) `on` buyTheBid

instance Eq BuyBid where
    (==) = (==) `on` buyTheBid


data SellBid = Seller {seller :: Person , sellTheBid :: Price} deriving Show

instance Ord SellBid where
    (<=) = (<=) `on` sellTheBid

instance Eq SellBid where
    (==) = (==) `on` sellTheBid

data OrderBook = OrderBook (Skew BuyBid, Skew SellBid) deriving Show

--tradeHelper :: OrderBook -> [Bid] -> IO ()
--tradeHelper orderbook bids = do


main1 :: IO ()
main1 = do 
  file <- readFile "bidders.txt"
  let list = lines file
  let bidList = fileToBid list 
  case bidList of 
    Left x ->  print ("WRONG INPUT NOT BUYER OR SELLER (parse failed) >" ++ x)
    Right x -> x
  let ob = orderBids' bidList
  print ob

-- takes a List of String and returns a Either String or a List of Bids
fileToBid :: [String] -> Either String [Bid]
fileToBid strList = traverse parseBid strList

buySingle :: BuyBid -> Skew BuyBid
buySingle buybid = single buybid 

sellSingle :: SellBid -> Skew SellBid
sellSingle sellbid = single sellbid

-- Helper function for orderbids' witch has an extra argument of OrderBook
orderBids :: [Bid] -> OrderBook -> OrderBook 
orderBids [] (OrderBook (buy,sell)) =(OrderBook (buy,sell))
orderBids (bid:bids) (OrderBook (buy,sell)) = case bid of 
  (Buy name price)                 -> orderBids bids (OrderBook ((insertSkew (Buyer name price) (buy)),(sell)))
  (Sell name price)                -> orderBids bids (OrderBook ((buy),(insertSkew (Seller name price) (sell))))
  (NewBuy name oldPrice newPrice)  -> orderBids bids (OrderBook ((insertSkew (Buyer name newPrice) (buy)),(sell)))
  (NewSell name oldPrice newPrice) -> orderBids bids (OrderBook ((buy),(insertSkew (Seller name newPrice) (sell))))

-- inputs a list of bids and returns a Orderbook with a two SkewHeaps (one for buyers one for sellers)
orderBids' :: [Bid] -> OrderBook
orderBids' bids = orderBids bids (OrderBook (emptySkew, emptySkew))




