import System.Environment (getArgs)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.PSQueue (Binding (..), PSQ (..))
import Data.PSQueue qualified as Q
import Graph (Edge (..), Graph)
import Graph qualified as G
import Route
import RouteGUI

-- Find the shortest path between two nodes (from and to) in a graph (g)
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to
  | list == [] || not (G.member from g || G.member to g) = Nothing
  | otherwise = Just (list, weight)
  where 
    list = pathList pq from to
    weight = label (fromJust (Q.lookup to pq))
    pq = dijkstra g from


pathList :: (Ord a, Ord b, Num b) => PSQ a (Edge a b) -> a -> a -> [a]
pathList psq from to
  | elem == Nothing = []
  | to /= from      = (pathList psq from prev) ++ [to]
  | otherwise       = [from]
    where
      elem = Q.lookup to psq
      prev = src (fromJust elem)

-- Dijkstra's algorithm implementation
dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ a (Edge a b)
dijkstra g from
  | G.member from g = dijkstra' g (Q.singleton from (Edge from from 0)) Q.empty
  | otherwise = Q.empty

dijkstra' :: (Ord a, Ord b, Num b) => Graph a b -> PSQ a (Edge a b) -> PSQ a (Edge a b) -> PSQ a (Edge a b)
dijkstra' g q s
  | Q.null q = s
  | otherwise = dijkstra' g q' s'
  where
    q' = Q.deleteMin (foldl (insertEdge s) q (G.adj (Q.key minQ) g))
    s' = Q.insert (Q.key minQ) (Q.prio minQ) s
    minQ = fromJust $ Q.findMin q

insertEdge :: (Ord a, Ord b, Num b) => PSQ a (Edge a b) -> PSQ a (Edge a b) -> Edge a b -> PSQ a (Edge a b)
insertEdge s q e
  | (Q.lookup node s /= Nothing) && (Q.lookup node q == Nothing) = q
  | otherwise = Q.insert node newEdge q
  where
    node = dst e
    source = src e
    newEdge = if (isJust (Q.lookup node s) || (Q.lookup node q == Nothing))
    	then Edge source node (label e + label (Q.prio minQ))
      else min (Edge source node (label e + label (Q.prio minQ))) (fromJust $ Q.lookup node q)
    minQ = fromJust $ Q.findMin q

main :: IO ()
main = do
  -- Get command-line arguments
  args <- getArgs

  -- Check if two arguments are provided
  if length args /= 4
    then putStrLn "Please provide four arguments."
  else do
    Right stops <- readStops $ head args  -- First text file argument
    Right lines <- readLines $ args !! 1  -- Second text file argument
    let src = args !! 2  -- Source stop
        dst = args !! 3  -- Destination stop

        -- Generate path from data
        graph = graphBuilder stops lines
        path  = shortestPath graph src dst
    case path of 
      Nothing -> print $ 2^31 -1
      Just (list,time) -> do
        print time
        putStr $ unlines list
    




startGUI :: IO ()
startGUI = do
  Right stops <- readStops "data/stops-nopath.txt"
  Right lines <- readLines "data/lines-nopath.txt"
  let graph = graphBuilder stops lines
  runGUI stops lines graph shortestPath

graphBuilder :: [Stop] -> [LineTable] -> Graph String Integer
graphBuilder stops lines = foldr addLineTableEdges initialGraph lines
  where
    initialGraph = G.addVertices (map name stops) G.empty

    addLineTableEdges :: LineTable -> Graph String Integer -> Graph String Integer
    addLineTableEdges lineTable graph = foldr addEdge graph (stopPairs lineTable)
      where
        stopPairs :: LineTable -> [(LineStop, LineStop)]
        stopPairs (LineTable _ lineStops) = zip lineStops (tail lineStops)

        addEdge :: (LineStop, LineStop) -> Graph String Integer -> Graph String Integer
        addEdge ((LineStop stop1 _), (LineStop stop2 time)) g =
          G.addEdge stop1 stop2 time g

testGraph =
  G.addBiEdge "A" "B" 1 $
    G.addBiEdge "A" "C" 2 $
      G.addBiEdge "B" "D" 3 $
        G.addBiEdge "C" "D" 4 $
          G.addBiEdge "D" "E" 5 $
            G.addBiEdge "E" "F" 3 $
              G.addBiEdge "E" "G" 4 $
                G.addBiEdge "F" "H" 2 $
                  G.addBiEdge "G" "H" 1 $
                    G.addBiEdge "H" "I" 4 $
                      G.addVertices ["A", "B", "C", "D", "E", "F", "G", "H", "I"] $
                        G.empty
