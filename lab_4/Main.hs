import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.PSQueue (Binding (..), PSQ (..))
import Data.PSQueue qualified as Q
import Graph (Edge (..), Graph)
import Graph qualified as G
import Route
import RouteGUI
import System.Environment (getArgs)

-- Find the shortest path between two nodes (from and to) in a graph (g)
shortestPath :: (Ord a, Ord b, Num b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to
  | list == [] || not (G.member from g || G.member to g) = Nothing -- if no path exists or if either of the nodes are not in the graph
  | otherwise = Just (list, weight) -- Return the path and the weight of the path
  where
    list = pathList pq from to -- path of stops
    weight = label (fromJust (Q.lookup to pq)) -- weight of path
    pq = dijkstra g from -- priority queue of shortest paths from source to all other nodes

-- Get the nodes of the shortest path between two node from the priority queue
pathList :: (Ord a, Ord b, Num b) => PSQ a (Edge a b) -> a -> a -> [a]
pathList psq from to
  | elem == Nothing = [] -- if the destination is not in the priority queue
  | to /= from = (pathList psq from prev) ++ [to] -- if node is not the source, add it to the list and find next node
  | otherwise = [from] -- if node is the source, add it to the list and return
  where
    elem = Q.lookup to psq -- Edge of destination node
    prev = src (fromJust elem) -- Previus node found in destination node edge

-- Dijkstra's algorithm implementation
dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ a (Edge a b)
dijkstra g from
  | G.member from g = dijkstra' g (Q.singleton from (Edge from from 0)) Q.empty -- start recursive function with source node in the "q" priority queue
  | otherwise = Q.empty -- if source is not in graph, return empty priority queue

-- Helper function for dijkstra (recursive)
-- g = graph
-- q = priority queue of nodes to be visited
-- s = priority queue of nodes already visited
dijkstra' :: (Ord a, Ord b, Num b) => Graph a b -> PSQ a (Edge a b) -> PSQ a (Edge a b) -> PSQ a (Edge a b)
dijkstra' g q s
  | Q.null q = s -- when all nodes have been visited, return the priority queue of visited nodes
  | otherwise = dijkstra' g q' s' -- if not all nodes have been visited, continue with next node
  where
    v = fromJust $ Q.findMin q -- node with the lowest weight in q
    vName = Q.key v -- name of node with lowest weight in q
    vEdge = Q.prio v -- edge of node with lowest weight in q
    q' = Q.deleteMin (foldl (addNeighbour s) q (G.adj vName g)) -- add neighbours of node with lowest weight to q (then delete node from q)
    s' = Q.insert vName vEdge s -- add node with lowest weight to visited nodes

-- Insert edge into priority queue if an edge with smaller wheight and same source is not already in priority queue
-- q = priority queue of nodes to be visited
-- s = priority queue of nodes already visited
-- e = edge to be inserted
addNeighbour :: (Ord a, Ord b, Num b) => PSQ a (Edge a b) -> PSQ a (Edge a b) -> Edge a b -> PSQ a (Edge a b)
addNeighbour s q e
  | isVisited = q -- if neighbour of node is already visited, do nothing
  | otherwise = Q.insert neighbour minEdge q -- otherwise, add neighbour to q
  where
    isVisited = Q.lookup neighbour s /= Nothing -- check if neighbour is already visited
    neighbour = dst e -- neighbour of node (key)
    source = src e -- source of node (key)
    min = fromJust $ Q.findMin q -- node with the lowest weight in q
    minEdge = min newEdge oldEdge -- compare edges and choose the one with the lowest weight
    newEdge = Edge source neighbour (label e + label (Q.prio min)) -- new edge to be inserted into q
    oldEdge = fromMaybe newEdge (Q.lookup neighbour q) -- if neighbour already in q, get edge of neighbour, otherwise set to newEdge

main :: IO ()
main = do
  -- Get command-line arguments
  args <- getArgs

  -- Check if two arguments are provided
  if length args /= 4
    then putStrLn "Please provide four arguments."
    else do
      Right stops <- readStops $ head args -- First text file argument
      Right lines <- readLines $ args !! 1 -- Second text file argument
      let src = args !! 2 -- Source stop
          dst = args !! 3 -- Destination stop

          -- Generate path from data
          graph = graphBuilder stops lines
          path = shortestPath graph src dst
      case path of
        Nothing -> print $ 2 ^ 31 - 1
        Just (list, time) -> do
          print time
          putStr $ unlines list

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "data/stops-gbg.txt"
  Right lines <- readLines "data/lines-gbg.txt"
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
