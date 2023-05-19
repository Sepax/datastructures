import Data.Maybe
import Route
import RouteGUI
import Graph (Graph, Edge)
import Graph qualified as G

-- Find the shortest path between two nodes (from and to) in a graph (g)
shortestPath :: (Ord a, Ord b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = case (dijkstra g from) of
  []        -> Nothing
  _ -> path to (dijkstra g from)
    where
      paths = dijkstra g from
      path :: (Ord a, Ord b) => a -> [(a,b)] -> Maybe (a,b)
      path _ []   = Nothing
      path e (x@(f,s):xs)
        | f /= e    = path e xs      
        | otherwise = Just x



-- Dijkstra's algorithm implementation
dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> a -> [([a],b)]
dijkstra g from
  | not $ G.member from g = []
  | otherwise             = undefined




main :: IO ()
main = undefined -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "data/stops-air.txt"
  Right lines <- readLines "data/lines-air.txt"
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