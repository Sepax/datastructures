import Data.Maybe
import Route
import RouteGUI
import Graph (Graph, Edge(..))
import Graph qualified as G
import Data.PSQueue (Binding(..),PSQ(..))
import Data.PSQueue qualified as Q
import Data.Map (Map)
import Data.Map qualified as M

-- Find the shortest path between two nodes (from and to) in a graph (g)
shortestPath :: (Ord a, Ord b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = undefined



-- Dijkstra's algorithm implementation
dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ a b
dijkstra g from 
  | G.member from g = dijkstra' g (Q.singleton from 0) Q.empty
  | otherwise       = Q.empty 

dijkstra' :: (Ord a, Ord b, Num b) => Graph a b -> PSQ a b -> PSQ a b -> PSQ a b
dijkstra' g q s 
  | Q.null q  = s
  | otherwise = dijkstra' g q' s'
    where
      q'   = Q.deleteMin (foldl (insertEdge s) q (G.adj (Q.key minQ) g))
      s'   = Q.insert (Q.key minQ) (Q.prio minQ) s
      minQ = fromJust $ Q.findMin q

insertEdge :: (Ord a, Ord b, Num b) => PSQ a b -> PSQ a b -> Edge a b -> PSQ a b
insertEdge s q edge 
  | Q.lookup name s /= Nothing = q
  | otherwise                  = Q.insert name weight q
    where 
      name    = dst edge
      weight  = if (Q.lookup name q == Nothing) then
                  label edge + Q.prio minQ
                else
                  min (label edge + Q.prio minQ) (fromJust $ Q.lookup name q)
      minQ    = fromJust $ Q.findMin q


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




