import Data.Maybe
import Route
import RouteGUI
import Graph (Graph, Edge(..))
import Graph qualified as G
import Data.PSQueue (PSQ(..), Binding(..))
import Data.PSQueue qualified as Q
import Data.Map (Map)
import Data.Map qualified as M

-- Find the shortest path between two nodes (from and to) in a graph (g)
shortestPath :: (Ord a, Ord b) => Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = undefined



-- Dijkstra's algorithm implementation
dijkstra :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ a b
dijkstra g from
  | not $ G.member from g = Q.empty
  | otherwise             = dijkstra' g from pq
    where 
      pq = Q.singleton from 0

dijkstra' :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ a b -> PSQ a b
dijkstra' g v pq 
  | n == Nothing = pq'
  | otherwise    = pq' --dijkstra' g n' pq' -- kör samma skit igen på n (närmsta granne till v)
    where 
      pq' = insertNeighbours g v pq  -- TODO: lägg till alla grannar till v i prioritetskön
      n = closestNeighbour g v pq -- närmsta granne till v
      n' = fst $ fromJust $ n
      d = fromJust $ Q.lookup v pq -- avståndet till v

testing = 
  insertNeighbours testGraph (fst $ fromJust $ closestNeighbour testGraph "A" (Q.singleton "A" 0)) $
  insertNeighbours testGraph "A" (Q.singleton "A" 0)

-- Find the closest neighbour to a node in a graph (g) that is not already in the
-- priority queue (pq)
closestNeighbour :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ a b -> Maybe (a, b)
closestNeighbour g v pq = listToMaybe $ filter (\(k, _) -> k `M.member` adjSet) $ Q.toList pq
  where
    adjEdges = G.adj v g
    adjSet = M.fromList $ map (\(Edge _ w _) -> (w, ())) adjEdges


-- Insert all neighbours of a node (v) in a graph (g) into a priority queue (pq)
-- If a neighbour is already in the priority queue, update its value if the new
-- value is lower than the old value.
insertNeighbours :: (Ord a, Ord b, Num b) => Graph a b -> a -> PSQ a b -> PSQ a b
insertNeighbours g v pq = foldr insertNeighbour pq (G.adj v g)
  where
    insertNeighbour :: (Ord a, Ord b, Num b) => Edge a b -> PSQ a b -> PSQ a b
    insertNeighbour (Edge v' w l) pq'
      | Q.lookup w pq' == Nothing = Q.insert w (l + d) pq'
      | otherwise                 = Q.adjust (min (l + d)) w pq'
      where
        d = fromJust $ Q.lookup v' pq'



-- 1. Gå till första noden i grafen.
-- 2. Lägg till alla grannar till noden i en prioritetskö.
-- 3. Gå till närsmta granne i prioritetskön.
-- 4. Lägg till alla grannar till noden i en prioritetskö.
      -- Om grannen redan finns i prioritetskön, uppdatera dess värde om det är mindre än det nuvarande värdet.
-- 5. Gör steg 3 och 4 tills vi har nått slutnoden.


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

test :: IO ()
test = do
  let g = G.addVertex "hejda" G.empty
  let g' = G.addVertex "hej" g
  let g'' = G.addVertex "hejdasan" g'
  let g''' = G.addEdge "hejda" "hejdasan" 10 g''
  let g'''' = G.addEdge "hejda" "hej" 10 g'''
  let list = G.neighbours "hejda" g''''
  print list

testGraph =
  G.addEdge "B" "H" 5 $
  G.addEdge "A" "B" 1 $
  G.addEdge "A" "C" 2 $
  G.addEdge "B" "D" 3 $
  G.addEdge "C" "D" 4 $
  G.addEdge "D" "E" 5 $
  G.addEdge "E" "F" 3 $
  G.addEdge "E" "G" 4 $
  G.addEdge "F" "H" 2 $
  G.addEdge "G" "H" 1 $
  G.addEdge "H" "I" 4 $
  G.addVertices ["A", "B", "C", "D", "E", "F", "G", "H", "I"] $
  G.empty 

