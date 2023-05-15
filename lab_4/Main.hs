import Route
import RouteGUI
import Graph
import Data.Maybe

shortestPath :: Graph a b -> a -> a -> Maybe ([a], b)
shortestPath g from to = undefined -- TODO: implement Dijkstra's algorithm

main :: IO ()
main = undefined  -- TODO: read arguments, build graph, output shortest path

startGUI :: IO ()
startGUI = do
  Right stops <- readStops "data/stops-air.txt"
  Right lines <- readLines "data/lines-air.txt"
  let graph = undefined -- TODO: build your graph here using stops and lines
  runGUI stops lines graph shortestPath
