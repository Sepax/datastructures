import Graph

-- Test empty graph
testEmptyGraph :: IO ()
testEmptyGraph = do
  let g = empty :: Graph Int String
      expectedVertices = "[]"
      expectedEdges = "[]"

  putStrLn "Empty Graph:"
  putStrLn $ "Expected Vertices: " ++ expectedVertices
  putStrLn $ "Actual Vertices: " ++ show (vertices g)
  putStrLn $ "Vertices Match: " ++ show (show (vertices g) == expectedVertices)

  putStrLn $ "Expected Edges: " ++ expectedEdges
  putStrLn $ "Actual Edges: " ++ show (edges g)
  putStrLn $ "Edges Match: " ++ show (show (edges g) == expectedEdges)

-- Test adding vertices and edges
testGraphOperations :: IO ()
testGraphOperations = do
  let g = empty :: Graph Int String
      g' = addVertices [1, 2, 3] g
      g'' = addEdge 1 2 "label" g'
      g''' = addBiEdge 2 3 "label" g''
      expectedVertices = "[1,2,3]"
      expectedEdges = "[Edge {src = 1, dst = 2, label = \"label\"},Edge {src = 2, dst = 3, label = \"label\"}]"

  putStrLn "Graph Operations:"
  putStrLn $ "Expected Vertices: " ++ expectedVertices
  putStrLn $ "Actual Vertices: " ++ show (vertices g''')
  putStrLn $ "Vertices Match: " ++ show (show (vertices g''') == expectedVertices)

  putStrLn $ "Expected Edges: " ++ expectedEdges
  putStrLn $ "Actual Edges: " ++ show (edges g''')
  putStrLn $ "Edges Match: " ++ show (show (edges g''') == expectedEdges)

-- Test adjacency
testAdjacency :: IO ()
testAdjacency = do
  let g = empty :: Graph Int String
      g' = addVertices [1, 2, 3] g
      g'' = addEdge 1 2 "label" g'
      g''' = addEdge 1 3 "label" g''
      adjEdges = adj 1 g'''
      expectedAdjEdges = "[Edge {src = 1, dst = 2, label = \"label\"},Edge {src = 1, dst = 3, label = \"label\"}]"

  putStrLn "Adjacency:"
  putStrLn $ "Expected Adjacent Edges of 1: " ++ expectedAdjEdges
  putStrLn $ "Actual Adjacent Edges of 1: " ++ show adjEdges
  putStrLn $ "Adjacent Edges Match: " ++ show (show adjEdges == expectedAdjEdges)

-- Test all
main :: IO ()
main = do
  testEmptyGraph
  putStrLn ""
  testGraphOperations
  putStrLn ""
  testAdjacency

