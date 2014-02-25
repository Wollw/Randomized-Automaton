-- To run this program it must be given command line parameters:
--      width:      x dimension of image
--      height:     y dimension of image
--      nodecount:  number of nodes to create for the graph
--      start:      number of frames to skip before saving images
--      stop:       last frame to generate before quiting
--
-- Example usage to create 300 frames of 4096x4096 images
-- using 2014 nodes without skipping any frames:
--      $ graph 4096 4096 1024 0 299
-- 
import Data.Graph
import Data.Tuple.Select
import Control.Monad.State hiding (State)
import Graphics.Rendering.Cairo
import System.Directory
import System.Random
import System.Environment
import System.Exit
import Control.Applicative
import Text.Printf

-- Automaton rule to apply each frame.
rule :: AutomatonGraph -> Node -> Node
rule g node@((state, pos@(x, y)), v, es) =
    case length $ filter (==Off) nStates of
        0 -> ((succ state, pos), v, es)
        3 -> ((succ state, pos), v, es)
        4 -> ((succ state, pos), v, es)
        5 -> ((succ . succ $ state, pos), v, es)
        7 -> ((On 4, pos), v, es)
        _ -> ((pred state, pos), v, es)
  where
    nStates = neighborStates g v


-- Start value for image file names.
idStart :: Int
idStart = 013200

-- An AutomatonGraph is a graph of vertices with nodes
-- containing both a State and Position
type AutomatonGraph = ( Graph
                 , Vertex -> (NodeValue, Vertex, [Vertex])
                 , Int -> Maybe Vertex)
type NodeValue = (State, Position)
type Position = (Int, Int)
type Dimensions = (Width, Height)
type Width = Int
type Height = Int
type RenderRange = (Int, Int)
type Node = (NodeValue, Int, [Int])
type ImageDimensions = (Int, Int)
type NodeCount = Int

-- A node's state is either Off or On.
-- A node that is on contains a value of "on-ness"
-- which is used to determine how bright the node should be
-- when drawn.
data State = Off | On Int deriving (Show, Eq)
instance Enum State where
    succ Off    = On 1
    succ (On n) = On $ n + 1
    toEnum n | n <= 0    = Off
             | otherwise = On n
    fromEnum Off    = 0
    fromEnum (On n) = n

main :: IO ()
main = do
    -- Check argument list
    args <- map read <$> getArgs :: IO [Int]
    when (length args /= 5) $ do
        putStrLn "usage: graph width height nodecount start stop"
        exitFailure

    -- Generate graph
    putStrLn $ ("Node count: " ++) $ show . getNodeCount $ args
    graph <- randomGraph
        (getWidth args, getHeight args)
        $ getNodeCount args

    -- Generate images
    evalStateT (graphState args) graph
  where
    getWidth     = head
    getHeight    = (!! 1)
    getNodeCount = (!! 2)
    getStart     = (!! 3)
    getStop      = (!! 4)
    graphState args = saveGraphState
                    (getWidth args, getHeight args)
                    idStart
                    (getStart args, getStop args)

-- Convert a boolean value to a State
boolToState :: Bool -> State
boolToState False = Off
boolToState True  = On 1

-- Creates a random graph of nodes with positions within the
-- confines of the image dimensions and with a node count
randomGraph :: ImageDimensions -> NodeCount -> IO AutomatonGraph
randomGraph pos n = graphFromEdges <$> randomEdges pos n
randomEdges :: ImageDimensions -> NodeCount -> IO [Node]
randomEdges (width', height') nodeCount = do
    rs <- take (nodeCount * 10)
       <$> randomRs (0, nodeCount - 1)
       <$> newStdGen
    return $ makeNodes nodeCount rs
  where
    makeNodes 0 _ = []
    makeNodes n rs = makeNode (n-1) rs : makeNodes (n-1) (drop 10 rs)
    makeNode v (s:x:y:n1:n2:n3:n4:n5:n6:n7:rs) =
        ( ( boolToState (s `mod` 2 == 0)
          , ( floor $ fromIntegral x
                    / (fromIntegral nodeCount :: Float)
                    * fromIntegral width'
            , floor $ fromIntegral y
                    / (fromIntegral nodeCount :: Float)
                    * fromIntegral height'
            )
          )
        , v
        , [n1,n2,n3,n4,n5,n6,n7]
        )

-- Apply a function to each node in the graph.
mapGraph :: (Node -> Node) -> AutomatonGraph -> AutomatonGraph
mapGraph f (graph, lookup, _) = graphFromEdges
    [ f . lookup $ v | v <- vertices graph ]

-- Returns the states of all of a vertex's neighbors in a list.
neighborStates :: AutomatonGraph -> Vertex -> [State]
neighborStates graph v = map (fst . sel1 . sel2 graph)
                         $ (sel3 . sel2 graph) v

-- Evaluate and write each state of the graph in the
-- range of frames specified.
saveGraphState :: Dimensions -> Int -> RenderRange -> StateT AutomatonGraph IO ()
saveGraphState dimensions fileStartNumber (start, stop) = do
    liftIO $ createDirectoryIfMissing True "img"
    graphState' stop
  where
    graphState' (-1) = return ()
    graphState' count = do
        graph <- get
        liftIO $
            when (count <= stop - start) $ do
                let fileName = printf "%0.6d.png" (stop - start - count + fileStartNumber) :: String
                saveGraphFrame
                    dimensions
                    (printf "img/%s" fileName)
                    graph
        modify (mapGraph (rule graph))
        graphState' $ count - 1

-- Draw and write an image of the current graph state.
saveGraphFrame :: Dimensions -> FilePath -> AutomatonGraph -> IO ()
saveGraphFrame (width', height') fileName graph = do
    putStrLn $ printf "saving %s..." fileName
    withImageSurface FormatRGB24 width' height' $ \surface -> do
        renderWith surface $ sequence
            [ draw . sel2 graph $ v | v <-  vertices . sel1 $ graph ]
        surfaceWriteToPNG surface fileName
    return ()
  where
    circle x y r = arc x y r 0 $ pi * 2
    draw ((s, (x', y')), v, es) = do
        let x = fromIntegral x'
            y = fromIntegral y'
        moveTo x y

        case s of
            On n -> setSourceRGBA (0.05 * fromIntegral n / 2)
                                  0.0
                                  (1.0 / (2 * fromIntegral n))
                                  0.5
            Off  -> setSourceRGBA 0.0 0.0 0.5 0.5

        circle x y $ (sqrt . fromIntegral $ (width' * height')) / (2.0 * 200)
        fill

        sequence_ [ (\((_, (x', y')), _, _) ->
            moveTo x y
            >> lineTo (fromIntegral x') (fromIntegral y')
            >> stroke) $ sel2 graph v'
            | v' <- es ]
        return ()
