-- Strictly plagarism: http://rosettacode.org/wiki/Conway's_Game_of_Life#Clojure

data Grid = Grid
    { width :: Int
    , height :: Int
    , cells :: [[Status]]
    }
    deriving (Show)

data Status =
    Alive
    | Dead
    deriving (Show, Eq)

getCell grid x y =
    cells grid !! y' !! x'
    where
    x' = x `mod` width grid
    y' = y `mod` height grid

neighbours grid x y =
    [getCell grid x' y'
        | x' <- near x
        , y' <- near y
        , x' /= x || y' /= y]
    where
    near i = zipWith (+) (repeat i) [-1..1]

curry3 f x y z = f (x, y, z)
uncurry3 f (x, y, z) = f x y z

livingNeighbours =
    curry3 $ length . filter (== Alive) . uncurry3 neighbours 

evolveCell Dead 3 = Alive
evolveCell Dead _ = Dead
evolveCell Alive n | n > 3 = Dead
evolveCell Alive n | n < 2 = Dead
evolveCell Alive _ = Alive

evolveGrid grid = grid {cells =
    [[evolveCell s (livingNeighbours grid x y)
        | (s, x) <- zip r [0..]]
    | (r, y) <- zip (cells grid) [0..]]}

generations = iterate evolveGrid

makeGrid width height pattern =
    Grid width height validCells
    where
    cells =
        [concatMap
            (\(n, s) -> replicate n s)
            (zip p (cycle [Dead, Alive]))
            | p <- pattern]
    validCells =
        case
            length cells == height &&
            all ((== width) . length) cells of
        True -> cells
        False -> error . show $ cells

showCell Alive = "[X]"
showCell Dead =  " . "
showRow = concatMap showCell
showGrid = map showRow . cells
putGrid = mapM_ putStrLn . showGrid
putGenerations n start =
    mapM_ ((>> putStrLn "") . putGrid)
        (take n $ generations start)

blinker = makeGrid 5 5
    [ [5]
    , [5]
    , [1, 3, 1]
    , [5]
    , [5]
    ]

figureEight = makeGrid 10 10
    [ [10]
    , [10]
    , [2, 3, 5]
    , [2, 3, 5]
    , [2, 3, 5]
    , [5, 3, 2]
    , [5, 3, 2]
    , [5, 3, 2]
    , [10]
    , [10]
    ]

glider = makeGrid 7 7
    [ [7]
    , [2, 1, 4]
    , [3, 1, 3]
    , [1, 3, 3]
    , [7]
    , [7]
    , [7]
    ]

lineGun = makeGrid 40 3
    [ [40]
    , [1, 8, 1, 5, 3, 3, 6, 6, 1, 5, 1]
    , [40]
    ]
