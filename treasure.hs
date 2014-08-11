import System.IO

main = do
	contents <- readFile "map.txt"
	let maze = lines contents
	if isMazeValid maze
		then do
			putStrLn "Maze is Valid ..."
			putStrLn contents
			let (pathExists, newMaze) = findPath 0 0 maze in
				if pathExists
				then do
						putStrLn "Path exists ..."
						putStrLn $ printMaze newMaze 0
				else do
						putStrLn "Path does NOT exist ..."
						putStrLn $ printMaze newMaze 0
		else putStrLn "Error: Invalid Maze"

isMazeValid :: [String] -> Bool
isMazeValid maze =
	let rowSize = length (maze !! 0)
	in all (\row -> (length row == rowSize)) maze

findPath :: Int -> Int -> [String] -> (Bool, [String])
findPath x y maze
	| x < 0 || y < 0 = (False, maze) -- Out of maze bounds
	| y >= (length maze) = (False, maze) -- Out of maze bounds
	| x >= (length $ maze !! y) = (False, maze) -- Out of maze bounds
	| checkTile x y maze == '#' = (False, maze) -- Blocked
	| checkTile x y maze == '+' = (False, maze) -- Visited
	| checkTile x y maze == '@' = (True, maze) -- Treasure
	| let (pathExists, newMaze) = findPath x (y-1) (iterateMaze x y maze) in pathExists = (True, let (pathExists, newMaze) = findPath x (y-1) (iterateMaze x y maze) in newMaze)
	| let (pathExists, newMaze) = findPath (x+1) y (iterateMaze x y maze) in pathExists = (True, let (pathExists, newMaze) = findPath (x+1) y  (iterateMaze x y maze) in newMaze)
	| let (pathExists, newMaze) = findPath x (y+1) (iterateMaze x y maze) in pathExists = (True, let (pathExists, newMaze) = findPath x (y+1) (iterateMaze x y maze) in newMaze)
	| let (pathExists, newMaze) = findPath (x-1) y (iterateMaze x y maze) in pathExists = (True, let (pathExists, newMaze) = findPath (x-1) y (iterateMaze x y maze) in newMaze)
	| otherwise = (False, maze)

checkTile :: Int -> Int -> [String] -> Char
checkTile x y maze = maze !! y !! x

iterateMaze :: Int -> Int -> [String] -> [String]
iterateMaze n m (x:xs)
	| m == 0 = ((setTileToVisited n x):xs)
	| otherwise = x:iterateMaze n (m-1) xs

setTileToVisited :: Int -> String -> String
setTileToVisited n (x:xs)
	| n == 0 = ('+':xs)
	| otherwise = x:setTileToVisited (n-1) xs
	
setUselessTiles :: String -> String
setUselessTiles str =
	map (\c -> if (c == '-') then '!' else c) str
	
printMaze :: [String] -> Int -> String
printMaze maze n
	| length maze == n = ""
	| otherwise = do
					setUselessTiles (maze !! n) ++ "\n" ++ printMaze maze (n+1)