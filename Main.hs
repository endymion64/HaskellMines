module Main (main) where

import MyBoard

main = top (\x y z -> initialize x y z :: MyBoard) 

top :: Board b => (Int -> (Int, Int) -> (Int, Int) -> b) -> IO ()
top cinit = do putStrLn "Enter a seed..."
               seed <- readLn
               putStrLn "Enter the width of the board"
               width <- readLn
               putStrLn "Enter the height of the board"
               height <- readLn
               putStrLn "First click..."
               click <- readLn 
               loop $ cinit seed (width, height) click

-- A turn
loop :: Board b => b -> IO ()
loop board
  | won board  = putStrLn $ show board ++ "\n Gratz, you won!!!"
  | lost board = putStrLn $ show board ++ "\n Soz, you lost..."
  | otherwise  = do putStrLn $ show board
                    newBoard <-flag_loop (Just (-1, -1)) board
                    putStrLn "Click..."
                    coord <- readLn
                    loop $ click coord newBoard

-- Place flags
flag_loop :: Board b => Maybe (Int, Int) -> b -> IO b
flag_loop Nothing board = return board
flag_loop (Just coord) board = do putStrLn "Place a flag???"
                                  mcoord <- readLn
                                  flag_loop mcoord (flag coord board)
