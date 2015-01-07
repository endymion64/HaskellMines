import Graphics.UI.Gtk

import Control.Monad
import Control.Monad.Trans(liftIO)
import Control.Concurrent

import System.Random
import System.Exit

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IORef

import MyBoard

data MyGUI = GUI { window :: Window
                 , timer :: Label
                 , timerOn :: IORef Bool
                 , timerThread :: IORef ThreadId
                 , counter :: Label
                 , table :: Table
                 , resetButton :: Button
                 }

main :: IO ()
main = do
    initGUI
    dim <- askBoardSize
    gameLoop dim

--askBoardSize :: IO (Int,Int)
askBoardSize = do
    putStrLn "What is the width of the board?"
    w <- readLn
    putStrLn "What is the height of the board?"
    h <- readLn
    --dialog <- dialogNewWithButtons
    --dialogAddButton
    --dialogAddActionWidget 
    return (w, h)

gameLoop :: (Int,Int) -> IO ()
gameLoop dim = do
    -- creates the GUI of the game
    gui <- makeGUI dim
    -- pick a random seed and create a board
    seed <- randomIO :: IO Int
    -- we now allow that the first click in the GUI results in losing the game!
    let theBoard = MyBoard.initialize seed dim (-1,-1) -- for testing i used seed 2562498
    -- do some adjustments to the GUI
    labelSetText (counter gui) (show (Set.size (bombs theBoard)))
    -- sets the timer "on first click" (since it results in a removal of the cell)
    (table gui) `on` remove $ (\_ -> do isTimerOn <- readIORef (timerOn gui)
                                        if isTimerOn
                                        then return ()
                                        else do atomicModifyIORef' (timerOn gui) (\_ -> (True,()))
                                                startTimer (timer gui) >>= (\id -> atomicModifyIORef' (timerThread gui) (\_ -> (id,())))
                                                return ())
    (window gui) `on` deleteEvent $ do liftIO $ readIORef (timerThread gui) >>= killThread
                                       liftIO mainQuit
                                       return False
    (resetButton gui) `on` buttonActivated $ do readIORef (timerThread gui) >>= killThread
                                                widgetDestroy (window gui)
                                                postGUIAsync mainQuit
                                                main 
    -- let the game begin                                     
    updateTable gui theBoard
    widgetShowAll (window gui)
    mainGUI

generateCoordinates width height = [(x,y)| x <- [0..width-1], y <- [0..height-1]]

-- timer based upon https://www.haskell.org/haskellwiki/Gtk2Hs/Tutorials/ThreadedGUIs
startTimer timeLabel = do
    forkIO $ do let printTime t = do threadDelay 1000000
                                     postGUIAsync $ labelSetText timeLabel (show (t :: Int))
                                     printTime (t+1)
                printTime 1

makeGUI dim@(w,h)= do
    window <- windowNew
    
    topbox <- vBoxNew False 5

    timerBox <- hBoxNew True 0
    timerText <- labelNew (Just "Time: ")
    timeLabel <- labelNew (Just "0")
    boxPackStart timerBox timerText PackRepel 0
    boxPackStart timerBox timeLabel PackRepel 0
    boxPackStart topbox timerBox PackRepel 0

    timerOn <- newIORef False
    currentThread <- myThreadId
    timerThread <- newIORef currentThread

    counterBox <- hBoxNew True 0
    counterText <- labelNew (Just "Bombs left: ")
    countLabel <- labelNew (Just "0")
    boxPackStart counterBox counterText PackRepel 0
    boxPackStart counterBox countLabel PackRepel 0
    boxPackStart topbox counterBox PackRepel 0

    table <- tableNew w h True
    boxPackStart topbox table PackNatural 0

    resetButton <- buttonNew
    set resetButton [buttonLabel := "Reset"]
    boxPackStart topbox resetButton PackRepel 0

    set window [windowTitle := "Minesweeper", containerBorderWidth := 5 , containerChild := topbox]

    return $ GUI window timeLabel timerOn timerThread countLabel table resetButton

onLeftRight :: Button -> IO () -> IO () -> IO ()
onLeftRight btn left right = do
  btn `on` buttonReleaseEvent $ do
        click <- eventButton
        liftIO $ case click of { LeftButton -> left; RightButton -> right }
        return False
  return ()

updateTable gui newBoard
    | won newBoard = do makeAnnouncement "OMG, much win! Wow, amazing!"
                        transformTable gui newBoard True
    | lost newBoard = do makeAnnouncement "LMFAO, such loser!"
                         transformTable gui newBoard True
    | otherwise = transformTable gui newBoard False

transformTable gui board endOfGame = do
    let coordinates = generateCoordinates (width board) (height board)
    children <- containerGetChildren (table gui)
    mapM_ (\child -> do containerRemove (table gui) child
                        widgetDestroy child) 
          children
    mapM_ (updateFieldOfTable gui board endOfGame) coordinates

updateFieldOfTable gui board endOfGame cell@(x,y) = do
    let tab = table gui
    if endOfGame && (isBomb cell board)
       then do image <- imageNewFromFile "img/bomb.png"
               tableAttachDefaults tab image x (x+1) y (y+1)
               widgetShow image
       else if (isFlagged cell board)
            then do button <- buttonNew
                    image <- imageNewFromFile "img/flag.png"
                    buttonSetImage button image
                    tableAttachDefaults tab button x (x+1) y (y+1)
                    widgetShow button
                    if endOfGame
                       then return ()
                       else onLeftRight button 
                                        (do addToCounter gui 1
                                            updateTable gui (click cell board))
                                        (return ())
            else if (isMasked cell board)
                 then do button <- buttonNew
                         image <- imageNewFromFile "img/masked.png"
                         buttonSetImage button image
                         tableAttachDefaults tab button x (x+1) y (y+1)
                         widgetShow button
                         if endOfGame
                            then return ()
                            else onLeftRight button 
                                             (updateTable gui (click cell board))
                                             (do addToCounter gui (-1) 
                                                 updateTable gui (flag cell board))
                 else do let adjBombs = Map.findWithDefault 0 cell (clickedCells board)
                         adjLabel <- labelNew (Just (show adjBombs))
                         tableAttachDefaults tab adjLabel x (x+1) y (y+1)
                         widgetShow adjLabel

addToCounter gui val = labelGetText (counter gui) >>= readIO >>= (\xx -> labelSetText (counter gui) . show $ xx+val)

makeAnnouncement message = do
    dialog <- messageDialogNew Nothing [] MessageInfo ButtonsOk message
    dialogRun dialog -- blocks program until button is clicked
    widgetDestroy dialog