import Graphics.UI.Gtk
import Control.Monad
import Control.Monad.Trans(liftIO)
import Control.Concurrent
import Control.Concurrent.MVar
import System.Exit
import qualified Data.Map.Strict as Map
import MyBoard as Board

type Cell = (Int, Int)
data MyGUI = GUI { window :: Window
                 , timer :: Label
                 , counter :: Label
                 , table :: Table
                 , resetButton :: Button
                 }
--main :: IO ()
main = do
    dim <- askBoardSize
    gameLoop dim

--askBoardSize :: () -> IO (Int,Int)
askBoardSize = do
    putStrLn "What is the width of the board?"
    w <- readLn
    putStrLn "What is the height of the board?"
    h <- readLn
    return (w, h)

--makeGuiBoard :: Int -> Int-> IO ()
gameLoop dim@(width, height) = do
    initGUI
    gui <- makeGUI dim

    -- sets the initial board internally
    -- we now allow that the first click in the GUI results in losing the game!
    let theBoard = Board.initialize 2562498 dim (-1,-1) :: MyBoard
    updateTable (table gui) theBoard
    widgetShowAll (window gui)
    mainGUI

generateCoordinates width height = [(x,y)| x <- [0..width-1], y <- [0..height-1]]

-- timer based upon https://www.haskell.org/haskellwiki/Gtk2Hs/Tutorials/ThreadedGUIs
startTimer timeLabel = do
    forkIO $ do let printTime t = do threadDelay 1000000
                                     postGUIAsync $ labelSetText timeLabel (show (t :: Int))
                                     printTime (t+1)
                printTime 1

-- table van buttons 
-- if click or flag
-- maak nieuwe table gebaseerd op resultaat van nieuw MyBoard
-- vervang oude table in box 
-- eerste klik zou timer moeten starten
-- reset knop zou nieuw bord moeten geven, timer resetten en 

makeGUI dim@(w,h)= do
    window <- windowNew
    window `on` deleteEvent $ liftIO mainQuit >> return False

    topbox <- vBoxNew True 1

    timerBox <- hBoxNew True 0
    timerText <- labelNew (Just "Time: ")
    timeLabel <- labelNew (Just "0")
    boxPackStart timerBox timerText PackNatural 0
    boxPackStart timerBox timeLabel PackNatural 0
    boxPackStart topbox timerBox PackNatural 0

    table <- tableNew w h True
    boxPackStart topbox table PackNatural 0

    startButton <- buttonNew
    set startButton [buttonLabel := "Start"]
    startButton `on` buttonActivated $ do void $ startTimer timeLabel
    boxPackStart topbox startButton PackNatural 0

    resetButton <- buttonNew
    set resetButton [buttonLabel := "Reset"]
    resetButton `on` buttonActivated $ do main
    boxPackStart topbox resetButton PackNatural 0

    set window [windowTitle := "Minesweeper", containerBorderWidth := 10 , containerChild := topbox]

    return $ GUI window timeLabel timeLabel table resetButton


onLeftRight :: Button -> IO () -> IO () -> IO ()
onLeftRight btn left right = do
  btn `on` buttonReleaseEvent $ do
        click <- eventButton
        liftIO $ case click of { LeftButton -> left; RightButton -> right }
        return False
  return ()

updateTable table newBoard
    | won newBoard = do --stop timer
                        transformTable table newBoard True
    | lost newBoard = transformTable table newBoard True
    | otherwise = transformTable table newBoard False


transformTable table board endOfGame = do
    let coordinates = generateCoordinates (width board) (height board)
    children <- containerGetChildren table
    mapM_ (\child -> do containerRemove table child
                        widgetDestroy child) 
          children
    mapM_ (updateFieldOfTable table board endOfGame) coordinates

-- dirty in my opinion
updateFieldOfTable table board endOfGame cell@(x,y) = do
    if endOfGame && (isBomb cell board)
       then do image <- imageNewFromFile "img/bomb.png"
               tableAttachDefaults table image x (x+1) y (y+1)
               widgetShow image
       else if (isFlagged cell board)
            then do button <- buttonNew
                    image <- imageNewFromFile "img/flag.png"
                    buttonSetImage button image
                    tableAttachDefaults table button x (x+1) y (y+1)
                    widgetShow button
                    if endOfGame
                       then return ()
                       else onLeftRight button 
                                        (updateTable table (click cell board))
                                        (return ())
            else if (isMasked cell board)
                 then do button <- buttonNew
                         image <- imageNewFromFile "img/masked.png"
                         buttonSetImage button image
                         tableAttachDefaults table button x (x+1) y (y+1)
                         widgetShow button
                         if endOfGame
                            then return ()
                            else onLeftRight button 
                                             (updateTable table (click cell board))
                                             (updateTable table (flag cell board))
                 else do let adjBombs = Map.findWithDefault 0 cell (clickedCells board)
                         adjLabel <- labelNew (Just (show adjBombs))
                         tableAttachDefaults table adjLabel x (x+1) y (y+1)
                         widgetShow adjLabel

attachButtonsToTable table (x,y) = do   
    button <- buttonNew
    set button [buttonLabel := "Hello World"]
    tableAttachDefaults table button x (x+1) y (y+1)
    return ()