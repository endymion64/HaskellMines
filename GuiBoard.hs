import Graphics.UI.Gtk
import Control.Monad
import Control.Monad.Trans(liftIO)
import Control.Concurrent
import Control.Concurrent.MVar
import System.Random
import System.Exit
import qualified Data.Set as Set
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
    --windowSetModal dialog 
    return (w, h)

--gameLoop :: (Int,Int) -> IO ()
gameLoop dim = do
    gui <- makeGUI dim
    seed <- randomIO :: IO Int
    putStrLn $ show seed
    -- sets the initial board internally
    -- we now allow that the first click in the GUI results in losing the game!
    let theBoard = Board.initialize 2562498 dim (-1,-1) :: MyBoard -- currently a fixed seed for testing
    labelSetText (counter gui) (show (Set.size (bombs theBoard)))
    -- per flag -1
    -- per unflag +1
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

-- eerste klik zou timer moeten starten
-- reset knop zou nieuw bord moeten geven, timer resetten en COUNTER

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

    counterBox <- hBoxNew True 0
    counterText <- labelNew (Just "Bombs left: ")
    countLabel <- labelNew (Just "0")
    boxPackStart counterBox counterText PackNatural 0
    boxPackStart counterBox countLabel PackNatural 0
    boxPackStart topbox counterBox PackNatural 0

    table <- tableNew w h True
    boxPackStart topbox table PackNatural 0

    startButton <- buttonNew
    set startButton [buttonLabel := "Start"]
    startButton `on` buttonActivated $ do void $ startTimer timeLabel
    boxPackStart topbox startButton PackNatural 0

    resetButton <- buttonNew
    set resetButton [buttonLabel := "Reset"]
    resetButton `on` buttonActivated $ do main -- delete previous window???
    boxPackStart topbox resetButton PackNatural 0

    set window [windowTitle := "Minesweeper", containerBorderWidth := 10 , containerChild := topbox]

    return $ GUI window timeLabel countLabel table resetButton


onLeftRight :: Button -> IO () -> IO () -> IO ()
onLeftRight btn left right = do
  btn `on` buttonReleaseEvent $ do
        click <- eventButton
        liftIO $ case click of { LeftButton -> left; RightButton -> right }
        return False
  return ()

updateTable gui newBoard
    | won newBoard = do putStrLn "OMG, much win! Wow, amazing!"
                        transformTable gui newBoard True
    | lost newBoard = do putStrLn "LMFAO, such loser!"
                         transformTable gui newBoard True
    | otherwise = transformTable gui newBoard False

transformTable gui board endOfGame = do
    let coordinates = generateCoordinates (width board) (height board)
    children <- containerGetChildren (table gui)
    mapM_ (\child -> do containerRemove (table gui) child
                        widgetDestroy child) 
          children
    mapM_ (updateFieldOfTable gui board endOfGame) coordinates

-- dirty in my opinion
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

addToCounter gui val = labelGetText (counter gui) >>= (\prev -> readIO prev >>= (\xx -> labelSetText (counter gui) . show $ xx+val ))