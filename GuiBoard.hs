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

askBoardSize :: IO (Int,Int)
askBoardSize = do
    widthMessage <- labelNew (Just "What is the width of the board?")
    heightMessage <- labelNew (Just "What is the height of the board?")
    dialog <- dialogNew 
    upper <- dialogGetUpper dialog
    widthSpin <- spinButtonNewWithRange 1 20 1
    heightSpin <- spinButtonNewWithRange 1 20 1
    boxPackStart upper widthMessage PackRepel 0
    boxPackStart upper widthSpin PackRepel 5
    boxPackStart upper heightMessage PackRepel 0
    boxPackStart upper heightSpin PackRepel 0
    dialogAddButton dialog "Ok" ResponseOk
    widgetShowAll dialog
    response <- dialogRun dialog
    w <- spinButtonGetValueAsInt widthSpin
    h <- spinButtonGetValueAsInt heightSpin
    widgetDestroy dialog
    return (w,h)

gameLoop :: (Int,Int) -> IO ()
gameLoop dim = do
    -- creates the GUI of the game
    gui <- makeGUI dim
    seed <- randomIO :: IO Int -- a random seed will be used to generate the game board
    -- we now allow that the first click in the GUI results in losing the game!
    let theBoard = MyBoard.initialize seed dim (-1,-1)
    labelSetText (counter gui) (show (Set.size (bombs theBoard)))
    -- starts the timer "on first click" (since it results in a removal of the cell)
    (table gui) `on` remove $ (\_ -> do isTimerOn <- readIORef (timerOn gui)
                                        if isTimerOn
                                        then return ()
                                        else do atomicModifyIORef' (timerOn gui) (\_ -> (True,()))
                                                startTimer (timer gui) >>= (\id -> atomicModifyIORef' (timerThread gui) (\_ -> (id,())))
                                                return ())
    (window gui) `on` deleteEvent $ do liftIO $ stopTimer gui
                                       liftIO mainQuit
                                       return False
    (resetButton gui) `on` buttonActivated $ do stopTimer gui
                                                widgetDestroy (window gui)
                                                postGUIAsync mainQuit
                                                main                                   
    updateTable gui theBoard
    widgetShowAll (window gui)
    mainGUI

generateCoordinates :: Int -> Int -> [(Int,Int)]
generateCoordinates width height = [(x,y)| x <- [0..width-1], y <- [0..height-1]]

-- timer based upon https://www.haskell.org/haskellwiki/Gtk2Hs/Tutorials/ThreadedGUIs
startTimer :: Label -> IO ThreadId
startTimer timeLabel = do
    forkIO $ do let printTime t = do threadDelay 1000000
                                     postGUIAsync $ labelSetText timeLabel (show (t :: Int))
                                     printTime (t+1)
                printTime 1

stopTimer :: MyGUI -> IO ()
stopTimer gui = 
    readIORef (timerOn gui) >>= (\isTimerOn -> if isTimerOn
                                               then readIORef (timerThread gui) >>= killThread
                                               else return ())

makeGUI :: (Int, Int) -> IO MyGUI
makeGUI (w,h)= do
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
    set resetButton [buttonLabel := "Gimme another!"]
    boxPackStart topbox resetButton PackRepel 0

    set window [ windowTitle := "Minesweeper"
               , containerBorderWidth := 5
               , containerChild := topbox
               , windowResizable := False
               , windowWindowPosition := WinPosCenter
               ]

    return $ GUI window timeLabel timerOn timerThread countLabel table resetButton

onLeftRight :: Button -> IO () -> IO () -> IO ()
onLeftRight btn leftClickAction rightClickAction = do
  btn `on` buttonReleaseEvent $ do
        click <- eventButton
        liftIO $ case click of { LeftButton -> leftClickAction; RightButton -> rightClickAction}
        return False
  return ()

updateTable :: MyGUI -> MyBoard -> IO ()
updateTable gui newBoard
    | won newBoard = stopGame gui winMessage newBoard
    | lost newBoard = stopGame gui loseMessage newBoard
    | otherwise = transformTable gui newBoard False

transformTable :: MyGUI -> MyBoard -> Bool -> IO ()
transformTable gui board endOfGame = do
    let coordinates = generateCoordinates (width board) (height board)
    children <- containerGetChildren (table gui)
    mapM_ (\child -> containerRemove (table gui) child >> widgetDestroy child) 
          children
    mapM_ (updateFieldOfTable gui board endOfGame) coordinates

updateFieldOfTable :: MyGUI -> MyBoard -> Bool -> (Int, Int) -> IO ()
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

stopGame :: MyGUI -> String -> MyBoard -> IO ()
stopGame gui message lastBoardToShow = do 
    stopTimer gui
    makeAnnouncement message
    transformTable gui lastBoardToShow True

winMessage :: String
winMessage = "OMG, much win! Wow, amazing!"

loseMessage :: String
loseMessage = "LMFAO, such loser!"

addToCounter :: MyGUI -> Int -> IO ()
addToCounter gui val = labelGetText (counter gui) >>= readIO >>= (\c -> labelSetText (counter gui) . show $ c+val)

makeAnnouncement :: String -> IO ()
makeAnnouncement message = do
    dialog <- messageDialogNew Nothing [] MessageInfo ButtonsOk message
    dialogRun dialog -- blocks program until button is clicked
    widgetDestroy dialog