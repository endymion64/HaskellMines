import Graphics.UI.Gtk

--main :: IO ()
main = do
	(w, h) <- askBoardSize
	makeGuiBoard w h

--askBoardSize :: () -> IO (Int,Int)
askBoardSize = do
	putStrLn "What is the width of the board?"
	w <- readLn
	putStrLn "What is the height of the board?"
	h <- readLn
	return (w, h)

--makeGuiBoard :: Int -> Int-> IO ()
makeGuiBoard width height = do
	initGUI
	window <- windowNew
	topbox <- vBoxNew True 10
	table <- tableNew width height True
	attachButtonsToTable table
	set window [containerBorderWidth := 10 , containerChild := table]
	set button [buttonLabel := "Hello World"]
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI

attachButtonsToTable table = do
	width <- 
	height <- 
	x <- [0..width]
	y <- [0..height]
	button <- buttonNew
	tableAttachDefaults table button x x+1 y y+1
