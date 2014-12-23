import Graphics.UI.Gtk

main :: IO ()
main = do
	initGUI
	window <- windowNew
	button <- buttonNew
	set window [ c o n t a i n e r B o r d e r W i d t h := 10 , cont ainerCh ild := button ]
	set button [ buttonLabel := " Hello World " ]
	onClicked button ( putStrLn " Hello World ")
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
