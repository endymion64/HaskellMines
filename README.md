# HaskellMines
Minesweeper in Haskell.
This was a small project I had to make for my course Functional Programming in Haskell.
You can play the game in command line and or via GUI.

## Requirements
[Haskell Platform](https://www.haskell.org/platform/)

[Gtk2Hs](http://projects.haskell.org/gtk2hs/download/)

## Build
To build the CLI version :
```
  ghc --make CLI.hs -o MinesCLI -main-is CLI
```

To build the GUI version
```
  ghc --make GUI.hs -o Mines -threaded -main-is GUI
```

