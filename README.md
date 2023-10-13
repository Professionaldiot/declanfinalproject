# **Tic-Tac-Toe and Hangman**

An exploration of Scala, through the programming of tic-tac-toe and hangman.

* Need to knows:
  * The amount of rounds you play in hangman can be adjusted by changing the **_<u>game.maxRounds</u>_** value to a different number
  * Each game has a reset button that will fully reset the game to its original state
    * For hangman this will bring you back to the GUI where you choose to play against a computer or not
    * For tic-tac-toe this will fully reset the board, and re-enabled the buttons you had pressed down
  * Each game has a **_Main menu_** button that will bring you back to the first GUI that pops up
  * For hangman, if you have the full word on screen, or you are confident in your guess, hit the **_"I think I have the right word"_** button
    * This button runs a series of checks for either a full string in your guess, or checks if you have all the letters in the empty word (the underscores at the beginning), if it fails both checks, then you get to keep trying, but your round will increase, so be careful! ;)

**_<u>Declan Costello, 2023</u>_**