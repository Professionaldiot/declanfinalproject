# **Tic-Tac-Toe and Hangman**

An exploration of Scala, through the programming of tic-tac-toe and hangman.

* Need to knows:
  * The amount of rounds you play in hangman can be adjusted by changing the **_<u>game.maxRounds</u>_** value to a different number
  * Each game has a reset button that will fully reset the game to its original state
    * For hangman this will bring you back to the GUI where you choose to play against a computer or not
    * For tic-tac-toe this will fully reset the board, and open a new clean board (be wary, this will leave behind another Frame if you are playing against the computer).
  * Each game has a **_Main menu_** button that will bring you back to the first GUI that pops up
  * For hangman, if you have the full word on screen, or you are confident in your guess, hit the **_"I think I have the right word"_** button
    * This button runs a series of checks for either a full string in your guess, or checks if you have all the letters in the empty word (the underscores at the beginning), if it fails both checks, then you get to keep trying, but your round will increase, so be careful! ;)
  * Fun facts!
    * There a total of **_<u>1,065</u> lines of code_** in this program.
    * This has gone through so so many versions (and I started tracking the version pretty late too) that I got up to version 3.x.x
      * The computer algorithm for tic-tac-toe itself went through multiple iterations as I developed it, at first it was a match case that called a function, then it was a mountain of if statements, and the final product is a lot of if statements in one big match case for the sums of the rows
    * The computer (tic-tac-toe) uses a sum of each of the rows to figure out where to play
      * But this would result in duplicate answers, and longer code (thus slower), so I decided to use 5 (the result of adding 1+2+2, which is the biggest normal number you can get without winning)
        This also results in a far less buggy program, I have not found a bug, but if you find one, feel free to tell me in class! Just tell me how you did it, and I'll try to replicate it if I can
    * I then use the match cases on the sums (which are in a ListBuffer) and determine what I need to do
      * note: each match case has an if statement telling it to ignore the following if there is a sum of 2 anywhere on the board (a 1, 1, 0 combination), this is so if there is the possibility that you are close to winning, it can stop it.
  * The board inside the Frame is the current positions of everything, player 1 being x's and player 2 being o's
    * should it not update in the current Frame, you can look at the console for the board state, it says there the board state in numbers
  * There is automatic tie and win detection! You don't need to do anything, just play the game and it will work automatically.
  * Should it not tell you who won (or tied), also try to tell me, and I can attempt to figure out why.
  * The song I am listening to as  I write this is : [**_<u>this song!</u>_**](https://music.youtube.com/watch?v=RPRTJSb4Uc4&si=5R-3u1hVdbEEYwK)
  * Here is a picture of my dogs :) [**_<u>for those of you that can't see the image</u>_**](https://photos.app.goo.gl/wd7ZvP7ciZn8TZpZ7)

    <img alt="all 3 of my dogs in a car" height="500" src="C:\Users\Declan\Downloads\PXL_20230615_142146890.jpg" title="Doggies :)"/>

**_<u>Declan Costello, 2023</u>_**