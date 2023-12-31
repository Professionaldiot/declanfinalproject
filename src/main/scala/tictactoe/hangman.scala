package tictactoe

import tictactoe.game.compLogic

import scala.collection.mutable
import scala.collection.mutable.*
import scala.collection.mutable.ArrayBuffer.*
import scala.io.StdIn.readLine
import scala.swing.*
import scala.util.Random.*
import scala.util.control.Breaks.*

object prev :
  var word = ""

case class newWord(compWord : String) //using above object make sure we cant select the previous word either, this object could be anywhere
//i chose to put it above the class class for readability

def getNewerWord : Vector[newWord] = {
  Vector(
    newWord("hello world"),
    newWord("hangman in scala"),
    newWord("christmas"),
    newWord("computer science"),
    newWord("data"),
    newWord("programming"),
    newWord("coding"),
    newWord("code"),
    newWord("spaghetti code"),
    newWord("deutschland"),
    newWord("halloween"),
    newWord("scala"),
    newWord("java"),
    newWord("java script"),
    newWord("html"),
    newWord("python"),
    newWord("r"),
    newWord("lisp"),
    newWord("assembly"),
    newWord("intellij"),
    newWord("integrated development environment"),
    newWord("array buffer"),
    newWord("char"),
    newWord("c sharp"),
    newWord("string"),
    newWord("integer"),
    newWord("to string"),
    newWord("bugs in my code"),
    newWord("hewlett packard"),
    newWord("sun microsystems"),
    newWord("rip and tear, until it is done"),
    newWord("stardew valley"),
    newWord("doom eternal"),
    newWord("hey there"),
  )
}

object word :
  private var wrongLetters : ArrayBuffer[Any] = ArrayBuffer[Any]()
  private var wrongString : String = ""
  private var idx = 0
  private var wrongLetterNotFound = 0
  private var underscoredWord : String = ""
  var userArray : ArrayBuffer[Char] = ArrayBuffer[Char]() //including spaces
  var underscoreArray : ArrayBuffer[Char] = ArrayBuffer[Char]()

  def reset() : Unit = {
    wrongLetters= ArrayBuffer[Any]()
    wrongString = ""
    idx = 0
    wrongLetterNotFound = 0
    underscoredWord= ""
    userArray = ArrayBuffer[Char]() //including spaces
    underscoreArray = ArrayBuffer[Char]()
    guess.newUserGuess = ' '
    guess.correctWord = ""
    game.guessNew = ""
    game.round = 0
    open.orElse = 0
  }

  def copyArray() : Unit = {
    for elem <- underscoredWord do if elem != ' ' then underscoreArray += elem
  }

  def toArray(word : String) : Any = {
    for i <- word do
      userArray += i
  }

  def wrongLettersToString() : String = {
    wrongString = ""
    for elem <- wrongLetters do wrongString += f"${elem}, "
    wrongString
  }

  def setLetters(userArray : ArrayBuffer[Char], charsToAdd : String, indexToAddCharsAt: Int): ArrayBuffer[Char] = {
    val lettersArray = charsToAdd.toList
    lettersArray.zipWithIndex.foreach {
      case(correctLetters, arrayIndex) => userArray(arrayIndex + indexToAddCharsAt) = correctLetters
    }
    userArray
  }

  def addLetters() : Unit = {
    idx = 0
    wrongLetterNotFound = 0
    while idx < underscoreArray.length do
      val charToAdd = guess.newUserGuess.toString
      if guess.correctWord(idx) == guess.newUserGuess then
        setLetters(underscoreArray, charToAdd, idx)
        wrongLetterNotFound += 1
      else if idx == underscoreArray.length-1 then
        if guess.correctWord(underscoreArray.length-1) == guess.newUserGuess then
          setLetters(underscoreArray, charToAdd, idx)
          wrongLetterNotFound += 1
        else if wrongLetterNotFound == 0 then
          wrongLetters += guess.newUserGuess
      idx += 1
  }

  def toUnderscores(string : ArrayBuffer[Char]) : String = {
    for elem <- string do
      if elem == '\u0020' || elem == '\u0009' || elem == '\u000D' || elem == '\u000A' then
        underscoredWord += " > "
      else if elem == ',' then
        underscoredWord += ","
      else if elem == '\'' then
        underscoredWord += "\'"
      else
        underscoredWord += "_ "
    underscoredWord
  }

object guess :
  var correctWord = ""
  var newUserGuess = ' '
  var wordBool = false
  def guessBool(guess : String) : Unit = {
    if guess == correctWord then wordBool = true
    else if guess.length == 1 then
      breakable {
        for elem <- correctWord do
          if elem == guess(0) then
            wordBool = true
            break
          else wordBool = false
      }
    else wordBool = false
  }

object wind :
  var bool = false
  var stringCheck = ""
  def arrayToString : Unit = {
    stringCheck = ""
    for elem <- word.underscoreArray do
      if elem == '>' || elem == '\u003E' then
        stringCheck += " "
      if elem == '\u002C' then
        println()
      if elem == '\'' then
        stringCheck += "'"
      if elem != '\u003E' && elem != '\'' && elem != '\u002C' then
        stringCheck += elem
  }

object open :
  var orElse : Int = 0

def newText() : Unit = {
  wind.arrayToString
  if wind.stringCheck == guess.correctWord then
    println("You did it! you have guessed the correct word")
  if guess.correctWord == game.guessNew then
    println(f"you did it! you got ${guess.correctWord} in ${game.round-1} rounds!")
  else if open.orElse >= 1 then
    run()
  open.orElse += 1
  if game.round == game.maxRounds then
    println(f"you did not find the word ${guess.correctWord} in ${game.round-1} rounds")
  else
    new Frame() {
      title = "HANGMAN"
      preferredSize = new Dimension(500, 500)
      contents = new GridPanel(7, 5) {
        contents += new TextField("Type your guess in the console, use the button to start the check...", 25)
        contents += new TextField(word.underscoreArray.toString().substring(12,word.underscoreArray.length*3 + 10))//this converts the array to a string that we put on the screen
        contents += new TextField(word.wrongLettersToString())
        contents += new ToggleButton("I think I have guessed the word!") {
          reactions += {
            case event.ButtonClicked(_) =>
              wind.arrayToString
              println(wind.stringCheck)
              if wind.stringCheck == guess.correctWord then
                println(f"You did it! you got the word, ${guess.correctWord}, in ${game.round} rounds!")
              else if guess.correctWord == game.guessNew then
                println(f"You did it! you got the word, ${guess.correctWord}, in ${game.round} rounds!")
              else
                println("Nope, the input that the computer read, does not match the expected word, try again :)")
          }
        }
        contents += new ToggleButton("Reset...") {
          reactions += {
            case event.ButtonClicked(_) =>
              println("Resetting...")
              close()
              word.reset()
              compOrNot()
          }
        }
        contents += new ToggleButton("Make a guess...") {
          reactions += {
            case event.ButtonClicked(enabled_) =>
              close()
              newText()
          }
        }
        contents += new ToggleButton("Back to Main GUI") {
          reactions += {
            case event.ButtonClicked(_) =>
              close()
              GUI()
          }
        }
      }
      pack()
      centerOnScreen()
      open()
      game.goNext
    }
}

def compOrNot() : Unit = {
  new Frame {
    title = "HANG-MAN HOME SCREEN"
    preferredSize = new Dimension(500,500)
    contents = new FlowPanel {
      contents += new ToggleButton("Computer") {
        reactions += {
          case event.ButtonClicked(_) =>
            close()
            compLogic(getNewerWord(nextInt(getNewerWord.size)).compWord)
            newText()
        }
      }
      contents += new ToggleButton("Another Player") {
        reactions += {
          case event.ButtonClicked(_) =>
            word.reset()
            close()
            hmMain()
        }
      }
      contents += new ToggleButton("MAIN MENU") {
        reactions += {
          case event.ButtonClicked(_) =>
            close()
            GUI()
        }
      }
    }
    pack()
    centerOnScreen()
    open()
  }
}

object game :
  var round : Int = 0
  val maxRounds : Int = 15
  var guessNew = ""

  def goNext : Int = {
    round += 1
    round
  }

  def getNewWord : Any = {
    println("Type your word in the console below this.")
    val word = readLine()
    guess.correctWord = word

  }

  def newGuess() : Unit = {
    println("Type your guess below")
    val userGuess = readLine()
    var userChar : Char = ' '
    for elem <- userGuess do userChar = elem
    guess.newUserGuess = userChar
    guessNew = userGuess
  }

  def nextRound : Unit= {
    word.toArray(guess.correctWord)
    word.toUnderscores(word.userArray)
    word.copyArray()
  }

  def compLogic(compWord : String) : Unit = {
    guess.correctWord = compWord
    nextRound
    if prev.word == guess.correctWord then //so you don't get the same word two times in a row
      compLogic(getNewerWord(nextInt(getNewerWord.size)).compWord)
    prev.word = guess.correctWord
  }

def run() : Unit = {
  game.newGuess()
  guess.guessBool(game.guessNew)
  word.addLetters()
}

def hmMain(): Unit = {
  game.getNewWord
  game.nextRound
  newText()
}