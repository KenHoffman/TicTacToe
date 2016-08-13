package main

import scala.annotation.tailrec
import scala.io.StdIn.readLine

object Main {

  case class Row[A](left: A, middle: A, right: A)
  case class Col[A](top: Row[A], middle: Row[A], bottom: Row[A])

  case class Board(matrix: Col[Option[PlayerChoice]])

  sealed trait PlayerChoice
  case object Xs extends PlayerChoice
  case object Os extends PlayerChoice

  case class GameState(board: Board, player: PlayerChoice)

  sealed trait UserError
  case object InvalidMove extends UserError
  case object GameOver extends UserError

  sealed trait UserInput
  case class Cell(column: Int, row: Int) extends UserInput
  case object Quit extends UserInput
  case object IllegalMove extends UserInput

  def update(inputCell: Cell, state: GameState): Either[UserError, GameState] = {
    val matrix = state.board.matrix
    val currentCellState = inputCell match {
      case Cell(0,0) => matrix.top.left
      case Cell(0,1) => matrix.top.middle
      case Cell(0,2) => matrix.top.right
      case Cell(1,0) => matrix.middle.left
      case Cell(1,1) => matrix.middle.middle
      case Cell(1,2) => matrix.middle.right
      case Cell(2,0) => matrix.bottom.left
      case Cell(2,1) => matrix.bottom.middle
      case Cell(2,2) => matrix.bottom.right
    }
    if (currentCellState.isEmpty) {
      val newStateBoard: Board = inputCell match {
        case Cell(0, 0) => Board(Col[Option[PlayerChoice]](matrix.top.copy(left = Some(state.player)),
          matrix.middle, matrix.bottom))
        case Cell(0, 1) => Board(Col[Option[PlayerChoice]](matrix.top.copy(middle = Some(state.player)),
          matrix.middle, matrix.bottom))
        case Cell(0, 2) => Board(Col[Option[PlayerChoice]](matrix.top.copy(right = Some(state.player)),
          matrix.middle, matrix.bottom))

        case Cell(1, 0) => Board(Col[Option[PlayerChoice]](matrix.top,
          matrix.middle.copy(left = Some(state.player)), matrix.bottom))
        case Cell(1, 1) => Board(Col[Option[PlayerChoice]](matrix.top,
          matrix.middle.copy(middle = Some(state.player)), matrix.bottom))
        case Cell(1, 2) => Board(Col[Option[PlayerChoice]](matrix.top,
          matrix.middle.copy(right = Some(state.player)), matrix.bottom))

        case Cell(2, 0) => Board(Col[Option[PlayerChoice]](matrix.top,
          matrix.middle, matrix.bottom.copy(left = Some(state.player))))
        case Cell(2, 1) => Board(Col[Option[PlayerChoice]](matrix.top,
          matrix.middle, matrix.bottom.copy(middle = Some(state.player))))
        case Cell(2, 2) => Board(Col[Option[PlayerChoice]](matrix.top,
          matrix.middle, matrix.bottom.copy(right = Some(state.player))))
      }
      val newStatePlayer = state.player match {
        case Xs => Os
        case Os => Xs
      }
      Right(GameState(newStateBoard, newStatePlayer))
    } else {
      Left(InvalidMove)
    }
  }

  val legalMovePattern = "([0-2]),([0-2])".r

  def getInput(state: GameState): UserInput = {
    val nextMove = state.player match {
      case Xs => "X"
      case Os => "O"
    }
    print(s"Enter row,col for next move for $nextMove, or q to quit: ")
    val input = readLine().trim
    input match {
      case "q" => Quit
      case legalMovePattern(col,row) => Cell(col.toInt, row.toInt)
      case _ => IllegalMove
    }
  }

  def showError(err: UserError): Unit = {
    err match {
      case InvalidMove => println("Invalid move.")
      case GameOver => println("Game over.")
    }
  }

  def printBoard(state: GameState): Unit = {
    println
    printRow(state.board.matrix.top)
    println("-----------")
    printRow(state.board.matrix.middle)
    println("-----------")
    printRow(state.board.matrix.bottom)
    println
  }

  def printRow(row: Row[Option[PlayerChoice]]): Unit = {
    println(s" ${printableCellValue(row.left)} | ${printableCellValue(row.middle)} | ${printableCellValue(row.right)}")
  }

  def printableCellValue(cellValue: Option[PlayerChoice]): String = {
    cellValue match {
      case None => " "
      case Some(Xs) => "X"
      case Some(Os) => "O"
    }
  }

  def boardIsFull(state: GameState): Boolean = {
    val matrix = state.board.matrix
    if (matrix.top.left.isDefined && matrix.top.middle.isDefined && matrix.top.right.isDefined &&
        matrix.middle.left.isDefined && matrix.middle.middle.isDefined && matrix.middle.right.isDefined &&
        matrix.bottom.left.isDefined && matrix.bottom.middle.isDefined && matrix.bottom.right.isDefined)
      true
    else
      false
  }

  def playerHasWon(state: GameState, playerChoice: PlayerChoice): Boolean = {
    val matrix = state.board.matrix
    if
      // check for 3 in a row horizontally
     ((matrix.top.left.contains(playerChoice) && matrix.top.middle.contains(playerChoice) && matrix.top.right.contains(playerChoice)) ||
      (matrix.middle.left.contains(playerChoice) && matrix.middle.middle.contains(playerChoice) && matrix.middle.right.contains(playerChoice)) ||
      (matrix.bottom.left.contains(playerChoice) && matrix.bottom.middle.contains(playerChoice) && matrix.bottom.right.contains(playerChoice)) ||
      // check for 3 in a row vertically
      (matrix.top.left.contains(playerChoice) && matrix.middle.left.contains(playerChoice) && matrix.bottom.left.contains(playerChoice))  ||
      (matrix.top.middle.contains(playerChoice) && matrix.middle.middle.contains(playerChoice) && matrix.bottom.middle.contains(playerChoice))  ||
      (matrix.top.right.contains(playerChoice) && matrix.middle.right.contains(playerChoice) && matrix.bottom.right.contains(playerChoice)) ||
      // check for 3 in a row diagonally
      (matrix.top.left.contains(playerChoice) && matrix.middle.middle.contains(playerChoice) && matrix.bottom.right.contains(playerChoice))  ||
      (matrix.top.right.contains(playerChoice) && matrix.middle.middle.contains(playerChoice) && matrix.bottom.left.contains(playerChoice)))
      true
    else
      false
  }

  @tailrec
  def loop(state: GameState): Unit = {

    if (playerHasWon(state, Xs))
      println("Player X has won -- game over.")
    else if (playerHasWon(state, Os))
      println("Player O has won -- game over.")
    else if (boardIsFull(state))
      println("The board is full, no winner -- game over.")
    else {
      val userInput = getInput(state)

      userInput match {
        case Quit =>  // leave the loop and the game
        case IllegalMove => println("Illegal move."); loop(state)
        case Cell(col, row) =>
          update(Cell(col, row), state) match {
            case Left(error) => showError(error); loop(state)
            case Right(state) => printBoard(state); loop(state)
          }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val initialState =
      GameState(Board(Col[Option[PlayerChoice]](
        Row(None, None, None),
        Row(None, None, None),
        Row(None, None, None))), Xs)
    println("row and col each can be 0, 1, or 2.  Top left is 0,0.")
    printBoard(initialState)
    loop(initialState)
  }
}

