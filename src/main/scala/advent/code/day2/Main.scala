package advent.code.day2

import advent.code.file.reader.FileReader

/*
  --- Day 2: Rock Paper Scissors ---
  The Elves begin to set up camp on the beach. To decide whose tent gets to
  be closest to the snack storage, a giant Rock Paper Scissors tournament is
  already in progress.

  Rock Paper Scissors is a game between two players. Each game contains many
  rounds; in each round, the players each simultaneously choose one of Rock,
  Paper, or Scissors using a hand shape. Then, a winner for that round is
  selected: Rock defeats Scissors, Scissors defeats Paper, and Paper defeats
  Rock. If both players choose the same shape, the round instead ends in a draw.

  Appreciative of your help yesterday, one Elf gives you an encrypted strategy
  guide (your puzzle input) that they say will be sure to help you win. "The
  first column is what your opponent is going to play: A for Rock, B for Paper,
  and C for Scissors. The second column--" Suddenly, the Elf is called away to
  help with someone's tent.

  The second column, you reason, must be what you should play in response:
  X for Rock, Y for Paper, and Z for Scissors. Winning every time would be
  suspicious, so the responses must have been carefully chosen.

  The winner of the whole tournament is the player with the highest score.
  Your total score is the sum of your scores for each round. The score for
  a single round is the score for the shape you selected (1 for Rock, 2 for
  Paper, and 3 for Scissors) plus the score for the outcome of the round (0 if
  you lost, 3 if the round was a draw, and 6 if you won).

  Since you can't be sure if the Elf is trying to help you or trick you, you
  should calculate the score you would get if you were to follow the strategy
  guide.

  For example, suppose you were given the following strategy guide:

  A Y
  B X
  C Z

  This strategy guide predicts and recommends the following:

  In the first round, your opponent will choose Rock (A), and you should choose
  Paper (Y). This ends in a win for you with a score of 8 (2 because you chose
  Paper + 6 because you won).
  In the second round, your opponent will choose Paper (B), and you should
  choose Rock (X). This ends in a loss for you with a score of 1 (1 + 0).
  The third round is a draw with both players choosing Scissors, giving you a
  score of 3 + 3 = 6.
  In this example, if you were to follow the strategy guide, you would get a
  total score of 15 (8 + 1 + 6).

  What would your total score be if everything goes exactly according to your
  strategy guide?

  --- Part Two ---

  The Elf finishes helping with the tent and sneaks back over to you. "Anyway,
  the second column says how the round needs to end: X means you need to lose,
  Y means you need to end the round in a draw, and Z means you need to win.
  Good luck!"

  The total score is still calculated in the same way, but now you need to
  figure out what shape to choose so the round ends as indicated. The example
  above now goes like this:

  In the first round, your opponent will choose Rock (A), and you need the round
  to end in a draw (Y), so you also choose Rock. This gives you a score of
  1 + 3 = 4.
  In the second round, your opponent will choose Paper (B), and you choose Rock
  so you lose (X) with a score of 1 + 0 = 1.
  In the third round, you will defeat your opponent's Scissors with Rock for
  a score of 1 + 6 = 7.
  Now that you're correctly decrypting the ultra top secret strategy guide, you
  would get a total score of 12.

  Following the Elf's instructions for the second column, what would your total
  core be if everything goes exactly according to your strategy guide?

 */

object Main {
  def main(args: Array[String]): Unit = {
    val lines = FileReader.getLines("src/main/resources/day2/input")
    // Part One
    val partOneLines = lines.map(string => {
      val array = string.split(" ").map(_.trim)
      ScoreCounter.countScore(HandShape(array.head), HandShape(array.tail.head))
    })
    println(partOneLines.sum)
    // Part Two
    val partTwoLines = lines.map(string => {
      val array = string.split(" ").map(_.trim)
      ScoreCounter.countScore(HandShape(array.head), GameResult(array.tail.head))
    })
    println(partTwoLines.sum)
  }
}

sealed trait HandShape
case object Rock extends HandShape
case object Paper extends HandShape
case object Scissors extends HandShape

object HandShape {
  def apply(value: String): HandShape = value match {
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scissors
  }
}

sealed trait GameResult
case object Win extends GameResult
case object Lose extends GameResult
case object Draw extends GameResult

object GameResult {
  def apply(value: String): GameResult = value match {
    case "X" => Lose
    case "Y" => Draw
    case "Z" => Win
  }
  def getResult(handShape1: HandShape, handShape2: HandShape): GameResult = (handShape1, handShape2) match {
    case (p1, p2) if p1 == p2                                 => Draw
    case (Rock, Scissors) | (Paper, Rock) | (Scissors, Paper) => Lose
    case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => Win
  }

  def getHandShapeOfResult(handShape: HandShape, gameResult: GameResult): HandShape =
    gameResult match {
      case Win =>
        handShape match {
          case Rock     => Paper
          case Paper    => Scissors
          case Scissors => Rock
        }
      case Lose =>
        handShape match {
          case Rock     => Scissors
          case Paper    => Rock
          case Scissors => Paper
        }
      case Draw => handShape
    }
}

object ScoreCounter {
  def countGameResultScore(gameResult: GameResult): Int = gameResult match {
    case Win  => 6
    case Draw => 3
    case Lose => 0
  }

  def countHandShapeScore(handShape: HandShape): Int = handShape match {
    case Rock     => 1
    case Paper    => 2
    case Scissors => 3
  }

  def countScore(player1: HandShape, player2: HandShape): Int = {
    val handShapeScore = ScoreCounter.countHandShapeScore(player2)
    val gameResultScore = ScoreCounter.countGameResultScore(GameResult.getResult(player1, player2))
    handShapeScore + gameResultScore
  }

  def countScore(player1: HandShape, gameResult: GameResult): Int = {
    val gameResultScore = ScoreCounter.countGameResultScore(gameResult)
    val handShapeScore = ScoreCounter.countHandShapeScore(GameResult.getHandShapeOfResult(player1, gameResult))
    gameResultScore + handShapeScore
  }
}
