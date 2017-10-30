package org.dragan.puzzle

import org.dragan.puzzle.game.Game
import org.dragan.puzzle.strategy.Strategy

import scala.annotation.tailrec

trait GamePlay[D] {
  this: Strategy[Choice, D] =>

  val game: Game

  def run() = playGame(game)

  @tailrec
  private def playGame(game: Game): Unit = {
    update(game.choices)
    makeDecision() match {
      case Some(c) =>
        game.makeMove(c)
        playGame(game)
      case None =>
    }
  }
}
