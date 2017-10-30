package org.dragan.puzzle.game

import org.dragan.puzzle.{GameState, Task}

sealed trait Restriction[-T] {
  def restrict: (T, GameState) => Boolean
}

case object MoveRestriction extends Restriction[Task] {
  override def restrict: (Task, GameState) => Boolean =
    (choice, state) => {
      state.freeDevs.contains(choice.d)
    }
}