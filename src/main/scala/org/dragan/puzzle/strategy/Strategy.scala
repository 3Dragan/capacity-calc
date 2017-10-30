package org.dragan.puzzle.strategy


trait Strategy[T, D] {

  def update(t: Set[T]): Unit

  def makeDecision(): Option[T]

  def getDetails: D
}

