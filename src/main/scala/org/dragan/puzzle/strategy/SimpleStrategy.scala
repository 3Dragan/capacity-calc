package org.dragan.puzzle.strategy

import scala.collection.mutable.ListBuffer

trait SimpleStrategy[T] extends Strategy[T, List[T]] {
  val choiceList: ListBuffer[Option[T]] = ListBuffer.empty

  override def update(t: Set[T]): Unit =
    choiceList += t.headOption

  override def makeDecision(): Option[T] = choiceList.last

  override def getDetails: List[T] = choiceList.flatten.toList
}
