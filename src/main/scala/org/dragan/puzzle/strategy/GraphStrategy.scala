package org.dragan.puzzle.strategy

import org.dragan.puzzle.strategy.GraphStrategy.{Info, UnChecked}

import scalax.collection.edge.LkDiEdge
import scalax.collection.mutable.Graph
import scalax.collection.edge.Implicits._

case class NodeInfo[T](current: Info[T], children: Set[Info[T]])

trait GraphStrategy[T] extends Strategy[T, Graph[T, LkDiEdge]] {

  val simpleStrategy: SimpleStrategy[T] = new SimpleStrategy[T] {}

   // val oldGrapth: Graph[T, LkDiEdge]

  val graph = Graph.empty[T, LkDiEdge]

  var currentChoice: Option[T] = None

  override def update(t: Set[T]): Unit = {
    currentChoice.foreach(
      c => t.foreach(cc => graph add (c ~+#> cc) (UnChecked))
    )
    simpleStrategy.update(t)
    currentChoice = simpleStrategy.makeDecision()
  }

  override def makeDecision(): Option[T] = {
    currentChoice
  }

  override def getDetails: Graph[T, LkDiEdge] = graph
}

object GraphStrategy {

  sealed trait NodeState

  case object Checked extends NodeState

  case object UnChecked extends NodeState

  type Info[T] = (T, NodeState)
}
