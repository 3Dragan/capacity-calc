package org.dragan

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeLikeIn

package object puzzle {

  type MutableGraph[N,E[X] <: EdgeLikeIn[X]] = scalax.collection.mutable.Graph[N,E]

  case class Dev(name: String)

  sealed trait DevState

  case class Busy(time: Long) extends DevState

  case object Free extends DevState

  sealed trait Choice

  case class Task(name: String, d: Dev, estimate: Long) extends Choice

  case class Wait(time: Long) extends Choice

  type OurGraph = Graph[Task, DiEdge]

  case class GameState(score: Long, devs: Map[Dev, DevState], currentGraph: OurGraph) {
    def freeDevs: Stream[Dev] = devs.filter(_._2 == Free).keys.toStream

    def isAllDevFree: Boolean = freeDevs.length == devs.size

    def busyDevInfo: Map[Dev, Busy] = devs.filter(_._2 != Free).mapValues(_.asInstanceOf[Busy])
  }
}
