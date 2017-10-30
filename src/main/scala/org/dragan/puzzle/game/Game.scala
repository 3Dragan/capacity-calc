package org.dragan.puzzle.game

import org.dragan.puzzle._

import scala.collection.mutable

case class Game(
                 initState: GameState,
                 restrics: Seq[Restriction[Task]],
                 waitRules: Seq[Rule[Wait]],
                 taskRules: Seq[Rule[Task]]) {

  private var currentState = initState

  def choices: Set[Choice] = {
    val results: mutable.Set[Choice] = mutable.Set.empty[Choice]

    val moveChoices = Move(currentState.currentGraph).choices()

    results ++= moveChoices.filter(
      choice => restrics.map(_.restrict(choice, currentState)).reduceLeft(_ && _)
    )

    if (!currentState.isAllDevFree)
      results.toSet + Wait(currentState.busyDevInfo.values.map(_.time).min)
    else
      results.toSet
  }


  def makeMove(c: Choice): Unit = {
    c match {
      case t: Task =>
        currentState = taskRules.foldLeft(currentState)((s, r) => r.exec(t, s))
      case w: Wait =>
        currentState = waitRules.foldLeft(currentState)((s, r) => r.exec(w, s))
    }
    println("State: \n" + currentState)
  }

  def score: Long = currentState.score

}

case class Move(currentGraph: OurGraph) {
  def choices(): Set[Task] = {
    if (currentGraph.edges.nonEmpty) {
      val egdeInputs: Set[Task] = currentGraph.edges.map(_._1.toOuter).toSet[Task]
      val egdeOtputs: Set[Task] = currentGraph.edges.map(_._2.toOuter).toSet[Task]

      egdeInputs diff egdeOtputs
    } else {
      currentGraph.nodes.map(_.toOuter).toSet[Task]
    }
  }

  def makeMove(t: Task): OurGraph = {
    currentGraph find t match {
      case Some(_) => currentGraph - t
      case None => throw new UnsupportedOperationException(s"Can not make move $t")
    }
  }
}