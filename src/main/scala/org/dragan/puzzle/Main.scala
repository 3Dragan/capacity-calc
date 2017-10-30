package org.dragan.puzzle

import org.dragan.puzzle.game._
import org.dragan.puzzle.strategy.{GraphStrategy, SimpleStrategy}

import scalax.collection.Graph
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.edge.LkDiEdge

object Main {

  val dima = Dev("Dima")
  val marek = Dev("Marek")

  def getGraph: Graph[Task, DiEdge] = {

    val mergeTask = Task("1", dima, 1)
    val chain1 = Graph(Task("0", dima, 2) ~> mergeTask)
    val chain2 = Graph(Task("2", marek, 4) ~> mergeTask)

    val us1 = chain1 union chain2

    val middleStepTask = Task("4", dima, 8)

    val us2 = Graph(
      Task("3", marek, 16) ~> middleStepTask,
      middleStepTask ~> Task("5", dima, 32)
    )

    us1 union us2
  }

  def main(args: Array[String]): Unit = {

    val graphs = getGraph

    val theGame = game.Game(
      initState = GameState(0, Map(dima -> Free, marek -> Free), graphs),
      restrics = Seq(MoveRestriction),
      taskRules = Seq(DevBecomeBusyByTheirTask, GraphUpdater),
      waitRules = Seq(ScoreUpdater)
    )


   /* val gamePlay = new GamePlay[MutableGraph[Choice,LkDiEdge]] with GraphStrategy[Choice] {
      override val game: Game = theGame
    }*/

    val gamePlay = new GamePlay[List[Choice]] with SimpleStrategy[Choice] {
      override val game: Game = theGame
    }
    gamePlay.run()

    println(gamePlay.getDetails)
    println(theGame.score)
  }

}





