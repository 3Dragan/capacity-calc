package org.dragan.puzzle

import scala.annotation.tailrec
import scala.collection.mutable
import scalax.collection.Graph
import scalax.collection.GraphPredef._

object Main {

  def main(args: Array[String]): Unit = {
    val dima = Dev("Dima")
    val marek = Dev("Marek")

    val mergeTask = Task("1", dima, 15)
    val chain1 = Graph(Task("0", dima, 10) ~> mergeTask)
    val chain2 = Graph(Task("2", marek, 5) ~> mergeTask)

    val us1 = chain1 union chain2


    val middleStepTask = Task("4", dima, 4)

    val us2 = Graph(
      Task("3", marek, 5) ~> middleStepTask,
      middleStepTask ~> Task("5", dima, 12)
    )

    val graphs = us1 union us2

    val restric: Seq[Restriction[Task]] = Seq(MoveRestriction)

    val game = Game(
      initState = GameState(0, Map(dima -> Free, marek -> Free), graphs),
      restrics = restric,
      taskRules = Seq(DevBecomeBusyByTheirTask, GraphUpdater),
      waitRules = Seq(ScoreUpdater)
    )


    val globalHistory = getAllPath(game)
    println("Number of options:" + globalHistory.size)
    globalHistory.foreach(println)
    /*
    val (path, score) = playGame(game)
    println("Done")
    println("Final score:" + score)
    println(path)*/
  }

  @tailrec
  def getAllPath(game: Game,
                 history: Set[Seq[Choice]] = Set.empty): Set[Seq[Choice]] = {
    val newHistory = history + getAnotherPath(game.copy(), history)._1
    if (newHistory.size != history.size) {
      getAllPath(game, newHistory)
    } else {
      newHistory
    }
  }


  @tailrec
  def getAnotherPath(game: Game,
                     history: Set[Seq[Choice]] = Set.empty,
                     curPath: Seq[Choice] = Seq.empty): (Seq[Choice], Long) = {
    game.choices match {
      case s: Set[Choice] if s.nonEmpty =>
        s.find(c =>
          history.exists(seq => !seq.startsWith(curPath :+ c))
        ) match {
          case Some(choice) =>
            game.makeMove(choice)
            getAnotherPath(game, history, curPath :+ choice)
          case None =>
            val choice = s.head
            game.makeMove(choice)
            getAnotherPath(game, history, curPath :+ choice)
        }
      case _ =>
        (curPath, game.score)
    }
  }

  @tailrec
  def playGame(game: Game, l: List[Choice] = List.empty): (List[Choice], Long) = {
    game.choices match {
      case s: Set[Choice] if s.nonEmpty =>
        val choice = s.head
        print("Choice: " + choice)
        game.makeMove(choice)
        playGame(game, l :+ choice)
      case _ =>
        (l, game.score)
    }
  }

  sealed trait Chain {
    val l: List[Choice]
  }

  case class Finished(l: List[Choice]) extends Chain

  case class NotFinished(l: List[Choice]) extends Chain

  trait Strategy {
    val state: StrategyState
  }

  trait StrategyState

}






