package tournament

import java.time.LocalDate
import app.{Player, Result}
import utils.Table

/**
  * Created by nikos on 20/11/2016.
  */
abstract sealed class Node {
  val colWidth: Int = tournament.colWidth
  def winner: Option[Player]
  def finished: Boolean
}

/************************************************************************************/

case class Game(var p1: Option[Player] = None,
                var p2: Option[Player] = None,
                var score1: Option[Int] = None,
                var score2: Option[Int] = None,
                leftChild: Node,
                rightChild: Node) extends Node {

	var created: Option[LocalDate] = None
	updateDate()

	def updateDate(date: Option[LocalDate] = None): Unit = {
		created =
			if (p1.isDefined && p2.isDefined && !finished) {
        if (date.isDefined) date
        else Some(LocalDate.now())
      } else None
	}

  def setScore(p1Score: Int, p2Score: Int): Unit = { score1 = Some(p1Score); score2 = Some(p2Score)}
  def deleteScore(): Unit = {score1 = None ; score2 = None}
  def finished: Boolean = leftChild.finished && rightChild.finished && ((p1.isEmpty && p2.isEmpty) || (p1.isDefined && p2.isEmpty) || (p1.isEmpty && p2.isDefined) || (score1.isDefined && score2.isDefined))
  def isBetween(player1: String, player2: String): Boolean = isBetween(tournament.player(player1), tournament.player(player2))
	def opponent(id: String): Option[Player] = if (p1.isDefined && p1.get.id == id) p2 else if (p2.isDefined && p2.get.id == id) p1 else None
  def isBetween(player1: Player, player2: Player): Boolean = {
    (p1.isDefined && p2.isDefined) &&
      ((p1.get.id == player1.id && p2.get.id == player2.id) ||
        (p2.get.id == player1.id && p1.get.id == player2.id))
  }

  def plays(p: String): Boolean = p1.isDefined && p2.isDefined && (p1.get.id == p || p2.get.id == p)

  def setScore(result: Result): Boolean = {
//    if (result.p1Score == result.p2Score) return false
    if (finished || !isBetween(result.p1, result.p2)) return false
    if (p1.get.id == result.p1 && p2.get.id == result.p2) setScore(result.p1Score, result.p2Score)
    else setScore(result.p2Score, result.p1Score)

		updateDate()
    true
  }

  def deleteResult(result: Result): Boolean = {
    if (!finished || !isBetween(result.p1, result.p2)) false
    else {
      deleteScore()
			updateDate()
      true
    }
  }

  def winner: Option[Player] =
    if (!finished) None
    else if (p1.isEmpty) p2
    else if (p2.isEmpty) p1
    //      else if (p1.get.id.toDouble < p2.get.id.toDouble) p1
    else if (score1.get > score2.get) p1
    else if (score1.get < score2.get) p2
		else None

  def toString(ind: Int): String = {
    val space: String = {
      val n =
        if (ind == 0 || ind == 1 || ind ==3 || ind == 4 || ind == 7 || ind == 8 || ind == 10 || ind == 11) (colWidth - toString.length) / 2.0
        else if (ind == 2 || ind == 5 || ind == 9 || ind == 12) colWidth - toString.length/2.0 - 1
        else if (ind != 14) 2*colWidth - toString.length/2.0
        else 2*colWidth - toString.length/2.0 - 40
      " " * (n max 0).toInt
    }

    space + toString + space
  }

  override def toString: String =
    if (score1.isDefined) p1.map(_.shortName()).get + s" ${score1.get}" + "-" + s"${score2.get} " + p2.map(_.shortName()).get
		else if (finished) {
			if (p1.isDefined) p1.map(_.shortName()).get
			else if (p2.isDefined) p2.map(_.shortName()).get
			else ""
		} else p1.map(_.shortName()).getOrElse("______") + " - " + p2.map(_.shortName()).getOrElse("______")

  def simpleName: Option[String] = {
    if (p1.isEmpty || p2.isEmpty) None
    else Some("`" + p1.map(_.shortName()).get + " - " + p2.map(_.shortName()).get + "`")
  }

  def toSave: Option[String] = {
    if (score1.isDefined && score2.isDefined) Some("g," + p1.get.id + "," + p2.get.id + "," + score1.get + "," + score2.get)
    else None
  }
}

/************************************************************************************/

case class Group(id: Int, players: Seq[Player]) extends Node {
  private val combs = players.combinations(2).toSeq
  private val games = combs.map { case (Seq(p1, p2)) => Game(Some(p1), Some(p2), score1 = Some(1)/*, score2 = {if (Random.nextDouble() > 0.5) Some(3) else Some(0)}*/, leftChild = Leaf, rightChild = Leaf) }

  def finished: Boolean = pendingGames.isEmpty
  def pendingGames: Seq[Game] = games.filterNot(_.finished)
  def hasBetween(p1: String, p2: String): Boolean = games.exists(_.isBetween(p1, p2))

  def setScore(result: Result): Boolean = {
//    if (result.p1Score == result.p2Score) return false

    val game = games.find(g => !g.finished && g.isBetween(result.p1, result.p2))
    if (game.nonEmpty) {
      val g = game.get
      if (g.p1.get.id == result.p1 && g.p2.get.id == result.p2) g.setScore(result.p1Score, result.p2Score)
      else g.setScore(result.p2Score, result.p1Score)
      true
    } else false
  }

  def deleteResult(result: Result): Boolean = {
    val game = games.find(g => g.finished && g.isBetween(result.p1, result.p2))
    if (game.nonEmpty) {
      val g = game.get
      g.deleteScore()
      true
    } else false
  }

  def getRanks: Seq[(Player, Int)] = {
    players
      .map(p => (p,
        games
          .flatMap(_.winner)
          .count(_ == p))) //number of wins
      .sortWith {
      case ((p1, pos1), (p2, pos2)) => pos1 > pos2 || ((pos1 == pos2) && games
        .find(_.isBetween(p1, p2))
				.getOrElse(Game(leftChild = Leaf, rightChild = Leaf))
        .winner
        .getOrElse(p2) == p1) //score between the 2 players
    }
  }

  def winner: Option[Player] =
    if (!finished) None
    else {
			val ranks = getRanks
			if (ranks.length > 2) getRanks.find(_._2 > 0).map(_._1)
			else getRanks.headOption.map(_._1)
		}

  def runnerUp: Option[Player] =
    if (!finished) None
    else {
			val ranks = getRanks
			if (ranks.length > 2) getRanks.drop(1).find(_._2 > 0).map(_._1)
			else getRanks.drop(1).headOption.map(_._1)
		}

  override def toString: String = {
    games.map{g =>
      g.toString
    }.mkString("\n")
  }

  def gamesTable: Table = new Table(games.map(g =>
    Seq(g.p1.map(_.shortName()).getOrElse("None") + " - " + g.p2.map(_.shortName()).getOrElse("None"),
      if(g.score1.isDefined && g.score2.isDefined) g.score1.get + "-" + g.score2.get else "")), Seq("Group "+ (id + 1), ""))

  def toSave: String = players.map(p => s"$id,${p.id}").mkString("\n")

  def gamesToSave: Option[String] = {
    val save = games.flatMap(_.toSave)
    if (save.nonEmpty) Some(save.mkString("\n"))
    else None
  }
}

/************************************************************************************/

object Leaf extends Node {
  override def winner: Option[Player] = None
  def toString(level: Int, ind: Int, isGroup: Boolean): String = ""
  def finished: Boolean = true
}