package tournament

import app.{Player, Result, bot, players}
import utils.storage

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

/**
  * Created by nikos on 19/11/2016.
  */
object tournament {
  class Tournament() {
    var registering = true
    val players = new ArrayBuffer[Player]

    var tree: Option[Tree] = None

    def register(player: Player): String = {
      if (registering) {
        if (players.contains(player)) "Player already registered"
        else {
          players += player
          s"${player.shortName()} registered!"
        }
      } else "Registration is closed!"
    }

    def start() = {
      registering = false
      if (players.length > 1) {
        tree = Some(new Tree(players.sortBy(_.elo).reverse))
        storage.saveTree(tree.get)

				tournamentWatcher.tournamentStarted()
      } else tournament.stop()
    }

    override def toString: String =
      if (registering) "Currently registered:\n" + players.map("`" + _.shortName() + "`").sorted.mkString("\n")
      else if (tree.isEmpty) "No tree exists"
      else tree.get.toString
  }

  /***************************************************************************************************/

  val colWidth = 30
  val fakePlayers = List.tabulate(40)(i => Player("" + (i + 1), fake = true))
  private var current: Option[Tournament] = None

  def newTournament() = current = Some(new Tournament())
  def stop() = current = None
  def winner: Option[Player] = current.flatMap(_.tree).flatMap(_.winner)
  def tree: Tree = current.get.tree.get

  def start: Boolean = {
    if (current.isDefined) current.get.start()
    current.isDefined
  }

  def exists: Boolean = current.isDefined && current.get.tree.isDefined

  def nextGames: String = {
    if (!exists) return "No tournament running."
    winner
    "*Upcoming games*\n" + tree.getUpcoming().mkString("\n")
  }

  def register(id: String): String =
    if (current.isDefined) current.get.register(player(id))
    else "No tournament open for registration."

  def newResult(result: Result): Boolean = {
    if (!exists || current.get.registering) false
    else {
      val tree = current.get.tree.get
      if (tree.newResult(result)) true
      else false
    }
  }

  def deleteResult(result: Result): Boolean = {
    if (!exists || current.get.registering) false
    else {
      val tree = current.get.tree.get
      if (tree.deleteResult(result)) true
      else false
    }
  }

  def confirmResult(p1: String, p2: String, notify: Boolean = true): Unit = {
    if (!exists) return
    winner
		if (notify) {
			val upcoming1 = tree.getUpcoming(Some(p1))
			val upcoming2 = tree.getUpcoming(Some(p2))
			val common = "Thanks for this tournament result."
			val m1 =
				if (winner.isDefined && winner.get.id == p1) s"Congratulations ${winner.get.shortName()}, you won the tournament! :tada::tada::tada: :champagne::champagne::champagne:"
				else common + {if (upcoming1.nonEmpty) upcoming1.mkString("\n*:calendar:Your next games:*\n","\n", "") else "\nNo pending tournament games."}
			val m2 =
				if (winner.isDefined && winner.get.id == p2) s"Congratulations ${winner.get.shortName()}, you won the tournament! :tada::tada::tada: :champagne::champagne::champagne:"
				else common +{if (upcoming2.nonEmpty) upcoming2.mkString("\n*:calendar:Your next games:*\n","\n", "") else "\nNo pending tournament games."}
      bot.sendMessage(p1, m1, 1 second)
      bot.sendMessage(p2, m2, 1 second)
    }

    if (winner.isDefined) {
			bot.sendMessageChannel(text = tournament.toString)
			bot.sendMessageChannel(text = s"/giphy I believe I can fly")
    }
  }

  def load(lines: List[String]): Unit = {
    stop()
    newTournament()
    current.get.registering = false
    current.get.tree = Some(new Tree(lines.filterNot(_.startsWith("g")).map(_.split(',').last).distinct.map(player)))
    current.get.tree.get.fromLoad(lines)
  }

  override def toString: String = {
    if (current.isEmpty) return "No tournament exists."
    if (winner.isDefined) tree.toString + "\nThe winner:crown: is *" + winner.get.shortName() + "*. Congratulations! :clap:"
    else current.get.toString
  }

  //  def fakeTree: String = {
  //    val tree = new Tree(fakePlayers)
  //    tree.winner
  //    tree.toString
  //  }

  def player(id: String) = {
    //    if (fakePlayers.nonEmpty) fakePlayers.filter(_.id == id).head
    /*else */
    players.getOrCreate(id)
  }
}