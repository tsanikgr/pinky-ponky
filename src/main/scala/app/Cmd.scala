package app

import slack.models.Message
import tournament.tournament
import utils.storage

import scala.util.{Failure, Success, Try}

/**
  * Created by nikos on 12/11/2016.
  */

object Cmd {
  val commands: List[(String, String)] = List(
    ("vs @<opponent> 2-1", "Report a score after a match."),
    ("stats", "See the leaderboard"),
    ("top10", "The best"),
    ("stats by <column to order>", "Sort the leaderboard by 'elo', 'games', 'win%', 'streak' or 'sets'"),
    ("stats me", "Show your personal stats against others"),
    ("stats @<player-name>", "See the personal stats of someone else"),
    ("challenge @<opponent>", "Challenge an opponent to a game of :table_tennis_paddle_and_ball:"),
    ("pending", "See who has not yet confirmed a score"),
    ("tournament", "See the progress of a tournament"),
    ("register", "Register to a tournament"),
    ("next", "See the upcoming matches"),
    ("help", "See this message"))

  def usage =
    s"""The available commands are:\n${commands.map{case(c, e) => s"- `$c`: $e"}.mkString("\n")}
       |
       |- To prevent spam, prefer to talk to me in a *private room* :robot_face::wink:
       |
       |- Sometimes though, you need to *brag* - do it here! :crown:
       |
       |- I will always ask the other player to verify the score.
       |
       |- Keep an eye in this channel if you want to play in *tournaments*!
     """.stripMargin

  def apply(message: Message): Cmd = {
    //    val args = message.text.split(" ").filterNot(_.matches("^<@.*"))

    message.text.trim.toLowerCase match {
      case c: String if c.startsWith("vs") => NewResultCmd(message)
      case c: String if c.startsWith("stats") => StatsCmd(message)
      case c: String if c.startsWith("stats by") => StatsCmd(message)
      case c: String if c == "y" || c == "yes" => ConfirmCmd(message)
      case c: String if c == "n" || c == "no" => ConfirmCmd(message)
      case c: String if c == "help" => ReplyCmd(Some(usage), message)
      case c: String if c == "pending" => PendingCmd(message)
      case c: String if c == "register" => RegisterCmd(message)
      case c: String if c == "next" => NextGames(message)
      case c: String if c == "tournament" => PrintTournament(message)
      case c: String if c == "top10" => Top10Cmd(message)
      case c: String if c.startsWith("challenge") => ChallengeCmd(message)

      case c: String if c == "exit" && bot.fromId(message.user).getOrElse("unknown") == bot.admin => ExitCmd(message)
      case c: String if c == "reload" && bot.fromId(message.user).getOrElse("unknown") == bot.admin => ReloadCmd(message)

      case c: String if c == "new tournament" && bot.fromId(message.user).getOrElse("unknown") == bot.admin => NewTournament(message)
      case c: String if c == "start" && bot.fromId(message.user).getOrElse("unknown") == bot.admin => StartTournament(message)
      case c: String if c == "stop" && bot.fromId(message.user).getOrElse("unknown") == bot.admin => StopTournament(message)

      case c: String if c.startsWith("result") && bot.fromId(message.user).getOrElse("unknown") == bot.admin => NewResult(message)
      case c: String if c.startsWith("register") && bot.fromId(message.user).getOrElse("unknown") == bot.admin => RegisterCmd(message)
      case c: String if c.startsWith("id") && bot.fromId(message.user).getOrElse("unknown") == bot.admin => IdCmd(message)
      case c: String if c.startsWith("deletepending") && bot.fromId(message.user).getOrElse("unknown") == bot.admin => DeletePending(message)
      case c: String if c.startsWith("delete") && bot.fromId(message.user).getOrElse("unknown") == bot.admin => DeleteTournamentResult(message)

      case _ => NoReplyCmd(message)
    }
  }
}

/********************************************************************************/

abstract sealed class Cmd(message: Message) {
  def getMessage: Message = message
  val needsSave = false
  val user: String = message.user
  val channel: String = message.channel
  val args: List[String] = message.text.split(" ")/*.filterNot(_.matches("^<@.*"))*/.map(_.replaceAll("[<>@]", "")).toList
  def execute: Try[String]
  override def toString: String = s"`${args.mkString(" ")}`"
}

case class EmptyReply() extends Exception("Empty reply")
case class NoReplyCmd(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Failure(EmptyReply())
}

/********************************************************************************/

case class ReplyCmd(reply: Option[String], message: Message) extends Cmd(message) {
  def execute: Try[String] = reply match {
    case Some(r) => Success(r)
    case None => Failure(new Exception("Empty reply"))
  }
}

/********************************************************************************/

case class ExitCmd(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try{
    bot.sendMessage(bot.nameToId(bot.admin).get,"Saving pending & going offline!")
    storage.savePending()

    System.exit(0)
    ""
  }
}

/********************************************************************************/

case class NewResultCmd(message: Message) extends Cmd(message) {
  def execute: Try[String] = Try{
    val p1 = message.user
    val p2 = args(1)

    if (p1 == p2) "You thought you would trick me eh?! Mouhaha"
    else {
      val (score1, score2) =
        if (args.length == 3) (args(2).split('-').head.toInt, args(2).split('-').last.toInt)
        else (args(2).toInt, args(4).toInt)

      if (confirmationQueue.newResult(Result(p1, p2, score1, score2))) throw EmptyReply()
      else s"Asking ${bot.fromId(p2).get} to verify the score. Hang tight!"
    }
  }
}

/********************************************************************************/

case class StatsCmd(message: Message) extends Cmd(message) {
  def execute: Try[String] = Try{
    if (args.length < 2) s"""${players.leaderboard().toString(true)}"""
    else if (args(1).toLowerCase == "by") s"""${players.leaderboard(Seq(args.drop(2).mkString(" ").toLowerCase)).toString(true)}"""
    else if (args(1).toLowerCase == "me") s"""${players.playerStats(user)}"""
    else s"""${players.playerStats(args(1))}"""
  }
}

/********************************************************************************/

case class ConfirmCmd(message: Message) extends Cmd(message) {

  def execute: Try[String] = Try{
    message.text.toLowerCase match {
      case s: String if s == "y" || s == "yes" =>
        val (p1, p2, table) = confirmationQueue.confirm(confirmed = true, user)
        bot.sendMessage(p1, s"${bot.fromId(p2).get} confirmed your score. I'll pencil it down.\nHere are your new stats:\n${table.toString(true)}")
        s"Ok, I'll pencil it down.\nHere are your new stats:\n${table.toString(true)}"

      case s: String if s == "n" || s == "no" =>
        val (p1, p2, table) = confirmationQueue.confirm(confirmed = false, user)
        bot.sendMessage(p1, s"${bot.fromId(user).get} *did not* confirm your score. Scratching it.")
        "Ok, I'll scratch it. Maybe report the right score?"
    }
  }
}

/********************************************************************************/

case class PendingCmd(message: Message) extends Cmd(message) {

  def execute: Try[String] = Try {
    val pending = confirmationQueue.getPending.filter(_._2.nonEmpty)
    if (pending.isEmpty) "No results pending! :wheeeey:"
    else pending.map { r =>
      s"`${players(r._1).shortName()}`:\n${r._2.map(res => s"${players(res.p1).shortName()} vs ${players(res.p2).shortName()} ${res.p1Score}-${res.p2Score}").mkString("\n")}"
    }.mkString("\n")
  }
}

/********************************************************************************/

case class ReloadCmd(message: Message) extends Cmd(message) {
  def execute: Try[String] = Try {
    players.reload()
    "Reloading successful"
  }
}

/********************************************************************************/

case class ChallengeCmd(message: Message) extends Cmd(message) {
  def execute: Try[String] = Try {
    bot.sendMessage(args(1), s"${bot.fromId(user).get} challenged you to a game of :table_tennis_paddle_and_ball:.")
    "Ok, I 'll let him know!"
  }
}

/********************************************************************************/

case class NewTournament(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try{
    tournament.newTournament()
    import scala.concurrent.duration._

    val pls = players.players.zipWithIndex.foreach{ case(p,i) => bot.sendMessage(p.id,
      ":trophy:The tournament is now open for registration! Type `register` if you want to play.:trophy:\nRegistration closes at 5pm!", i*500.0 millis)}
    //    players.players.foreach(p => RegisterCmd(Message(message.ts, channel, user, s"register ${p.id}", None)).execute)
    "Created new tournament"
  }
}

/********************************************************************************/

case class StartTournament(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    if (tournament.start) {
      bot.sendMessageChannel(bot.getChannel, tournament.toString + "\nTournament started!\n" + tournament.nextGames)
      "Tournament started"
    }
    else "Tournament cannot start"
  }
}

/********************************************************************************/

case class StopTournament(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    tournament.stop()
    "Tournament stopped"
  }
}

/********************************************************************************/

case class NextGames(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    tournament.nextGames
  }
}

/********************************************************************************/

case class PrintTournament(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    tournament.toString
  }
}

/********************************************************************************/

case class NewResult(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    val result = Result(args(1), args(2), args(3).toInt, args(4).toInt)
    //    tournament.newResult(result)
    confirmationQueue.confirm(confirmed = true, "", Some(result))
    "Made note of result"
  }
}

/********************************************************************************/

case class DeleteTournamentResult(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    val result = Result(args(1), args(2), 0, 0)
    if (tournament.deleteResult(result)) "Result was deleted."
    else "No such result in tournament."
  }
}

/********************************************************************************/

case class RegisterCmd(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    if (args.length == 1) tournament.register(user)
    else tournament.register(args(1))
  }
}

/********************************************************************************/

case class IdCmd(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    bot.nameToId(bot.fromId(args(1)).get).get
  }
}

/********************************************************************************/

case class DeletePending(message: Message) extends Cmd(message) {
  override def execute: Try[String] = Try {
    val (p1,p2,table) = confirmationQueue.confirm(confirmed = false, args(1))
    "Deleted pending result between " + bot.fromId(p1).get + " and " + bot.fromId(p2).get
  }
}

/********************************************************************************/

case class Top10Cmd(message: Message) extends Cmd(message) {
  def execute: Try[String] = Try{
    s"""${players.leaderboard().toString(true, rowsToShow = 10)}"""
  }
}