package tournament

import java.sql.Date
import java.time.{LocalDate, LocalDateTime}
import java.time.temporal.ChronoUnit
import java.util.Calendar

import akka.actor.ActorSystem

import scala.concurrent.duration._
import app.{Result, bot}

import scala.concurrent.ExecutionContextExecutor

/**
	* Created by nikolaos.tsaousis on 14/04/2017.
	*/
object tournamentWatcher {

	implicit val system: ActorSystem = bot.getSystem
	implicit val ec: ExecutionContextExecutor = system.dispatcher

	val runAt = 11			//11 am
	val deadline = 7		//7 days

	def startWatching(): Unit = {
//		system.scheduler.schedule(30 seconds, 24 hours) {
		system.scheduler.schedule(at10am, 24 hours) { () =>
			if (tournament.exists) {
				finishGames()
				notifyDeadlines()
			}
		}
	}

	def at10am: FiniteDuration = {
		val now = LocalDateTime.now()
		val at10 = LocalDateTime.of(now.getYear, now.getMonth, now.getDayOfMonth, runAt, 0, 0).plus(1, ChronoUnit.DAYS)
		ChronoUnit.SECONDS.between(LocalDateTime.now(), at10) seconds
	}

	def tournamentStarted(): Unit = {
		notifyDeadlines()
	}

	def finishGames(): Unit = {
		if (!tournament.exists) return ()

		val tree = tournament.tree
		val games = tree.getUpcomingGames()
			.filter(_.created.isDefined)
			.map(g => (g, daysWithoutWeekends(LocalDate.now(), plusDays(g.created.get, 7))))
			.groupBy{ case(game, days) => days }
			.filter(_._1 <= 0)
			.flatMap { case(days, ops) => ops.map(_._1)}
		  .toSeq

		games.foreach { g =>
			val result = Result(g.p1.get.id, g.p2.get.id, 0, 0)
			if (tournament.newResult(result)) tournament.confirmResult(result.p1,result.p2, notify = false)
		}
		val players = games.flatMap(g => Seq(g.p1, g.p2)).flatten.toSet

		var interval = 0
		players.foreach { p =>
			val opps = games
				.filter(_.plays(p.id))
				.flatMap(_.opponent(p.id))
			  .map(_.shortName())

			val message =
				if (opps.length == 1) {
					s"You did not play with " + opps.head + ". Match is void."
				} else if (opps.length > 1) {
					s"You did not play with " + opps.init.mkString(", ") + " and " + opps.last + ". Matches are void."
				} else ""

			if (message != "") {
				bot.sendMessage(p.id, message, interval seconds)
				interval += 1
			}
		}
	}

	def notifyDeadlines(): Unit = {
		if (!tournament.exists) return ()

		val tree = tournament.tree
		val games = tree.getUpcomingGames()
			.filter(_.created.isDefined)
		val players = games.flatMap(g => Seq(g.p1, g.p2)).flatten.toSet

		var interval = 0
		players.foreach { p =>
			games
				.filter(_.plays(p.id))
				.map(g => (g.opponent(p.id), g.created))
				.filter(_._1.isDefined)
				.map(g => (g._1.get.shortName(), daysWithoutWeekends(LocalDate.now(), plusDays(g._2.get, 7))))
				.groupBy{ case(_, days) => days }
				.map { case(days, ops) => (days, ops.map(_._1))}
				.toSeq.sortBy(_._1)
				.foreach { case (days, opps) =>
					val message =
						if (opps.length == 1) s"You have *$days days* to play with " + opps.head + " for the tournament."
						else if (opps.length > 1) s"You have *$days days* to play with " + opps.init.mkString(", ") + " and " + opps.last + " for the tournament."
						else ""

					if (message != "") {
						bot.sendMessage(p.id, message, interval seconds)
						interval += 1
					}
				}
		}
	}

	def setDeadline(game: Game, days: Int) = game.updateDate(Some(LocalDate.now().plus(days - deadline, ChronoUnit.DAYS)))

	def extendDeadline(days: Int, player1: Option[String] = None, player2: Option[String] = None): Boolean = {

		if (!tournament.exists) return false

		val tree = tournament.tree
		tree.getUpcomingGames()
			.filter{g =>
				g.created.isDefined &&
					(player1.isEmpty || g.plays(player1.get)) &&
					(player2.isEmpty || g.plays(player2.get))
			}
		  .foreach(setDeadline(_, days))

		finishGames()

		true
	}

	def plusDays(start: LocalDate, days: Int): LocalDate = start.plus(days, ChronoUnit.DAYS)

	def daysWithoutWeekends(start: LocalDate, end: LocalDate): Long = {
		//Ignore argument check

		val c1 = Calendar.getInstance()
		c1.setTime(Date.valueOf(start))
		var w1 = c1.get(Calendar.DAY_OF_WEEK)
		c1.add(Calendar.DAY_OF_WEEK, -w1)

		val c2 = Calendar.getInstance()
		c2.setTime(Date.valueOf(end))
		var w2 = c2.get(Calendar.DAY_OF_WEEK)
		c2.add(Calendar.DAY_OF_WEEK, -w2)

		//end Saturday to start Saturday
		val days = (c2.getTimeInMillis-c1.getTimeInMillis)/(1000*60*60*24)
		val daysWithoutWeekendDays = days-(days*2/7)

		// Adjust days to add on (w2) and days to subtract (w1) so that Saturday
		// and Sunday are not included
		if (w1 == Calendar.SUNDAY && w2 != Calendar.SATURDAY) {
			w1 = Calendar.MONDAY
		} else if (w1 == Calendar.SATURDAY && w2 != Calendar.SUNDAY) {
			w1 = Calendar.FRIDAY
		}

		if (w2 == Calendar.SUNDAY) {
			w2 = Calendar.MONDAY
		} else if (w2 == Calendar.SATURDAY) {
			w2 = Calendar.FRIDAY
		}

		val ans = daysWithoutWeekendDays-w1+w2
		if (ans > 0) ans
		else deadline + ans
	}
}
