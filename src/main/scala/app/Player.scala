package app

import utils.{Row, Table, storage}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
	* Created by nikos on 12/11/2016.
	*/
case class Player(id: String,
									name: String,
									var elo: Int,
									var gamesWon: Int,
									var gamesLost: Int,
									var gamesDrawn: Int,
									var winStreak: Int,
									var setsWon: Int,
									var setsLost: Int) {

	private var _shortName = players.formatName(name)
	def shortName(): String = _shortName
	def shortName(name: String) = _shortName = name

	def totalGames = gamesWon + gamesLost + gamesDrawn
	def totalSets = setsWon + setsLost

	def sortCol(col: String): Int = col.toLowerCase match {
		case "elo" => elo
		case "games" => gamesWon + gamesLost
		case "win%" => (gamesWon.toDouble / (gamesWon + gamesLost + gamesDrawn) * 100000.0).toInt
		case "win %" => (gamesWon.toDouble / (gamesWon + gamesLost + gamesDrawn) * 100000.0).toInt
		case "streak" => winStreak
		case "sets" => setsWon + setsLost
		case _ => elo
	}

	def getDeltaElo(res1: Int, res2: Int, elo1: Int, elo2: Int): Int = {
		val q1 = math.pow(10.0, elo1/400.0)
		val q2 = math.pow(10.0, elo2/400.0)
		val exp1 = q1/ (q1 + q2)

		val totalGames = res1 + res2
		val multFactor = 25 + 25/totalGames

		math.round(multFactor * (totalGames*exp1 - res1)).toInt
	}

	def updateScore(result1: Int, result2: Int, otherElo: Int): Int = {
		elo -= getDeltaElo(result1, result2, elo, otherElo)

		if (result1 > result2) {
			gamesWon += 1
			if (winStreak < 0) winStreak = 1
			else winStreak += 1
		} else if (result1 < result2) {
			gamesLost += 1
			if (winStreak > 0) winStreak = -1
			else winStreak -= 1
		} else {
			gamesDrawn += 1
			winStreak = 0
		}
		setsWon += result1
		setsLost += result2
		elo
	}

	def percentage(n: Double): String = {
		if (n.isNaN) ""
		else s"${n.toInt}%"
	}

	def toSeq: Seq[String] = Seq(
		shortName(),
		elo.toString,
		s"${gamesWon + gamesLost}",
		percentage(gamesWon.toDouble / (gamesWon + gamesLost + gamesDrawn)*100.0),
		winStreak.toString,
		s"${setsWon + setsLost} (${percentage(setsWon.toDouble/(setsWon + setsLost)*100.0)})")
}

object Player{
	//TODO: nemesis
	val header = Seq("", "name", "elo", "games", "win %", "streak", "sets")

	def apply(id: String): Player = new Player(id, bot.fromId(id).get,1500,0,0,0,0,0,0)
	def apply(id: String, fake: Boolean) = new Player(id,id,1500,0,0,0,0,0,0)
	def isReverse(col: String): Boolean = col.toLowerCase match {
		case "elo" => true
		case "wins" => true
		case "losses" => true
		case "draws" => true
		case "streak" => true
		case "sets won" => true
		case "sets lost" => true
		case _ => true
	}
}

object players {

	val players = new ArrayBuffer[Player]

	def apply(id: String): Player = players synchronized {
		getOrCreate(id)
	}

	def getOrCreate(playerId: String): Player = players synchronized {
		val p = players.find(_.id == playerId)
		if (p.isEmpty) {
			val newPlayer = Player(playerId)
			players += newPlayer
			updateShortNames()
			newPlayer
		} else p.get
	}

	def updateShortNames(): Unit = players synchronized {

		def hasDuplicates: Boolean = {
			val names = players.map(_.shortName())
			names.length != names.distinct.length
		}

		def updateRecursively(): Unit = {
			players
				.groupBy(_.shortName())
				.filter(_._2.length > 1)
				.flatMap(_._2)
				.toSet
				.foreach{ p: Player =>
					val chars = if (p.shortName().split(' ').length == 1) 1 else p.shortName().split(' ').last.length + 1
					p.shortName(formatName(p.name) + " " +
						p.name.split('.').tail.map(n => Table.capitalise(n.substring(0, (chars min n.length) max 0))).mkString(" "))
				}
			if (hasDuplicates) updateRecursively()
		}

		players.foreach(p => p.shortName(formatName(p.name)))
		if (hasDuplicates) updateRecursively()
	}

	def reload(): Unit = players synchronized {
		players.clear()
		storage.read()
	}

	def newResult(result: Result): Unit = players synchronized {
		val p1 = getOrCreate(result.p1)
		val p2 = getOrCreate(result.p2)

		val p1Temp = p1.elo
		p1.updateScore(result.p1Score, result.p2Score, p2.elo)
		p2.updateScore(result.p2Score, result.p1Score, p1Temp)
	}

	def getPosition(pId: String, sortBy: Seq[String] = Seq("elo")): Int = players synchronized {
		val p = getOrCreate(pId)
		val name = p.shortName()
		leaderboard(sortBy).rows.filter(r => r.values(1).toString.toLowerCase == name.toLowerCase).head.values.head.toString.toInt
	}

	def leaderboard(sortBy: Seq[String] = Seq("elo")): Table = players synchronized {
		val isReverse = Player.isReverse(sortBy.head)

		val header = Player.header.tail
		val sortInd = header.indexWhere(h => Table.toString(h).toLowerCase == sortBy.head.toLowerCase)
		val table = new Table(players.map(p => Row(p.toSeq)), header)
		val sorted =
			if (isReverse) table.sortBy(sortBy:_*).reverse
			else table.sortBy(sortBy:_*)

		var step = 0
		var last = 1
		val rows = sorted.rows.zipWithIndex.map{ case (r, i) =>
			if (i != 0 && sorted(i).values.drop(sortInd).headOption != sorted(i-1).values.drop(sortInd).headOption) {
				last += step
				step = 1
			} else {
				step += 1
			}
			Row(last +: r.values)
		}

		new Table(rows, Player.header)
	}

	//  def leaderboard(sortBy: String = "elo", players: Seq[Player] = this.players): Table = players synchronized {
	//    val isReverse = Player.isReverse(sortBy)
	//    val scores =
	//      if (isReverse) players.map(_.sortCol(sortBy)).sorted.reverse
	//      else players.map(_.sortCol(sortBy)).sorted
	//
	//    var step = 0
	//    var last = 1
	//    val positions = scores.zipWithIndex.map{ case (r, i) =>
	//      if (i != 0 && scores(i) != scores(i-1)) {
	//        last += step
	//        step = 1
	//      } else {
	//        step += 1
	//      }
	//      last
	//    }
	//    val rows =
	//      if (isReverse) players.sortBy(_.sortCol(sortBy)).reverse
	//      else players.sortBy(_.sortCol(sortBy))
	//
	//    val rowsWithPos = rows.zip(positions).map{ case (player,pos) => Row(pos +: player.toSeq)}
	//    new Table(rowsWithPos, Player.header)
	//  }

	def formatName(name: String): String = {
		val n = Table.capitalise(name.split('.').head)
		if (n == "Konstantinos") "Kostas"
		else if (n == "Nikolaos") "Nikos"
		else if (n == "Lukeo") "Luke"
		else n
	}

	def playerStats(id: String): String = {
		def getOpponent(id: String, result: Result): String = if (result.p1 == id) result.p2 else result.p1


		val player = players.find(_.id == id)
		if (player.isEmpty) s"Player ${bot.fromId(id).get} has not played any games yet. :rooster:\nType `challenge @${bot.fromId(id).get}` and I'll let them know!"
		else {
			val results = storage.getResults
			val opponents = results.filter(r => r.p1 == id || r.p2 == id).map(r => getOpponent(id, r)).distinct
			val ratings = opponents.map(o => players.find(_.id == o).get.elo)
			val games = opponents.map(o => results.count(r => (r.p1 == id && r.p2 == o) || (r.p2 == id && r.p1 == o)))
			val win = opponents.map(o => results.count{r =>
				((r.p1 == id && r.p2 == o) && (r.p1Score > r.p2Score)) ||
					((r.p2 == id && r.p1 == o) && (r.p2Score > r.p1Score))
			})
			val sets = opponents.map(o => results.filter(r => (r.p1 == id && r.p2 == o) || (r.p2 == id && r.p1 == o)).map(r => r.p1Score + r.p2Score).sum)
			val setsWon = opponents.map(o => results
				.filter(r => (r.p1 == id && r.p2 == o) || (r.p2 == id && r.p1 == o))
				.map(r => if (r.p1 == id) r.p1Score else r.p2Score).sum)
			val rows = for (i <- opponents.indices) yield {
				Row(Seq(players.filter(_.id == opponents(i)).head.shortName(), ratings(i), games(i), f"${win(i).toDouble/games(i) * 100.0}%.0f%%", f"${sets(i)} (${setsWon(i).toDouble/sets(i) * 100.0}%.0f%%)"))
			}
			val averageOpponentRating = {
				val weighted = setsWon.zip(sets).zip(opponents)
					.map{ case (((won,total),opponent)) =>
						(total * players.find(_.id == opponent).get.elo, total)
					}.reduce[(Int, Int)]{case((l,r), (l2,r2)) => (l + l2, r + r2)}

				weighted._1.toDouble / weighted._2.toDouble
			}
			val header = Seq("Opponent", "Rating", "Games", "Win %", "Sets (win %)")
			val pInfo: Seq[Any] = player.get.toSeq
			s"${new Table(Seq(Row(getPosition(id) +: pInfo)),Player.header).toString(true)}\n" +
				s"${new Table(rows, header).sortBy("Games").reverse.toString(true)}\n" +
				f"Average opponent rating: *$averageOpponentRating%.0f*\n" +
				s"Nemesis: *${opponents.zip(win).zip(games).sortBy{case ((_,w),g) => w.toDouble/g}.map(_._1._1).take(1).map(id => apply(id).shortName()).head}*"
		}
	}
}