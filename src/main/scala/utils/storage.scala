package utils

import java.io.{BufferedWriter, File, FileWriter}

import app.{Result, confirmationQueue, players}
import tournament.{Tree, tournament}

object storage {

  val CREDENTIALS_PATH = "src/main/resources/credentials.txt"
  val GAMES_PATH = "src/main/resources/games.txt"
  val PENDING_CONFS_PATH = "src/main/resources/pending.txt"
  val TOURNAMENT_PATH = "src/main/resources/tournament.txt"

	/** credentials.txt is a text file where:
		* 1st line: Slack token
		* 2nd line: Slack name of administrator
		* 3rd line: Ping-pong slack channel name
		*
		* @return (slack_token, administrator, ping_pong_channel_name)
		*/
  def getCredentials: (String, String, String) = {
		try {
			val lines = scala.io.Source.fromFile(CREDENTIALS_PATH).getLines().toList
			(lines.head, lines(1), lines.last)
		}catch {
			case ex: Exception =>
				throw new Exception(
					"""Cannot parse credentials.txt.
						|
						|Create the file src/main/resources/credentials.txt and put in it:
						|			1st line: slack token
						|			2nd line: administrator name
						|			3rd line: channel-name
						|
						|eg.
						|			aaaa-543265645634-aa4254fsdg34tgsg4t32gfdg
						|			the.boss
						|			games-channel
						|   """.stripMargin, ex)
		}
  }

  def read(): Unit = {
    getResults.foreach { r => players.newResult(r)}
    loadPending()
    loadTree()
  }

  def getResults: List[Result] = {
    val lines = scala.io.Source.fromFile(GAMES_PATH).getLines().toList
    lines.map(_.split(',')).map(r => Result(r(0), r(1), r(2).toInt, r(3).toInt))
  }

  def save(result: Result): Unit = appendToFile(new File(GAMES_PATH)){ p => p.println(result.toString)}

  def savePending(): Unit = {
    printToFile(new File(PENDING_CONFS_PATH)) { p =>
      confirmationQueue.getPending.foreach{ pending =>
        pending._2.foreach{ r =>
          p.println(r.toString)
        }
      }
    }
  }

  def loadTree(): Unit = {
    if (!new File(TOURNAMENT_PATH).exists()) return

    val lines = scala.io.Source.fromFile(TOURNAMENT_PATH).getLines().toList.filter(l => l.nonEmpty)
    if (lines.nonEmpty) tournament.load(lines)
  }

  def saveTree(tree: Tree) = {
    printToFile(new File(TOURNAMENT_PATH)) { p =>
      p.println(tree.toSave)
    }
  }

  def loadPending(): Unit = {
    val lines = scala.io.Source.fromFile(PENDING_CONFS_PATH).getLines().toList
    if (lines.nonEmpty) {
      //      bot.sendMessageChannel(bot.getChannel, "There were unconfirmed games, but I had to restart! I will re-send confirmation messages now! Sorry about that!")
      lines.map(_.split(',')).foreach{ r => confirmationQueue.newResult(Result(r(0), r(1), r(2).toInt, r(3).toInt), fromLoading = true)}
    }
  }

  private def appendToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new BufferedWriter(new FileWriter(f, true)), false)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  private def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new BufferedWriter(new FileWriter(f)), false)
    try {
      op(p)
    } finally {
      p.close()
    }
  }
}