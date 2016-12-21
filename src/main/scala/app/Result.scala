package app

import tournament.tournament
import utils.{Row, Table, storage}

import scala.concurrent.duration._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by nikos on 12/11/2016.
  */
case class Result(p1: String, p2: String, p1Score: Int, p2Score:Int) {
  def confirm(confirmed: Boolean) = confirmed match {
    case c: Boolean if c => players.newResult(this)
    case c: Boolean if !c =>
  }

  def askForConfirmation() = {
    val message =
      if (p1Score > p2Score) s"${bot.fromId(p2).get}, did you lose from ${bot.fromId(p1).get} $p1Score-$p2Score (yes/no)?"
      else if (p1Score < p2Score) s"${bot.fromId(p2).get}, did you beat ${bot.fromId(p1).get} $p1Score-$p2Score (yes/no)?"
      else s"${bot.fromId(p2).get}, did you draw with ${bot.fromId(p1).get} $p1Score-$p2Score (yes/no)?"
    bot.sendMessage(p2, message)
  }

  override def toString: String = s"$p1,$p2,$p1Score,$p2Score"
}

case class NotInNotifiedException(message: String) extends Exception(message)
object confirmationQueue {

  private val notified = new mutable.HashMap[String, Result]
  private val pending = new mutable.HashMap[String, ArrayBuffer[Result]]

  def getPending: mutable.HashMap[String, ArrayBuffer[Result]] = pending synchronized {
    val all: mutable.HashMap[String, ArrayBuffer[Result]] = new mutable.HashMap[String, ArrayBuffer[Result]]
    pending.foreach{ case (k,v) => all += k -> (new ArrayBuffer[Result]() ++ v)}
    notified.foreach{ n => all(n._1).insert(0,n._2)
    }
    all
  }

  def clearPending() = pending synchronized {
    pending.clear()
  }

  def notify(p1: String, p2: String, table: Table) = {
    bot.sendMessage(p1, s"You both reported the same score! Putting it down.\nHere are your new stats:\n${table.toString(true)}")
    bot.sendMessage(p2, s"You both reported the same score! Putting it down.\nHere are your new stats:\n${table.toString(true)}")
  }

  def alreadyReported(result: Result): Boolean = pending synchronized {
    if (notified.contains(result.p1) &&
      notified(result.p1).p2 == result.p1 &&
      notified(result.p1).p2Score == result.p1Score &&
      notified(result.p1).p1Score == result.p2Score) {
      val res = notified.remove(result.p1).get
      val (p1, p2, table) = confirm(confirmed = true, "", Some(res))
      notify(p1, p2, table)
      drain(result.p1)
      true
    } else if (pending.contains(result.p1)){
      val array = pending(result.p1)
      val existing = array.find { r => r.p2 == result.p1 && r.p1Score == result.p2Score && r.p2Score == result.p1Score}
      if (existing.isDefined) {
        array.remove(array.indexOf(existing.get))
        val (p1, p2, table) = confirm(confirmed = true, "", Some(existing.get))
        notify(p1, p2, table)
        true
      } else false
    } else false
  }

  def newResult(result: Result, fromLoading: Boolean = false): Boolean = pending synchronized {

    if (alreadyReported(result)) return true

    val key = result.p2
    if (pending.get(key).isEmpty) pending += key -> ArrayBuffer(result)
    else pending(key) += result
    storage.savePending()
    drain(key, fromLoading)
    false
  }

  def confirm(confirmed: Boolean, player: String, res: Option[Result] = None): (String, String, Table) = pending synchronized {
    val result =
      if (res.isDefined) res
      else notified.get(player)

    if (result.isDefined) {

      val p1OldPos = players.getPosition(result.get.p1)
      val p2OldPos = players.getPosition(result.get.p2)
      result.get.confirm(confirmed)
      val p1NewPos = players.getPosition(result.get.p1)
      val p2NewPos = players.getPosition(result.get.p2)

      if (res.isEmpty) {
        notified.remove(player)
        drain(player)
      }

      def delta(n: Int, o: Int): String = {
        if (o-n > 0) s"+${o-n}"
        else if (n == o) "-"
        else s"${o-n}"
      }

      val p1 = players.players.filter(_.id == result.get.p1).map(p => delta(p1NewPos, p1OldPos) +: p.toSeq).head
      val p2 = players.players.filter(_.id == result.get.p2).map(p => delta(p2NewPos, p2OldPos) +: p.toSeq).head

      val rows = Seq(p1, p2).map(p => Row(p))

      val table = new Table(rows, Player.header)
      if (confirmed) {
        storage.save(result.get)
        if (tournament.newResult(result.get)) tournament.confirmResult(result.get.p1,result.get.p2)
      }
      storage.savePending()
      (result.get.p1, result.get.p2, table)
    } else throw NotInNotifiedException(s"No game awaits confirmation from $player.")
  }

  def drain(key: String, fromLoading: Boolean = false) = {
    implicit val ec = bot.ec
    bot.system.scheduler.scheduleOnce(1 second) {
      pending synchronized {
        if (notified.get(key).isEmpty) {
          if (pending.get(key).isDefined && pending.get(key).get.nonEmpty) {
            val result = pending(key).remove(0)
            if (!fromLoading) result.askForConfirmation()
            notified += result.p2 -> result
          }
        }
      }
    }
  }
}