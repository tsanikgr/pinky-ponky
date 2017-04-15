package tournament

import app.{Player, Result, bot}
import utils.{Row, Table, storage}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by nikos on 20/11/2016.
  */
class Tree(players: Seq[Player]) {

  val colWidth = tournament.colWidth
  val order = Vector(0, 7, 3, 4, 2, 5, 1, 6)
  var groups = createGroups(players)
  var root = getNodes

  def newResult(result: Result): Boolean = {
    val ret = groups.exists(g => g.setScore(result)) || preorderTraversal.exists(_.setScore(result))
    if (ret) storage.saveTree(this)
    ret
  }

  def deleteResult(result: Result): Boolean = {
    val ret = preorderTraversal.exists(_.deleteResult(result)) || groups.exists(g => g.deleteResult(result))
    if (ret) storage.saveTree(this)
    ret
  }

  def winner: Option[Player] = {
    backfill()
    root.winner
  }

  def backfill(notify: Boolean = true): Unit = {
    def backfill(root: Node): Unit = root match {
      case Leaf =>
      case Group(_, _) =>
      case r@Game(_, _, _, _, leftChild, rightChild) =>
        backfill(leftChild)
        backfill(rightChild)
        propagate(r, leftChild, rightChild, notify)
    }
    backfill(root)
  }

  def propagate(root: Game, leftChild: Node, rightChild: Node, notify: Boolean): Unit = {
    val changed = root.p1.isEmpty || root.p2.isEmpty
    leftChild match {
      case Leaf =>
      case g: Game => root.p1 = g.winner
      case g@Group(id, _) =>
        if (id % 2 == 0) {
          root.p1 = g.winner
          root.p2 = groups(id + 1) match {
            case g: Group => g.runnerUp
            case _ => None
          }
        } else {
          root.p1 = groups(id - 1) match {
            case g: Group => g.runnerUp
            case _ => None
          }
          root.p2 = g.winner
        }
    }
    rightChild match {
      case Leaf =>
      case g: Group =>
      case g: Game => root.p2 = g.winner
    }

    if (notify && changed && root.p1.isDefined && root.p2.isDefined){
      bot.sendMessage(root.p2.get.id, s":calendar:Your next tournament match is with... *${root.p1.get.shortName()}*")
      bot.sendMessage(root.p1.get.id, s":calendar:Your next tournament match is with... *${root.p2.get.shortName()}*")
    }

    if (changed && root.p1.isDefined && root.p2.isDefined) root.updateDate()
  }

  def getNodes: Node = {
    def childInd(parentInd: Int, right: Boolean): Int = parentInd * 2 + { if (right) 1 else 0 }
    def createNode(depth: Int, ind: Int): Node =
      if (depth == 3) Game(leftChild = groups(ind), rightChild = groups(if (ind % 2 == 0) ind+1 else ind - 1))
      else Game(leftChild = createNode(depth + 1, childInd(ind, right = false)), rightChild = createNode(depth + 1, childInd(ind, right = true)))

    createNode(0, 0)
  }

  def createGroups(players: Seq[Player]): Vector[Group] = {
    val randomised = Random.shuffle(new ArrayBuffer[Player]() ++ players.drop(8))
    val groups =
      Vector.fill(8)(new ArrayBuffer[Player]())
        .zipWithIndex
        .map { case (g, i) => addFirstPlayer(players, g, i) }

    var ind = 0
    while (randomised.nonEmpty) {
      groups(order.reverse(ind)) += randomised.remove(0)
      ind += 1
      if (ind == 8) ind = 0
    }
    groups.zipWithIndex.map { case (ps, i) => Group(i, ps) }
  }

  def addFirstPlayer(players: Seq[Player], group: ArrayBuffer[Player], ind: Int): ArrayBuffer[Player] = {
    if (order(ind) >= players.length) group
    else group += players(order(ind))
  }

  def preorderTraversal: Seq[Game] = {
    val acc = new ArrayBuffer[Game]()
    def preorder(root: Node): Unit = root match {
      case g: Game =>
        preorder(g.leftChild)
        preorder(g.rightChild)
        acc += g
      case _ =>
    }
    preorder(root)
    acc
  }

  def repeat(n: Int, c: String): String = List.fill(n)(c).mkString("")

  override def toString: String = root match {
    case g: Game =>
      val (top, bottom) = ranks
      val games: Seq[String] = preorderTraversal.zipWithIndex.map{case(n, i) => n.toString(i)}
      val four = s"${repeat(colWidth/2-1," ")}+${repeat(colWidth/2-2,"-")}+${repeat(colWidth/2-1,"-")}+${repeat(colWidth/2-2," ")}"
      val lines =
        s"""
           |
             |${games.head + games(1) + games(3) + games(4)}
           |${repeat(colWidth/2-1," ")}|${repeat(colWidth-2," ")}|${repeat(colWidth-2," ")}|${repeat(colWidth-2," ")}|
             |$four $four
           |${repeat(colWidth-2," ")}|${repeat(colWidth*2-3," ")}|
             |${games(2)} ${games(5)}
           |${repeat(colWidth-2, " ")}|${repeat(2*colWidth-3, " ")}|
             |${repeat(colWidth-2, " ")}+${repeat(colWidth, "-")}+${repeat(colWidth-4, "-")}+
           |${repeat(2*colWidth-1, " ")}|
             |${games(6)}
           |${repeat(2*colWidth-1, " ")}|
             |${repeat(2*colWidth-1, " ")}|
             |                        --------->      ${games(14)}   <--------- FINAL
           |${repeat(2*colWidth-1, " ")}|
             |${repeat(2*colWidth-1, " ")}|
             |${games(13)}
           |${repeat(2*colWidth-1, " ")}|
             |${repeat(colWidth-2, " ")}+${repeat(colWidth, "-")}+${repeat(colWidth-4, "-")}+
           |${repeat(colWidth-2," ")}|${repeat(colWidth*2-3," ")}|
             |${games(9)} ${games(12)}
           |${repeat(colWidth-2," ")}|${repeat(colWidth*2-3," ")}|
             |$four $four
           |${repeat(colWidth/2-1," ")}|${repeat(colWidth-2," ")}|${repeat(colWidth-2," ")}|${repeat(colWidth-2," ")}|
             |${games(7) + games(8) + games(10) + games(11)}
           |
             |""".stripMargin
      "```" + top +
        lines +
        bottom + "```"
    case _ => ""
  }

  def ranks: (String, String) = {
    val gs = groups.zipWithIndex
      .map {
        case ((g: Group, i: Int)) =>
          (i,
            if (g.finished) new Table(g.getRanks.map { case ((p, w)) => Row(Seq(p.shortName(), w))}, Seq("Group " + (i + 1), ""))
            else g.gamesTable)
        case (_, i) => (i, new Table(Seq(Row(Seq()))))
      }
      .map { case (i, g) => g.toString(printHeader = true, noQuotes = true) }
      .map(_.split('\n'))

    def getGroup(gs: Vector[Array[String]], id: Int, line: Int): String =
      if (gs(id).length <= line) " " * gs(id).head.length
      else gs(id)(line)

    def space(id: Int, left: Boolean = true): String = " " * {
      if (((id < 2 || (id > 3 && id < 6)) && left) ||
        ((id > 5 || (id > 1 && id < 4)) && !left)) math.ceil((colWidth - gs(id).head.length)/2.0) max 0
      else math.floor((colWidth - gs(id).head.length)/2.0) max 0}.toInt

    val lines = gs.foldLeft(0) { case (n, a) => n max a.length }

    val top = for (l <- 0 until lines) yield (0 until 4).map(i => space(i) + getGroup(gs, i, l) + space(i, left = false)).mkString("")
    val bottom = for (l <- 0 until lines) yield (4 until 8).map(i => space(i) + getGroup(gs, i, l) + space(i, left = false)).mkString("")

    (top.mkString("\n"), bottom.mkString("\n"))
  }

  def getUpcoming(forPlayer: Option[String] = None): Seq[String] = {
    def filter: (Game => Boolean) =
      if (forPlayer.isEmpty) (_) => true
      else (game) => game.plays(forPlayer.get)

    (groups.flatMap(g => g.pendingGames) ++ preorderTraversal.filterNot(_.finished))
      .filter(filter)
      .flatMap(_.simpleName)
  }

	def getUpcomingGames(forPlayer: Option[String] = None): Seq[Game] = {
		def filter: (Game => Boolean) =
			if (forPlayer.isEmpty) (_) => true
			else (game) => game.plays(forPlayer.get)

		(groups.flatMap(g => g.pendingGames) ++ preorderTraversal.filterNot(_.finished))
			.filter(filter)
	}

  def toSave: String =
    (groups.map{ g => g.toSave}.mkString("","\n","\n") +
      groups.flatMap(_.gamesToSave).mkString("","\n","\n") +
      preorderTraversal.flatMap(_.toSave).mkString("\n"))
      .split('\n').filter(_.nonEmpty).mkString("\n")

  def fromLoad(lines: List[String]): Unit = {
    val players =
      lines
        .filterNot(_.startsWith("g"))
        .map(l => (l.split(',').head.toInt, tournament.player(l.split(',').last)))

    groups = Vector
      .fill(8)(new ArrayBuffer[Player])
      .zipWithIndex
      .map{ case(buf, i) => buf ++= players.filter(_._1 == i).map(_._2) ; Group(i, buf)}

    root = getNodes

    backfill(false)

    lines
      .filter(_.startsWith("g,"))
      .map(l => l.substring(2).split(','))
      .map(vs => Result(vs(0),vs(1),vs(2).toInt,vs(3).toInt))
      .foreach { r =>
        newResult(r)
        backfill(false)
      }
  }
}