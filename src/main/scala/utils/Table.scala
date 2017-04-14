package utils

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/************************************************************/

class Table(val rows: Seq[Row], val header: Option[Row]) {

  def this(rows: Seq[Row], header: Seq[Any]) = this(rows, Some(Row(header)))

  def isEqual(other: Table): Boolean = {
    rows.length == other.rows.length &&
      rows.zip(other.rows).forall{ case(thisRow, otherRow) =>
        thisRow.values.length == otherRow.values.length &&
          thisRow.values.zip(otherRow.values).forall{case (v1, v2) => Table.toString(v1) == Table.toString(v2)}}
  }

  def this(rows: Seq[Row]) = this(rows, None)

  def columnWidths(rows: Seq[Row], header: Option[Row]):ArrayBuffer[Int] = {
    val buf = new ArrayBuffer[Int]

    val percentageCols = getPercentageCols(header).toSet
    for (row <- rows) {
      while (buf.length < row.values.length) buf += 0
      row
        .values
        .indices
        .foreach(i => buf(i) = buf(i) max Table.length(row.values(i), percentageCols.contains(i)))
    }
    if (header.isDefined) for (i <- header.get.values.indices) {
      while (buf.length < header.get.values.length) buf += 0
      buf(i) = buf(i) max Table.length(header.get.valuesTrunc(i))
    }
    buf.map(_ + 2)
  }

  def hLine(columnWidths: Seq[Int]): String = {
    def col(width: Int): String = List.fill(width)("-").mkString("+","","")
    columnWidths.map(col).mkString("","","+") + "\n"
  }

  def removeRedundant(): (Seq[Row], Option[Row]) = {
    (rows, header)
    //    if (rows.isEmpty || rows.length == 1) return (rows, header)
    //
    //    val redundant:Seq[Boolean] = for (i <- rows.head.values.indices) yield {
    //      val col = rows.flatMap(r => if (r.values.length <= i) None else Some(r.values(i)))
    //      col.length > 1 && col.map(Table.toString(_)).distinct.length < 2
    //    }
    //    (
    //      rows.map(r => Row(r.values.zip(redundant).filter{case(_, r) => !r}.map{case(v,_) => v})),
    //      if (header.isEmpty) None else Some(Row(header.get.values.zip(redundant).filter{case(_, red) => !red}.map{case(v, _) => v}))
    //      )
  }

  def getPercentageCols(header: Option[Row]): Seq[Int] = header match {
    case Some(h) => h.values
      .zipWithIndex
      .flatMap{case(v, i) => if (Table.toString(v).toLowerCase.endsWith("pct")) Some(i) else None}
    case None => Nil
  }

  def toString(printHeader: Boolean = true, noQuotes: Boolean = false, rowsToShow: Int = -1): String = {

    val (rows, header) = removeRedundant()
    val widths = columnWidths(rows, header)
    val percentageCols = getPercentageCols(header)

    val r = if (rowsToShow == -1) rows.length else rowsToShow

    val q =
      if (noQuotes) ""
      else "```"
    q + {if (printHeader && header.isDefined) hLine(widths) + header.get.toColumnString(widths) + "\n" else ""} +
      hLine(widths) + rows.take(r).map(_.toRowString(widths, false, percentageCols.toSet)).mkString("", "\n", "\n") + hLine(widths) + q
  }

  def minus(other: Table): Option[Table] = {
    if (rows.length != other.rows.length) return None
    val newRows = rows.zip(other.rows).map{ case(thisRow, otherRow) =>
      if (thisRow.values.length != otherRow.values.length) return None
      val rowValues = thisRow.values.zip(otherRow.values).map{ case(v1, v2) => dif(v1, v2)}
      Row(rowValues)
    }
    Some(new Table(newRows, Some(header.get)))
  }

  def dif(v1: Any, v2: Any): Any = {
    try {
      val dif = Table.toString(v1.asInstanceOf[Double] - v2.asInstanceOf[Double])
      if (dif == "") Table.toString(v1)
      else Table.toString(v1) + " (" + dif + ")"
    } catch {
      case ex: Exception => Table.toString(v1)
    }
  }

  def tryStringToDouble: (String) => Double = { (x) => x.replace("%", "").replaceAll(" [(].*[)]", "").toDouble }

  def sortBy(col: String*): Table = {
    if (header.isEmpty) return this

    def sort(table: Table, col: String): Table = {
      val colInd = header.get.values.indexWhere(h => Table.toString(h).toLowerCase == col.toLowerCase)
      if (colInd == -1) this
      else {
        try {
          new Table(table.rows.sortBy(x => tryStringToDouble(x.values(colInd).toString)), header)
        } catch {
          case e: Exception =>
            new Table(table.rows.sortBy(_.values(colInd).toString), header)
        }
      }
    }

    def sortCol(table: Table, col: Seq[String]): Table = col match {
      case Seq(c) => sort(table, c)
      case _ => sortCol(sort(table, col.last), col.init)
    }
    sortCol(this, col.toSeq)
  }

  def reverse: Table = new Table(rows.reverse, header)

  def apply(rowInd: Int): Row = rows(rowInd)
}

/************************************************************/

case class Row(values: Seq[Any]) {

  def valuesTrunc(i: Int): String = {
    val split = Table.toString(values(i)).split(" ")
    if (split.length > 1 && (split.head.toLowerCase == "ucits" || split.head.toLowerCase == "hf")) split.init.mkString(" ")
    else Table.toString(values(i))
  }

  def space(s: Any, width: Int = 6, centered: Boolean = false, floor: Boolean = false): String =
    if (s == null) List.fill(width)(" ").mkString("")
    else {
      if (centered) {
        if (floor) List.fill(math.floor((width - s.toString.length)/2.0).toInt)(" ").mkString("")
        else List.fill(math.ceil((width - s.toString.length)/2.0).toInt)(" ").mkString("")
      }else List.fill(width - s.toString.length)(" ").mkString("")
    }

  def toRowString: String = toRowString(values.map(v => Table.length(v)+4), false, Set())

  def toRowString(columnWidths: Seq[Int], columnTitles: Boolean = false, percentageCols: Set[Int]): String = {
    val values =
      if (columnTitles) this.values.indices.map(valuesTrunc)
      else this.values

    columnWidths.zipWithIndex.zip(values).map{case ((w, i), v) => (w, Try(Table.toString(v, percentageCols.contains(i))))}.map {
      case (width, Success(value)) => " " + value + space(" " + value, width)
      case (width, Failure(_)) => space("", width)
    }.mkString("|", "|", "|")
  }

  def isTuple(value: Any): Boolean = {
    val split = Table.toString(value).split(" ")
    split.length > 1 && (split.head.toLowerCase == "ucits" || split.head.toLowerCase == "hf")
  }

  def toColumnString(columnWidths: Seq[Int]): String = {
    var c = 0
    val titleWidths = new ArrayBuffer[Int]
    while (c < columnWidths.length) {
      if (isTuple(values(c)) && (c+1 < columnWidths.length)) {
        titleWidths += (columnWidths(c) + columnWidths(c+1))
        c += 1
      } else titleWidths += columnWidths(c)
      c += 1
    }
    val titles = new ArrayBuffer[String]

    var i = 0
    while (i < values.length) {
      if (isTuple(values(i))) {
        titles += Table.toString(values(i)).split(" ").last
        i += 1
      } else titles += ""
      i += 1
    }
    /*titleWidths.zip(titles).map { case (width, value) =>
      " " + space(" " + value, width, true) + Table.capitalise(value) + space(" " + value, width, true, true)
    }.mkString("", "", "") + "\n" + */toRowString(columnWidths, true, Set())
  }
}

/************************************************************/

object Table /*extends App*/{

  //        val header = Row(List("AAA", "title1", "title2", "title3", "title4"))
  //        val rows = List(
  //          Row(List("1", "329.534952", "fadsff", "4t3", "0")),
  //          Row(List("-1", "32952", "", "44fdfg-34t3", "-10")),
  //          Row(List("2", "-400.952", "ffdfdnda", "44-3fsgffdfdsf4t3", "0.0011")),
  //          Row(List("3", "-400.952", "ffdfdnda", "44-3fsgffdfdsf4t3", "100.1")),
  //          Row(List("4", "-400.952", "ffdfdnda", "44-3fsgffdfdsf4t3", "-10"))
  //        )
  //
  //        println(new Table(rows, Some(header)).toString(true))

  def formatter(x: Double, percentage: Boolean = false): String = {

    if (percentage) {
      if (math.abs(x) <= 1e-2) return f"${x * 10000}%.0fb"
      else return f"${x * 100}%.1f%%"
    }

    //    if (math.abs(x) >= 1e9) f"${x / 1e9}%.1fbn"
    //    else if (math.abs(x) >= 1e6) f"${x / 1e6}%.1fm"
    //    else if (math.abs(x) >= 1e3) f"${x / 1e3}%.0fk"
    if (x == math.round(x)) f"$x%.0f"
    else f"$x%.1f"
  }

  def length(v: Any, percentage: Boolean = false): Int = v match {
    case v: Double => Table.formatter(v, percentage).length
    case v: Any =>
      try {
        Table.formatter(v.toString.toDouble, percentage).length
      } catch {
        case _: Exception =>
          try {
            v.toString.length
          } catch {
            case _: Exception => 0
          }
      }
    case _ => 0
  }

  def toString(v: Any, percentage: Boolean = false): String = v match {
    case v: Double =>
      if (v == 0) ""
      else if (v < 0) formatter(v, percentage)
      else "" + formatter(v, percentage)
    case v: Any =>
      try {
        val d = v.toString.toDouble
        if (d ==0) ""
        else if (d < 0) formatter(d, percentage)
        else "" + formatter(d, percentage)
      } catch { case _: Exception => capitalise(v.toString) }
  }

  def capitalise(value: String): String =
    if (value.length < 2) value.toUpperCase()
    else value.substring(0,1).toUpperCase + value.substring(1)
}