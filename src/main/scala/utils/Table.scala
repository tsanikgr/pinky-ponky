package utils

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/************************************************************/

class Table(val rows: Seq[Row], val header: Option[Row]) {

	def this(rows: Seq[Seq[Any]], header: Seq[Any]) = this(rows.map(Row), Some(Row(header)))
	def this(rows: Seq[Seq[Any]]) = this(rows.map(Row), None)

  def isEqual(other: Table): Boolean = {
    rows.length == other.rows.length &&
      rows.zip(other.rows).forall{ case(thisRow, otherRow) =>
        thisRow.values.length == otherRow.values.length &&
          thisRow.values.zip(otherRow.values).forall{case (v1, v2) => Table.toString(v1) == Table.toString(v2)}}
  }

  def columnWidths(rows: Seq[Row], header: Option[Row]):ArrayBuffer[Int] = {
    val buf = new ArrayBuffer[Int]

    for (row <- rows) {
      while (buf.length < row.values.length) buf += 0
      row
        .values
        .indices
        .foreach(i => buf(i) = buf(i) max Table.length(row.values(i)))
    }
    if (header.isDefined) {
			while (buf.length < header.get.values.length) buf += 0
			for (i <- header.get.values.indices) {
				buf(i) = buf(i) max Table.length(Table.toString(header.get.values(i)))
			}
		}
    buf.map(_ + 2)
  }

  def hLine(columnWidths: Seq[Int]): String = columnWidths.map("+" + "-" * _).mkString("","","+") + "\n"

  def toString(printHeader: Boolean = true, noQuotes: Boolean = false, rowsToShow: Int = -1): String = {

    val widths = columnWidths(rows, header)

    val r = if (rowsToShow == -1) rows.length else rowsToShow
    val stringRows = {
			if (rows.isEmpty && rowsToShow != 0 && header.isDefined)
				Seq(Row(header.get.values.map(_ => "")).toRowString(widths)).mkString("", "\n", "\n")
			else
				rows.take(r).map(_.toRowString(widths)).mkString("", "\n", "\n")
    }

    val q = if (noQuotes) "" else "```"

		q +
			{if (printHeader && header.isDefined) hLine(widths) + header.get.toRowString(widths, columnTitles = true) + "\n" else ""} +
      hLine(widths) +
			stringRows +
			hLine(widths) +
			q
  }

  def tryStringToDouble: (String) => Double = { x =>
		val s = x.replace("%", "").replaceAll(" [(].*[)]", "")
		if (s == "") 0.0
		else s.toDouble
	}

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

  def space(s: Any, width: Int = 6, centered: Boolean = false, floor: Boolean = false): String =
    if (s == null) List.fill(width)(" ").mkString("")
    else {
      if (centered) {
        if (floor) List.fill(math.floor((width - s.toString.length)/2.0).toInt)(" ").mkString("")
        else List.fill(math.ceil((width - s.toString.length)/2.0).toInt)(" ").mkString("")
      }else List.fill(width - s.toString.length)(" ").mkString("")
    }

  def toRowString(columnWidths: Seq[Int], columnTitles: Boolean = false): String = {
    columnWidths.zipWithIndex.zip(values).map{case ((w, i), v) => (w, Try(Table.toString(v)))}.map {
      case (width, Success(value)) => " " + value + space(" " + value, width)
      case (width, Failure(_)) => space(" ", width)
    }.mkString("|", "|", "|")
  }
}

/************************************************************/

object Table {

  def formatter(x: Double): String = {
    //    if (math.abs(x) >= 1e9) f"${x / 1e9}%.1fbn"
    //    else if (math.abs(x) >= 1e6) f"${x / 1e6}%.1fm"
    //    else if (math.abs(x) >= 1e3) f"${x / 1e3}%.0fk"
    if (x == math.round(x)) f"$x%.0f"
    else f"$x%.1f"
  }

  def length(v: Any): Int = v match {
    case v: Double => Table.formatter(v).length
    case v: Any =>
      try {
        Table.formatter(v.toString.toDouble).length
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

  def toString(v: Any): String = v match {
    case v: Double =>
      if (v == 0) ""
      else if (v < 0) formatter(v)
      else "" + formatter(v)
    case v: Any =>
      try {
        val d = v.toString.toDouble
        if (d ==0) ""
        else if (d < 0) formatter(d)
        else "" + formatter(d)
      } catch { case _: Exception => capitalise(v.toString) }
  }

  def capitalise(value: String): String =
    if (value.length < 2) value.toUpperCase()
    else value.substring(0,1).toUpperCase + value.substring(1)
}