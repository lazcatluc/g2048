package game

import scala.util.Random
/**
 * @author catalin
 */
object Table {
  val tableSize = 4
  def table: Array[Array[Option[Int]]] = Array.tabulate(tableSize, tableSize)((x, y) => None)

  def acceptNumber(tbl: Array[Array[Option[Int]]], x: Int, y: Int, number: Int): Array[Array[Option[Int]]] = tbl(x)(y) match {
    case None => {
      tbl(x)(y) = Some(number)
      tbl
    }
    case _ => acceptNumber(tbl, nextRandom, nextRandom, number)
  }

  def nextRandom = Random.nextInt(tableSize)

  def left(tbl: Array[Array[Option[Int]]]): Array[Array[Option[Int]]] = tbl map flattenRow
  def right(tbl: Array[Array[Option[Int]]]): Array[Array[Option[Int]]] = tbl map { _.reverse } map flattenRow map { _.reverse }
  def up(tbl: Array[Array[Option[Int]]]): Array[Array[Option[Int]]] = left(tbl transpose) transpose
  def down(tbl: Array[Array[Option[Int]]]): Array[Array[Option[Int]]] = right(tbl transpose) transpose

  def flattenRow(row: Array[Option[Int]]): Array[Option[Int]] = {
    def existing = addMatching(row.filter { x => x.isDefined })
    def nones = Array.tabulate(row.size - existing.size)(_ => None)

    existing ++ nones
  }

  def addMatching(row: Array[Option[Int]]): Array[Option[Int]] = row match {
    case Array(first, second, rest @ _*) if first == second => Array(Some(first.get + second.get)) ++ addMatching(rest.toArray)
    case Array(first, rest @ _*) => Array(first) ++ addMatching(rest.toArray)
    case _ => row
  }

  def printInt(x: Option[Int]) = x match {
    case None => print("•    ")
    case Some(number) => print("•" + "%1$4s".format(number))
  }

  def printArray(row: Array[Option[Int]]) = {
    row foreach printInt
    println()
  }

  def printTable(tbl: Array[Array[Option[Int]]]): Array[Array[Option[Int]]] = {
    tbl foreach printArray
    tbl
  }

  def process(tbl: Array[Array[Option[Int]]], ch: Char): Array[Array[Option[Int]]] = ch match {
    case 'w' => up(tbl)
    case 's' => down(tbl)
    case 'a' => left(tbl)
    case 'd' => right(tbl)
  }

  def main(args: Array[String]) {
    Iterator.continually(scala.io.StdIn.readChar()).takeWhile(_ != 'x')
      .foldLeft(printTable(acceptNumber(table, nextRandom, nextRandom, 2)))((tbl, ch) => 
          printTable(acceptNumber(process(tbl, ch), nextRandom, nextRandom, 2)))
  }

}
