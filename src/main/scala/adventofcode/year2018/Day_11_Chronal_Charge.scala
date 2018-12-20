package adventofcode.year2018

import adventofcode.common.Puzzle

case object Day_11_Chronal_Charge extends Puzzle {

  type Cell = (Int, Int)

  def cellFuel(serial: Int)(cell: Cell): Int = {
    val (x, y) = cell
    val level = ((x + 10) * y + serial) * (x + 10) % 1000
    level / 100 % 10 - 5
  }

  def cellBox(fuel: Cell => Int)(tl: Cell): Int = {
    val (lx, ty) = tl
    (for (x <- lx to lx + 2; y <- ty to ty + 2) yield fuel((x, y))).sum
  }

  def drawBox(fuel: Cell => Int)(tl: Cell, sz: Int): Unit = {
    val (lx, ty) = tl
    Disabled("----" + (lx until lx + sz).map(x => f"$x%4d").mkString)
    for (y <- ty until ty + sz) {
      Disabled(f"$y%3d:" + (lx until lx + sz).map { x =>
        f"${fuel((x, y))}%4d"
      }.mkString)
    }
  }

  def maxInLine(fuel: Cell => Int)(x: Int, ys: (Int, Int)): (Cell, Int) = {
    var col1, col2, col3 = 0
    val (minY, maxY) = ys

    def column(y: Int): Int = (x until x + 3).map(x => fuel((x, y))).sum

    col1 = column(minY + 0)
    col2 = column(minY + 1)

    var max = 0
    var maxCell: Cell = (0, 0)
    for (y <- minY to maxY) {
      col3 = column(y + 2)
      val s = col1 + col2 + col3
      if (s > max) {
        max = s
        maxCell = (x, y)
      }
      col1 = col2
      col2 = col3
    }
    maxCell -> max
  }

  def maxInArea(fuel: Cell => Int)(tl: Cell, br: Cell): (Cell, Int) = {
    val (minX, minY) = tl
    val (maxX, maxY) = br
    val ys = (minY, maxY)
    (minX to maxX).map { x =>
      maxInLine(fuel)(x, ys)
    }.maxBy(_._2)
  }

  cellFuel(8)((3,5)) shouldBe 4
  cellFuel(57)((122,79)) shouldBe -5
  cellFuel(39)((217,196)) shouldBe 0
  cellFuel(71)((101,153)) shouldBe 4

  cellBox(cellFuel(18))((33, 45)) shouldBe 29

  final val MaxPoint: Cell = (300, 300)

  maxInArea(cellFuel(18))((0,0),MaxPoint)._1 shouldBe (33,45)
  maxInArea(cellFuel(42))((0,0),MaxPoint)._1 shouldBe (21,61)

  def square(fuel: Cell => Int)(start: Cell, size: Int): Int = {
    val (sx, sy) = start
    val all = for (x <- sx until sx + size; y <- sy until sy + size) yield fuel((x, y))
    all.sum
  }
  
  def squares(fuel: Cell => Int)(start: Cell, max: Int): Iterator[Int] = {
    val (x, y) = start
    val maxSize = max - (x max y)
    (1 to maxSize).iterator.scanLeft(0) { (prev, size) =>
      val delta = size - 1
      val h = (x until x + size - 1).map(x => fuel((x, y + delta))).sum
      val v = (y until y + size - 1).map(y => fuel((x + delta, y))).sum
      prev + h + v + fuel((x + delta, y + delta))
    }.drop(1)
  }

  squares(cellFuel(18))((0, 0), 10).toList shouldBe (1 to 10).map(square(cellFuel(18))((0, 0), _)).toList

  def maxSizedInArea(fuel: Cell => Int)(max: Int): (Cell, Int) = {
    val (minX, minY) = (0, 0)
    val all = for (x <- (minX until max).iterator; y <- minY until max) yield {
      val (sum, sz) = squares(fuel)((x, y), max).zipWithIndex.maxBy(_._1)
      (x, y) -> sz -> sum
    }
    all.maxBy(_._2)._1
  }

  case class SumBuffer(size: Int)(f: Int => Int) {
    val sumFromZero: Vector[Int] = (0 until size).scanLeft(0)((acc, i) => acc + f(i)).toVector

    def sum(from: Int, until: Int): Int = sumFromZero(until) - sumFromZero(from)
  }

  SumBuffer(10)(identity).sum(3, 7) shouldBe (3 until 7).sum

  case class Squares(size: Int)(f: Cell => Int) {
    private val columns = Vector.tabulate(size) { x =>
      SumBuffer(size)(y => f((x, y)))
    }
    private val rows = Vector.tabulate(size) { y =>
      SumBuffer(size)(x => f((x, y)))
    }

    def squaresFrom(start: Cell, max: Int): Iterator[Int] = {
      val (x, y) = start
      (1 until max).iterator.scanLeft(f(x, y)) { (prev, d) =>
        val column = columns(x + d).sum(y, y + d)
        val row = rows(y + d).sum(x, x + d)
        val v = f(x + d, y + d)
        prev + column + row + v
      }
    }

    def maxSquare(max: Int): ((Int, Int, Int), Int) = {
      val all = for {
        x <- (0 until max).iterator
        y <- 0 until max
        (sum, sz) = squaresFrom((x, y), max - (x max y)).zipWithIndex.maxBy(_._1)
      } yield ((x, y, sz + 1), sum)
      all.maxBy(_._2)
    }
  }

  private val squares18 = Squares(300)(cellFuel(18))
  squares18.squaresFrom((0, 0), 10).toList shouldBe (1 to 10).map(square(cellFuel(18))((0, 0), _)).toList
//  squares18.maxSquare(300) shouldBe ((90,269,16), 113)

  override type Input = Int

  override def input: Input = 7689

  override def answer1: Answer = s => maxInArea(cellFuel(s))((0, 0), MaxPoint)

  override def answer2: Answer = s => Squares(300)(cellFuel(s)).maxSquare(300)._1
}
