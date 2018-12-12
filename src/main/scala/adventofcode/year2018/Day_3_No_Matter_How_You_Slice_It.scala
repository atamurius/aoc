package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.collection.mutable.ArrayBuffer

case object Day_3_No_Matter_How_You_Slice_It extends Puzzle {

  override type Input = Iterable[Rect]

  case class Rect(id: String, left: Int, top: Int, width: Int, height: Int)

  def toRect(str: String): Rect = str match {
    case RectFormat(id, left, top, width, height) =>
      Rect(id, left.toInt, top.toInt, width.toInt, height.toInt)
    case other => sys.error(s"Invalid rect: $other")
  }

  private val RectFormat = """#(\S+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  override def input: Input = inputLines map toRect

  class Fabric(val width: Int, val height: Int) {
    private val occupied = Vector.fill[ArrayBuffer[List[String]]](height) {
      ArrayBuffer.fill(width)(Nil)
    }

    def occupy(rect: Rect): this.type = {
      for {
        x <- rect.left until rect.left + rect.width
        y <- rect.top until rect.top + rect.height
      } occupied(y)(x) ::= rect.id
      this
    }

    def occupy(rects: Iterable[Rect]): this.type = rects.foldLeft[this.type](this)(_ occupy _)

    def overlapped(): Int = occupied.iterator.map(_.count(_.size > 1)).sum

    def overlappedIds(): Set[String] = occupied.foldLeft(Set.empty[String]) {
      (set, row) => row.foldLeft(set) {
        (set, pos) =>
          if (pos.size > 1) set ++ pos
          else set
      }
    }

    def debug(): this.type = {
      occupied.foreach { row =>
        Disabled(row.mkString(" "))
      }
      this
    }
  }

  locally {
    val fabric = new Fabric(8, 8)
      .occupy(
      """
        |#1 @ 1,3: 4x4
        |#2 @ 3,1: 4x4
        |#3 @ 5,5: 2x2
      """.stripMargin.lines.filter(_.trim.nonEmpty).toSeq map toRect
      )
//      .debug()

    fabric.overlapped() shouldBe 4
    fabric.overlappedIds() shouldBe Set("1", "2")
  }

  override def answer1: Answer = new Fabric(1000,1000).occupy(_).overlapped()

  override def answer2: Answer = { rs =>
    val overlapped = new Fabric(1000, 1000).occupy(rs).overlappedIds()
    rs.iterator.map(_.id).find(!overlapped.contains(_)).get
  }
}
