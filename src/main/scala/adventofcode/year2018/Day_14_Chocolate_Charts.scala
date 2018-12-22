package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec
import scala.language.postfixOps

case object Day_14_Chocolate_Charts extends Puzzle {

  class Scoreboard {
    private var scores = Vector[Byte](3, 7)
    private var (i, j) = (0, 1)

    @tailrec
    final def next(count: Int = 1): this.type =
      if (count > 0) {
        val value = scores(i) + scores(j)
        if (value >= 10) scores :+= (value / 10).toByte
        scores :+= (value % 10).toByte
        i = (i + scores(i) + 1) % scores.size
        j = (j + scores(j) + 1) % scores.size
        next(count - 1)
      }
      else this

    def ensure(count: Int): this.type = next(count - scores.size)

    def slice(after: Int, count: Int): String = {
      ensure(after + count)
      scores.iterator.slice(after, after + count).mkString
    }

    def findOffset(digits: String): Int = findOffset(digits.toVector.map(_ - '0' toByte))

    def findOffset(digits: Vector[Byte]): Int = {
      var offset = -1
      var found = false
      while (!found) {
        offset += 1
        ensure(offset + digits.size)
        found = scores(offset) == digits.head && digits.iterator.zipWithIndex.forall {
          case (x, k) => x == scores(offset + k)
        }
      }
      offset
    }

    def debug(): this.type = {
      Disabled(scores.zipWithIndex.map {
        case (x, k) if k == i || k == j => s"[$x]"
        case (x, _) => s" $x "
      }.mkString)
      this
    }
  }

  val scoreboard = new Scoreboard

  scoreboard.slice(9, 10) shouldBe "5158916779"
  scoreboard.slice(18, 10) shouldBe "9251071085"
  scoreboard.slice(2018, 10) shouldBe "5941429882"

  scoreboard.findOffset("51589") shouldBe 9
  scoreboard.findOffset("01245") shouldBe 5
  scoreboard.findOffset("92510") shouldBe 18
  scoreboard.findOffset("59414") shouldBe 2018

  override type Input = Int

  override def input: Input = 793031

  override def answer1: Answer = scoreboard.slice(_, 10)

  override def answer2: Answer = n => scoreboard.findOffset(n.toString)
}
