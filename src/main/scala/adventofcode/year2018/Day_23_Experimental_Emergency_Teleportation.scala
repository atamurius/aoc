package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_23_Experimental_Emergency_Teleportation extends Puzzle {

  type Pos = (Int, Int, Int)

  case class Bot(pos: Pos, radius: Int) {
    def inRange(p: Pos): Boolean = distance(pos, p) <= radius

    def intersects(that: Bot): Boolean = distance(this.pos, that.pos) <= (this.radius + that.radius)
  }

  object Bot {
    private val Format = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".r

    def parse: String => Bot = {
      case Format(x, y, z, r) => Bot((x.toInt, y.toInt, z.toInt), r.toInt)
    }
  }

  def distance(a: Pos, b: Pos): Int =
    (a.productIterator zip b.productIterator).foldLeft(0) {
      case (acc, (x: Int, y: Int)) => acc + math.abs(x - y)
    }

  def strongestSignalDraw(bots: Seq[Bot]): Set[Pos] = {
    val coverage = collection.mutable.Map.empty[Pos, Int] withDefaultValue 0
    for {
      bot @ Bot((cx,cy,cz), r) <- bots
      x <- cx - r to cx + r
      y <- cy - r to cy + r
      z <- cz - r to cz + r
      p = (x, y, z) if bot inRange p
    } coverage += p -> (coverage(p) + 1)
    val max = coverage.valuesIterator.max
    coverage.iterator.filter(_._2 == max).map(_._1).toSet
  }

  def strongestSignal(bots: Seq[Bot]): Set[Pos] = {
    val intersects = collection.mutable.Map.empty[Bot, collection.mutable.Set[Bot]]
    def intersection(a: Bot, b: Bot) = intersects.getOrElseUpdate(a, collection.mutable.Set.empty) += b
    bots.tails.foreach {
      case Nil =>
      case Seq(bot, rest @ _*) => for {
        other <- rest.iterator
        if bot intersects other
      } {
        intersection(bot, other)
        intersection(other, bot)
      }
    }

    @tailrec def fullIntersection(): Set[Bot] = {
      val (worst, ints) = intersects.minBy(_._2.size)
      if (ints.size == intersects.size - 1) intersects.keySet.toSet
      else {
        intersects -= worst
        intersects.valuesIterator.foreach(_ -= worst)
        fullIntersection()
      }
    }

    val best = fullIntersection()
    Debug(s"Best intersection: ${best.size}")

    val (b1, b2, dist) = (
      for (a <- best; b <- best if a != b)
        yield (a, b, a.radius + b.radius - distance(a.pos, b.pos))
    ).minBy(_._3)

    Debug(s"$b1 ~ $b2 = $dist")

    if (dist > 0) sys.error(s"Unhandled case: $b1, $b2, dist = $dist")
    else {
      val f = b1.radius.toDouble / distance(b1.pos, b2.pos)
      (b1.pos.productIterator zip b2.pos.productIterator).map {
        case (a: Int, b: Int) => a + (b - a) * f
      }.toSeq match {
        case Seq(dx, dy, dz) =>
          Debug(f"$dx%,.2f, $dy%,.2f, $dz%,.2f")
          val conv = Seq(math.ceil _, math.floor _).map(_ andThen (_.toInt))
          val res = for {
            xc <- conv
            yc <- conv
            zc <- conv
            p = (xc(dx), yc(dy), zc(dz))
            if b1.inRange(p) && b2.inRange(p)
          } yield p
          Debug(res)
          res.toSet
      }
    }
  }

  locally {
    val test =
      """
        |pos=<0,0,0>, r=4
        |pos=<1,0,0>, r=1
        |pos=<4,0,0>, r=3
        |pos=<0,2,0>, r=1
        |pos=<0,5,0>, r=3
        |pos=<0,0,3>, r=1
        |pos=<1,1,1>, r=1
        |pos=<1,1,2>, r=1
        |pos=<1,3,1>, r=1
      """.trim.stripMargin.lines.map(Bot.parse).toVector

    val max = test.maxBy(_.radius)
    test.count(b => distance(b.pos, max.pos) <= max.radius) shouldBe 7

  }

  locally {
    val test =
      """
        |pos=<10,12,12>, r=2
        |pos=<12,14,12>, r=2
        |pos=<16,12,12>, r=4
        |pos=<14,14,14>, r=6
        |pos=<50,50,50>, r=200
        |pos=<10,10,10>, r=5
      """.trim.stripMargin.lines.map(Bot.parse).toVector
    strongestSignal(test) shouldBe Set((12,12,12))
  }

  override type Input = Seq[Bot]

  override def input: Input = inputLines.map(Bot.parse).toVector

  override def answer1: Answer = { bs =>
    val Bot(center, radius) = bs.maxBy(_.radius)
    bs.count(b => distance(b.pos, center) <= radius)
  }

  override def answer2: Answer = bs => strongestSignal(bs).map(distance((0,0,0), _)).min
}
