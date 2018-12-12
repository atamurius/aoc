package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_6_Chronal_Coordinates extends Puzzle {

  type Pos = (Int, Int)

  override type Input = Vector[Pos]

  override def input: Input = inputLines.map(_.split(", ")).map {
    case Array(x, y) => (x.toInt, y.toInt)
  }.toVector

  def field(points: Iterable[Pos]): Field = {
    val (w, h) = points.foldLeft((0, 0)) {
      case ((mx, my), (x, y)) => (mx max x, my max y)
    }
    Field(w + 1, h + 1).fill(points)
  }

  type Id = Int

  val Unknown: Id = -1

  case class Field(width: Int, height: Int) {

    private val distances = Vector.fill(width) {
      collection.mutable.ArrayBuffer.fill(height) {
        Option.empty[(Id, Int)]
      }
    }

    def valid(p: Pos): Boolean = distances.indices.contains(p._1) && distances.head.indices.contains(p._2)

    @tailrec
    private def spread(points: Iterable[Pos]): Unit = if (points.nonEmpty) {
      var changed = List.empty[Pos]
      for {
        p @ (x, y) <- points
        Some((owner, distance)) = distances(x)(y) if owner != Unknown
        (dx, dy) <- Seq((1,0),(0,1),(-1,0),(0,-1))
        t @ (tx, ty) = (x + dx, y + dy) if valid(t)
        current = distance + 1
      } distances(tx)(ty) match {
        case None =>
          distances(tx)(ty) = Some(owner -> current)
          changed ::= t
        case Some((`owner`, `current`)) =>
        case Some((_, `current`)) =>
          distances(tx)(ty) = Some(Unknown -> current)
        case _ =>
      }
      spread(changed)
    }

    def fill(centers: Iterable[Pos]): this.type = {
      for (((x, y), id) <- centers.zipWithIndex) {
        distances(x)(y) = Some(id -> 0)
      }
      spread(centers)
      this
    }

    def edges: Set[Id] = {
      val vertical = for {
        x <- Set(0, width - 1)
        y <- distances.head.indices
      } yield (x, y)
      val horizontal = for {
        y <- Set(0, height - 1)
        x <- distances.indices
      } yield (x, y)
      (vertical ++ horizontal).map {
        case (x,y) => distances(x)(y).map(_._1).getOrElse(Unknown)
      } - Unknown
    }

    def areaOf(id: Id): Int = distances.map { row =>
      row.count(_.exists(_._1 == id))
    }.sum

    def debug(): Unit = {
      def name(id: Id): Char = if (id == Unknown) '?' else ('A' + id).toChar
      for (y <- 0 until height) {
        val row = (0 until width)
          .map(x => distances(x)(y).map { case (id, d) => f"${name(id)}%2s:$d%-3d" }.getOrElse(" -:-  "))
          .mkString
        Disabled(row)
      }
    }
  }

  locally {
    val test = Seq((1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9))
    val f = field(test)
    val nonEdge = test.indices.toSet -- f.edges
    nonEdge.map(id => id -> f.areaOf(id)).toMap shouldBe Map(
      3 -> 9,
      4 -> 17
    )
  }

  override def answer1: Answer = { centers =>
    val f = field(centers)
    val inner = centers.indices.toSet -- f.edges
    inner.map(f.areaOf).max
  }

  override def answer2: Answer = ???
}
