package adventofcode.year2018

import adventofcode.common.Puzzle

case object Day_22_Mode_Maze extends Puzzle {

  type Pos = (Int, Int)

  sealed trait Equipment
  case object ClimbingGear extends Equipment
  case object Torch extends Equipment

  class Cave(depth: Int, target: Pos, mouth: Pos = (0,0)) {

    private val geoIndexCache = collection.mutable.Map.empty[Pos, Long]

    val Rocky = 0
    val Wet = 1
    val Narrow = 2

    def geoIndex(p: Pos): Long = {
      p match {
        case `target`|`mouth` => 0
        case (x, 0) => x * 16807
        case (0, y) => y * 48271
        case (x, y) => geoIndexCache.getOrElseUpdate(p, erosion((x - 1, y)) * erosion((x, y - 1)))
      }
    }

    def erosion(p: Pos): Int = ((geoIndex(p) + depth) % 20183).toInt

    def terrain(p: Pos): Int = erosion(p) % 3

    def allowedEquipmentAt(pos: Pos): Seq[Option[Equipment]] =
      if (pos == target) Seq(Some(Torch))
      else terrain(pos) match {
        case Rocky => Seq(Some(ClimbingGear), Some(Torch))
        case Wet => Seq(Some(ClimbingGear), None)
        case Narrow => Seq(Some(Torch), None)
      }

    def draw(size: Int): Unit = {
      val path = distanceToTarget.map(_ => pathToTarget().map(_._2).toSet).getOrElse(Set.empty)
      for (y <- 0 until size) {
        Debug((0 until size).map { x =>
          if (path((x,y))) '#'
          else terrain((x,y)) match {
            case Rocky => '.'
            case Wet => '='
            case Narrow => '|'
          }
        }.mkString(" "))
      }
    }

    def risk: Int = (for (x <- mouth._1 to target._1; y <- mouth._2 to target._2) yield terrain((x,y))).sum

    private val shortestPath =
      collection.mutable.Map.empty[(Pos, Option[Equipment]), (Int, Pos, Option[Equipment])] +=
        (mouth, Some(Torch)) -> (0, mouth, Some(Torch))

    def distanceToTarget: Option[Int] = shortestPath.get(target, Some(Torch)).map(_._1)

    val ChangeEquipmentPenalty = 7

    private val adjacent = Seq((1,0),(0,1),(-1,0),(0,-1))
    private val equipmentChoice = Seq(Some(Torch), Some(ClimbingGear), None)

    def stepOut(from: Pos, maxDistance: Option[Int] = None): Vector[Pos] = {
      val nexts = for {
        equipment <- equipmentChoice.iterator
        (walked, _, _) <- shortestPath.get(from, equipment).toSeq
        (dx, dy) <- adjacent
        next = (dx + from._1, dy + from._2) if next._1 >= 0 && next._2 >= 0
        nextEquipment <- allowedEquipmentAt(next)
        penalty = if (nextEquipment == equipment) 0 else ChangeEquipmentPenalty
        distance = walked + 1 + penalty
        if shortestPath.get((next, nextEquipment)).forall(_._1 > distance) && maxDistance.forall(_ >= distance)
      } yield ((next, nextEquipment), distance, equipment)
      nexts.foldLeft(Vector.empty[Pos]) {
        case (acc, (p @ (next, _), distance, equipment)) =>
          shortestPath += p -> (distance, from, equipment)
          acc :+ next
      }
    }

    def findShortestPathToTarget(): Int = {
      var front = Vector(mouth)
      while (front.nonEmpty) {
        val currentDistance = distanceToTarget
        front = front.flatMap(stepOut(_, currentDistance)).distinct
      }
      distanceToTarget.get
    }

    def pathToTarget(): Seq[(Int, Pos, Option[Equipment])] = {
      Iterator.iterate(shortestPath(this.target, Some(Torch))) {
        case (_, source, equipment) =>
          shortestPath(source, equipment)
      }.takeWhile(_._2 != mouth).toVector.reverse
    }
  }

  locally {
    val cave = new Cave(510, (10, 10))
    cave.geoIndex((1, 1)) shouldBe 145722555L
    cave.erosion((1, 1)) shouldBe 1805
    cave.risk shouldBe 114

    cave.findShortestPathToTarget() shouldBe 45
//    cave.draw(16)
  }

  override type Input = (Int, Pos)

  override def input: Input = (10914, (9,739))

  override def answer1: Answer = { case (depth, target) =>
    new Cave(depth, target).risk
  }

  override def answer2: Answer = { case (depth, target) =>
    new Cave(depth, target).findShortestPathToTarget()
  }
}
