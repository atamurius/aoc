package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

case object Day_15_Beverage_Bandits extends Puzzle {

  class CombatUnit(
    val kind: Char,
    var position: Pos,
    var hitPoints: Int = 200,
    val power: Int = 3
  ) {
    override def toString: String = s"$kind($hitPoints)"
  }

  type Pos = (Int, Int)

  case class Combat(
    width: Int,
    height: Int,
    walls: Set[Pos],
    var units: Set[CombatUnit]
  )

  def copy(c: Combat)(power: PartialFunction[Char, Int]): Combat = c.copy(units = c.units.map { u =>
    new CombatUnit(u.kind, u.position, u.hitPoints, power.applyOrElse(u.kind, (_: Char) => u.power))
  })

  final val ReadingOrder: Ordering[Pos] = new Ordering[Pos] {
    override def compare(x: (Int, Int), y: (Int, Int)): Int = (x, y) match {
      case ((_, y1), (_, y2)) if y1 != y2 => y1 compareTo y2
      case ((x1, _), (x2, _)) => x1 compareTo x2
    }
  }

  def displayCombat(implicit c: Combat): Unit = {
    val units = c.units.groupBy(_.position).transform((_, vs) => vs.head)
    for (y <- 0 until c.height) {
      val row = for (x <- 0 until c.width)
        yield if (c.walls((x,y))) '#'
        else units.get((x,y)).fold('.')(_.kind)
      val hp = for {
        unit <- units.values.toSeq.filter(_.position._2 == y).sortBy(_.position)(ReadingOrder)
      } yield s"${unit.kind}(${unit.hitPoints})"
      Debug(row.mkString(" ") + "\t" + hp.mkString(", "))
    }
    Debug("")
  }

  def toCombat(lines: Iterable[String]): Combat = {
    val items = for {
      (line, y) <- lines.toSeq.zipWithIndex
      (c, x) <- line.zipWithIndex
      if c != '.'
    } yield ((x,y), c)
    val (walls, units) = items.partition(_._2 == '#')
    Combat(
      width = lines.head.length,
      height = lines.size,
      walls = walls.map(_._1).toSet,
      units = units.map { case (pos, k) => new CombatUnit(k, pos) }.toSet
    )
  }

  def adjacent(pos: Pos): Set[Pos] = Set((0,1),(0,-1),(1,0),(-1,0)).map { case (x, y) => (pos._1 + x, pos._2 + y) }

  def combatRound(implicit c: Combat): Boolean = c.units.toSeq.sortBy(_.position)(ReadingOrder) forall combatTurn

  def combatTurn(unit: CombatUnit)(implicit c: Combat): Boolean =
    if (unit.hitPoints <= 0) true
    else if (walkToTarget(unit)) {
      attack(unit)
      true
    }
    else false

  class Distances(implicit c: Combat) extends (Pos => Int) {
    private val distance = Vector.fill(c.width) {
      ArrayBuffer.fill(c.height)(Int.MaxValue)
    }
    val occupied: Set[Pos] = c.walls ++ c.units.map(_.position)

    def valid(p: Pos): Boolean = p match {
      case (x, y) => 0 <= x && x < c.width && 0 <= y && y <= c.height
    }

    override def apply(p: Pos): Int = distance(p._1)(p._2)

    @tailrec
    final def fill(ps: Iterable[Pos], dist: Int = 0): Unit = {
      val points = ps.filter(p => valid(p) && this(p) > dist)
      if (points.nonEmpty) {
        points.foreach {
          case (x, y) => distance(x)(y) = dist
        }
        val next = points.flatMap(adjacent).filterNot(occupied)
        fill(next, dist + 1)
      }
    }

    def debug(): Unit = for (y <- 0 until c.height) {
      Debug((0 until c.width).map(x => distance(x)(y)).map {
        case Int.MaxValue => " -"
        case x => f"$x%2d"
      }.mkString(" "))
    }
  }

  def walkToTarget(unit: CombatUnit)(implicit c: Combat): Boolean = {
    val close = adjacent(unit.position)
    val enemies = c.units.filter(_.kind != unit.kind).map(_.position)
    if (!enemies.exists(close)) {
      val dist = new Distances
      dist.fill(enemies)
      val walk = close.map(dist).min
      if (walk != Int.MaxValue) {
        val walkTo = close.filter(p => dist(p) == walk).min(ReadingOrder)
        unit.position = walkTo
      }
    }
    enemies.nonEmpty
  }

  def attack(unit: CombatUnit)(implicit c: Combat): Unit = {
    val close = adjacent(unit.position)
    val targets = c.units.filter(e => e.kind != unit.kind && close(e.position))
    if (targets.nonEmpty) {
      val minHP = targets.map(_.hitPoints).min
      val target = targets.filter(_.hitPoints == minHP).minBy(_.position)(ReadingOrder)
      target.hitPoints -= unit.power
      if (target.hitPoints <= 0) {
        c.units -= target
      }
    }
  }

  def combatEnded(implicit c: Combat): Boolean = c.units.map(_.kind).size < 2

  def runCombat(debug: Boolean = false)(implicit c: Combat): Int = {
    if (debug) {
//      print("\033[H\033[2J")
      Debug(s"Initial round -------------------")
      displayCombat
    }
    var round = 0
    while (combatRound) {
      round += 1
      if (debug) {
//        print("\033[H\033[2J")
        Debug(s"Round #$round -------------------")
        displayCombat
//        Thread.sleep(500)
      }
    }
    if (debug) {
      //        print("\033[H\033[2J")
      Debug(s"Finished -------------------")
      displayCombat
      //        Thread.sleep(500)
    }
    round
  }

  def winCombat(targetWinner: Char)(implicit c: Combat): Boolean = {
    val count = c.units.count(_.kind == targetWinner)
    while (combatRound && c.units.count(_.kind == targetWinner) == count) { }
    c.units.count(_.kind == targetWinner) == count
  }

  def totalHitPoints(implicit c: Combat): Int = c.units.foldLeft(0)(_ + _.hitPoints)

  def solveOutcome(map: String, debug: Boolean = false): Int = {
    implicit val c: Combat = toCombat(map.lines.toSeq)
    val rounds = runCombat(debug)
    rounds * totalHitPoints
  }

  def tryToWin(c: Combat, target: Char): Int = {
    var lose = 3
    var power = 3
    var win = false
    while (!win) {
      lose = power
      power += 10
      val test = copy(c) { case `target` => power }
      win = winCombat(target)(test)
    }
    tryToWin(c, target, lose, power)
  }

  @tailrec
  def tryToWin(c: Combat, target: Char, lose: Int, win: Int): Int = {
//    Debug(s"lose: $lose, win: $win")
    if (lose + 1 == win) win
    else {
      val middle = (lose + win) / 2
      val test = copy(c) { case `target` => middle }
      if (!winCombat(target)(test)) tryToWin(c, target, lose = middle, win)
      else tryToWin(c, target, lose, win = middle)
    }
  }

  solveOutcome(
    """
      |#######
      |#.G...#
      |#...EG#
      |#.#.#G#
      |#..G#E#
      |#.....#
      |#######
    """.trim.stripMargin
  ) shouldBe 27730

  solveOutcome(
    """
      |#######
      |#G..#E#
      |#E#E.E#
      |#G.##.#
      |#...#E#
      |#...E.#
      |#######
    """.trim.stripMargin
  ) shouldBe 36334

  solveOutcome(
    """
      |#######
      |#E..EG#
      |#.#G.E#
      |#E.##E#
      |#G..#.#
      |#..E#.#
      |#######
    """.trim.stripMargin
  ) shouldBe 39514

  solveOutcome(
    """
      |#######
      |#E.G#.#
      |#.#G..#
      |#G.#.G#
      |#G..#.#
      |#...E.#
      |#######
    """.trim.stripMargin
  ) shouldBe 27755

  solveOutcome(
    """
      |#######
      |#.E...#
      |#.#..G#
      |#.###.#
      |#E#G#G#
      |#...#G#
      |#######
    """.trim.stripMargin
  ) shouldBe 28944

  solveOutcome(
    """
      |#########
      |#G......#
      |#.E.#...#
      |#..##..G#
      |#...##..#
      |#...#...#
      |#.G...G.#
      |#.....G.#
      |#########
    """.trim.stripMargin
  ) shouldBe 18740

  tryToWin(
    toCombat(
      """
        |#######
        |#.G...#
        |#...EG#
        |#.#.#G#
        |#..G#E#
        |#.....#
        |#######
      """.trim.stripMargin.lines.toSeq),
    'E'
  ) shouldBe 15

  override type Input = Combat

  override def input: Input = toCombat(inputLines)

  override def answer1: Answer = { implicit c =>
    val rounds = runCombat()
    totalHitPoints * rounds
  }

  override def answer2: Answer = { c =>
    val p = tryToWin(c, 'E')
    Disabled(s"Least power: $p")
    implicit val c1: Combat = copy(c) { case 'E' => p }
    val rounds = runCombat()
    totalHitPoints * rounds
  }
}
