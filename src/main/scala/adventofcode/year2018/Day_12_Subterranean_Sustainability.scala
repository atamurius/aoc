package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_12_Subterranean_Sustainability extends Puzzle {

  type State = Set[Long]

  private val test =
    """
      |initial state: #..#.#..##......###...###
      |
      |...## => #
      |..#.. => #
      |.#... => #
      |.#.#. => #
      |.#.## => #
      |.##.. => #
      |.#### => #
      |#.#.# => #
      |#.### => #
      |##.#. => #
      |##.## => #
      |###.. => #
      |###.# => #
      |####. => #
    """.trim.stripMargin.lines

  def describe(state: State): String = {
    (state.min to state.max).map { i =>
      val res = if (state(i)) "#" else "."
      if (i == 0) s"[$res]"
      else res
    }.mkString
  }

  def toRule(line: String): Rule = {
    val Array(state, result) = line.split(" => ")
    Rule(state.iterator.zipWithIndex.filter(_._1 == '#').map(_._2 - 2).toSet, result == "#")
  }

  case class Rule(given: Set[Int], result: Boolean) {
    require(given.nonEmpty || !result, s"Cannot do something from nothing: $this")

    override def toString: String =
      s"${(-2 to 2).map(v => if (given(v)) '#' else '.').mkString} => ${if (result) '#' else '.'}"

    final lazy val left = given.min
    final val pattern = (-2 to 2).map(i => (given(i), i)).toVector

    def matched(state: State, at: Long): Boolean =
      pattern.forall {
        case (set, i) => state(i + at) == set
      }

    def applyTo(state: State): State =
      if (given.isEmpty) Set.empty
      else state.flatMap { i =>
        val pos = i - left
        if (matched(state, pos) && result) Seq(pos)
        else Nil
      }
  }

  toRule("..#.. => #") applyTo Set(1,2,5) shouldBe Set(5)
  toRule("##.## => #") applyTo Set(1,2,4,5) shouldBe Set(3)

  def generation(rules: Seq[Rule])(state: State): State =
    rules.foldLeft[State](Set.empty)(_ ++ _.applyTo(state))

  generation(Seq(
    toRule("..#.. => #"),
    toRule(".#.#. => #")
  ))(Set(5,7,15)) shouldBe Set(6,15)

  def generations(initial: State, rules: Seq[Rule]): Iterator[State] = {
    val gen = generation(rules) _
    Iterator.iterate(initial)(gen)
  }

  def readRules(lines: Iterator[String]): (State, Seq[Rule]) = {
    val initial = lines.next().stripPrefix("initial state: ").zipWithIndex.filter(_._1 == '#').map(_._2.toLong).toSet
    val rules = lines.filter(_.nonEmpty).map(toRule).filter(r => r.result && r.given.nonEmpty).toVector
    (initial, rules)
  }

  def normalize(state: State): State =
    if (state.isEmpty) state
    else {
      val min = state.min
      state.map(_ - min)
    }

  @tailrec
  def detectLoop(generations: Iterator[State], now: Int = 0, prev: State = Set.empty: State): (Int, Long) = {
    val current = generations.next()
    if (normalize(prev) != normalize(current)) detectLoop(generations, now + 1, current)
    else (now - 1, current.min - prev.min)
  }

  @tailrec
  def evalState(generations: Iterator[State], after: Long, now: Int = 0, prev: State = Set.empty: State): State = {
    val current = generations.next()
    if (now == after) current
    else if (normalize(prev) != normalize(current)) evalState(generations, after, now + 1, current)
    else {
      val shiftPerGen = current.min - prev.min
      val shift = (after - now) * shiftPerGen
      current.map(_ + shift)
    }
  }

  locally {
    val (initial, rules) = readRules(test)
    def gens = generations(initial, rules)
//    gens
//      .take(21)
//      .zipWithIndex
//      .foreach { case (state, i) =>
//        Disabled(f"$i%2d: (${state.min}%4d) ${describe(state)}")
//      }
    gens.drop(20).next().sum shouldBe 325
    detectLoop(generations(initial, rules)) shouldBe (86, 1)
    gens.drop(100).next() shouldBe evalState(gens, 100)
  }

  override type Input = (State, Seq[Rule])

  override def input: Input = readRules(inputLines.iterator)

  override def answer1: Answer = { case (initial, rules) => generations(initial, rules).drop(20).next().sum }

  override def answer2: Answer = { case (initial, rules) => evalState(generations(initial, rules), 50000000000L).sum }
}
