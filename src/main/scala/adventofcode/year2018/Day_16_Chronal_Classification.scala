package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.annotation.tailrec

case object Day_16_Chronal_Classification extends Puzzle {

  type Memory = Vector[Int]

  type ValueSource = Int => Memory => Int
  def value(x: Int): Memory => Int = Function.const(x)
  def register(x: Int): Memory => Int = _(x)
  def ignore(x: Int): Memory => Int = Function.const(0)

  case class Operation(name: String, A: ValueSource, B: ValueSource = ignore, f: (Int, Int) => Int) {
    def apply(a: Int, b: Int, c: Int)(mem: Memory): Memory =
      mem updated (c, f(A(a)(mem), B(b)(mem)))

    override def toString: String = name
  }

  def bool(f: (Int, Int) => Boolean): (Int, Int) => Int = (x, y) => if (f(x, y)) 1 else 0

  val Operations = Set(
    Operation("addr", register, register, f = _ + _),
    Operation("addi", register, value,    f = _ + _),
    Operation("mulr", register, register, f = _ * _),
    Operation("muli", register, value,    f = _ * _),
    Operation("banr", register, register, f = _ & _),
    Operation("bani", register, value,    f = _ & _),
    Operation("borr", register, register, f = _ | _),
    Operation("bori", register, value,    f = _ | _),
    Operation("setr", register,           f = (x, _) => x),
    Operation("seti", value,              f = (x, _) => x),
    Operation("gtir", value,    register, f = bool(_ > _)),
    Operation("gtri", register, value,    f = bool(_ > _)),
    Operation("gtrr", register, register, f = bool(_ > _)),
    Operation("eqir", value,    register, f = bool(_ == _)),
    Operation("eqri", register, value,    f = bool(_ == _)),
    Operation("eqrr", register, register, f = bool(_ == _)),
  )


  case class Example(before: Memory, opcode: Int, a: Int, b: Int, c: Int, after: Memory)

  def toExample(lines: Iterator[String]): Option[Example] = {
    def parse(prefix: String, sep: String, suffix: String)(line: String): Vector[Int] =
      line.stripPrefix(prefix).stripSuffix(suffix).split(sep).toVector.map(_.toInt)

    val before = lines.next()
    if (before startsWith "Before") Some {
      val Vector(code, a, b, c) = parse("", " ", "")(lines.next())
      val after = lines.next()
      lines.next()
      Example(
        parse("Before: [", ", ", "]")(before),
        code, a, b, c,
        parse("After:  [", ", ", "]")(after)
      )
    }
    else None
  }

  def verify(example: Example)(operation: Operation): Boolean =
    operation(example.a, example.b, example.c)(example.before) == example.after

  locally {
    val test =
      """
        |Before: [3, 2, 1, 1]
        |9 2 1 2
        |After:  [3, 2, 2, 1]
        |.
      """.trim.stripMargin.lines
    val e = toExample(test)
    e shouldBe Some(Example(Vector(3, 2, 1, 1), 9, 2, 1, 2, Vector(3, 2, 2, 1)))

    Operations.filter(verify(e.get)).map(_.name) shouldBe Set("mulr", "addi", "seti")
  }

  def evaluateOpcodes(examples: Seq[Example]): Map[Int, Operation] = {
    @tailrec
    def resolve(unresolved: Map[Int, Set[Operation]], resolved: Map[Int, Operation] = Map.empty): Map[Int, Operation] =
      if (unresolved.isEmpty) resolved
      else if (unresolved.exists(_._2.isEmpty)) sys.error(s"Impossible opcodes: $unresolved")
      else {
        val newResolved = unresolved.collect {
          case (code, set) if set.size == 1 => code -> set.head
        }
        val resolvedOps = newResolved.values.toSet
        resolve(
          unresolved = (unresolved -- newResolved.keys).transform((_, options) => options &~ resolvedOps),
          resolved = resolved ++ newResolved
        )
      }

    val initial = (0 until Operations.size).map(_ -> Operations).toMap

    resolve(examples.foldLeft(initial) { (current, example) =>
      val possible = Operations filter verify(example)
      current.transform((code, possibleBefore) =>
        if (code == example.opcode) possibleBefore & possible
        else possibleBefore
      )
    })
  }

  def run(instructions: Map[Int, Operation])(program: Seq[Vector[Int]], memory: Memory = Vector(0,0,0,0)): Memory =
    program.foldLeft(memory) { case (mem, Vector(opcode, a, b, c)) =>
      instructions(opcode)(a, b, c)(mem)
    }

  override type Input = (Seq[Example], Seq[Vector[Int]])

  override def input: Input = {
    val lines = inputLines.iterator
    val examples = Iterator.continually(toExample(lines)).takeWhile(_.isDefined).toVector.flatten
    val program = lines.filter(_.nonEmpty).map(_.split(" ").toVector.map(_.toInt)).toVector
    (examples, program)
  }

  override def answer1: Answer = _._1.count(ex => Operations.count(verify(ex)) >= 3)

  override def answer2: Answer = { case (examples, program) => run(evaluateOpcodes(examples))(program) }
}
