package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.language.postfixOps

case object Day_19_Go_With_The_Flow extends Puzzle {

  type Memory = Array[Int]

  sealed trait Argument extends (Memory => Int)
  case class Register(reg: Int) extends Argument {
    override def apply(v1: Memory): Int = v1(reg)
  }
  case class Value(value: Int) extends Argument {
    override def apply(v1: Memory): Int = value
  }
  case object Ignore extends Argument {
    override def apply(v1: Memory): Int = 0
  }

  case class Operation(title: String, f: (Int, Int) => Int)

  object Operation {
    val Add = Operation("A + B", _ + _)
    val Mul = Operation("A * B", _ * _)
    val And = Operation("A & B", _ & _)
    val Or  = Operation("A | B", _ | _)
    val Set = Operation("A", (a, _) => a)
    val Gt  = Operation("A > B", (a, b) => if (a > b) 1 else 0)
    val Eq  = Operation("A == B", (a, b) => if (a == b) 1 else 0)

    type Arg = Int => Argument

    val OpCodes: Map[String, (Operation, Arg, Arg)] = {
      val register: Arg = r => Register(r)
      val value: Arg = v => Value(v)
      val ignore: Arg = _ => Ignore

      Map(
        "addr" -> (Add, register, register),
        "addi" -> (Add, register, value),
        "mulr" -> (Mul, register, register),
        "muli" -> (Mul, register, value),
        "banr" -> (And, register, register),
        "bani" -> (And, register, value),
        "borr" -> (Or,  register, register),
        "bori" -> (Or,  register, value),
        "setr" -> (Set, register, ignore),
        "seti" -> (Set, value,    ignore),
        "gtir" -> (Gt,  value,    register),
        "gtri" -> (Gt,  register, value),
        "gtrr" -> (Gt,  register, register),
        "eqir" -> (Eq,  value,    register),
        "eqri" -> (Eq,  register, value),
        "eqrr" -> (Eq,  register, register)
      )
    }
  }

  case class Instr(operation: Operation, A: Argument, B: Argument, result: Register) {
    val f: Memory => Int = {
      val op = operation.f
      (A, B) match {
        case (Value(a), Value(b)) => _ => op(a, b)
        case (Register(a), Value(b)) => m => op(m(a), b)
        case (Register(a), Register(b)) => m => op(m(a), m(b))
        case (Value(a), Register(b)) => m => op(a, m(b))
        case (Value(a), Ignore) => _ => op(a, 0)
        case (Register(a), Ignore) => m => op(m(a), 0)
      }
    }
    def apply(memory: Memory): Int = f(memory)
  }

  case class Ip(reg: Int)

  object Instr {
    private val IpFormat = """#ip (\d+)""".r
    private val OpFormat = """(\w+) (\d+) (\d+) (\d+)""".r

    def parseIp(line: String): Ip = line match {
      case IpFormat(reg) => Ip(reg.toInt)
    }

    def parseInstr(line: String): Instr = line match {
      case OpFormat(name, a, b, c) =>
        val (op, aF, bF) = Operation.OpCodes(name)
        Instr(op, aF(a.toInt), bF(b.toInt), Register(c.toInt))
    }

    def parseCode(lines: Iterator[String]): (Ip, Vector[Instr]) = (
      parseIp(lines.next()),
      lines.map(parseInstr).toVector
    )

    def displayArg(a: Argument, ip: Int): String = a match {
      case Register(`ip`) => "ip"
      case Register(r) => s"R$r"
      case Value(v) => v.toString
      case Ignore => "_"
      case _ => "?"
    }

    def display(i: Instr, ip: Int): String = i match {
      case Instr(op, _, _, res) =>
        f"${displayArg(res, ip)} := ${op.title.replace("A", displayArg(i.A, ip)).replace("B", displayArg(i.B, ip))}"
    }

  }

  case class Machine(
    code: Vector[Instr],
    ipRegister: Int = 0,
    memory: Memory = new Array[Int](6)
  ) {
    val instructions: Array[Instr] = {
      val Ip = Register(ipRegister)
      code.zipWithIndex.map {
        case (i @ Instr(_, Ip, Ip, _), ip) => i.copy(A = Value(ip), B = Value(ip))
        case (i @ Instr(_, Ip, _, _), ip) => i.copy(A = Value(ip))
        case (i @ Instr(_, _, Ip, _), ip) => i.copy(B = Value(ip))
        case (i, _) => i
      }.toArray
    }

    val endOfChunk: Array[Int] = code.indices.map(i => code.indexWhere(_.result.reg == ipRegister, i) match {
      case -1 => code.size - 1
      case pos => pos
    }).toArray

    val chunkFrom: Array[Array[Instr]] = endOfChunk.zipWithIndex.map {
      case (j, i) => instructions.slice(i, j + 1)
    }

    def update(reg: Int, value: Int): this.type = {
      memory(reg) = value
      this
    }

    def ip: Int = memory(ipRegister)

    @inline
    final def executeNext(): Unit = {
      val ip = this.ip
      if (ip < code.size) {
        memory(ipRegister) = endOfChunk(ip)
        val chunk = chunkFrom(ip)
        var i = 0
        while (i < chunk.length) {
          val ins = chunk(i)
          memory(ins.result.reg) = ins.f(memory)
          i += 1
        }
        memory(ipRegister) += 1
      }
    }

    def tick(): Unit = code.lift(ip).foreach { instr =>
      memory(instr.result.reg) = instr(memory)
      memory(ipRegister) += 1
    }

    def debug: String = {
      val next = code.lift(ip).map { instr =>
        s"${Instr.display(instr, memory(ipRegister))}"
      }
      s"ip=${memory(ipRegister)} ${memory.mkString("[", ", ", "]")} ${next getOrElse ""}"
    }

    def run(): this.type = {
      var last = System.currentTimeMillis()
      var count = 0L
      while (memory(ipRegister) < instructions.length) {
        executeNext()
        count += 1
        if (count % 100000000 == 0) {
          val perSecond = 100000000L * 1000 / (System.currentTimeMillis() - last)
          Debug(f"$count%,d ticks, $perSecond%,d per second")
          last = System.currentTimeMillis()
        }
      }
      this
    }

    def displayCode: Seq[String] = code.zipWithIndex.map {
      case (i, ip) => f"$ip%2d: ${Instr.display(i, ipRegister)}"
    }
  }

  locally {
    val test =
      """
        |#ip 0
        |seti 5 0 1
        |seti 6 0 2
        |addi 0 1 0
        |addr 1 2 3
        |setr 1 0 0
        |seti 8 0 4
        |seti 9 0 5
      """.trim.stripMargin.lines

    val (ip, code) = Instr.parseCode(test)

    val machine = Machine(code, ip.reg)
    machine.run()
    machine.memory(0) shouldBe 7
  }

  override type Input = Machine

  override def input: Input = Instr.parseCode(inputLines.iterator) match { case (ip, code) => Machine(code, ip.reg) }

  override def answer1: Answer = _.run().memory(0)

  override def answer2: Answer = { m =>
    m.update(0, 1)
    while (m.memory(m.ipRegister) != 1)
      m.executeNext()
    val n = m.memory(2)
    (1 to n).filter(n % _ == 0).sum
  }
}
