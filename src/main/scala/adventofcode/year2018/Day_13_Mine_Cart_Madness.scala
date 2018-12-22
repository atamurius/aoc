package adventofcode.year2018

import adventofcode.common.Puzzle

import scala.collection.Iterator.iterate

case object Day_13_Mine_Cart_Madness extends Puzzle {

  private val test =
    """
      #/->-\
      #|   |  /----\
      #| /-+--+-\  |
      #| | |  | v  |
      #\-+-/  \-+--/
      #  \------/
    """.trim.stripMargin('#').lines.toSeq

  type Pos = (Int, Int)

  implicit class PosOps(val self: Pos) extends AnyVal {
    def + (delta: Pos): Pos = (self._1 + delta._1, self._2 + delta._2)
    def - (delta: Pos): Pos = (self._1 - delta._1, self._2 - delta._2)
    def unary_- : Pos = (-self._1, -self._2)
  }

  val Left: Pos = (-1, 0)
  val Right: Pos = (1, 0)
  val Up: Pos = (0, -1)
  val Down: Pos = (0, 1)

  val turns: Map[Pos, Vector[Pos]] = {
    val all = Vector(Left, Up, Right, Down)
    all.zipWithIndex.toMap.transform((_, i) => all.drop(i + 1) ++ all.take(i))
  }

  turns(Left) shouldBe Vector(Up, Right, Down)
  turns(Right) shouldBe Vector(Down, Left, Up)

  case class Agent(pos: Pos, direction: Pos, choice: Int = 0) {
    def char: Char = direction match {
      case Left => '<'
      case Right => '>'
      case Up => '^'
      case Down => 'v'
    }

    def next: Pos = pos + direction

    def goBy(edge: Char): Agent = edge match {
      case '-'|'|' => copy(pos = next)
      case '/' => copy(pos = next, direction = direction match {
        case Left => Down
        case Up => Right
        case Right => Up
        case Down => Left
      })
      case '\\' => copy(pos = next, direction = direction match {
        case Left => Up
        case Up => Left
        case Right => Down
        case Down => Right
      })
      case '+' => copy(
        pos = next,
        direction = turns(-direction)(choice),
        choice = (choice + 1) % 3
      )
    }
  }

  Agent((0,0), Right) goBy '-' shouldBe Agent((1,0), Right)
  Agent((0,0), Down)  goBy '|' shouldBe Agent((0,1), Down)
  Agent((0,0), Right) goBy '/' shouldBe Agent((1,0), Up)
  Agent((0,0), Right) goBy '+' shouldBe Agent((1,0), Up, choice = 1)
  Agent((0,0), Right, choice = 1) goBy '+' shouldBe Agent((1,0), Right, choice = 2)
  Agent((0,0), Right, choice = 2) goBy '+' shouldBe Agent((1,0), Down)


  def toAgent(pos: Pos, char: Char): Option[Agent] = char match {
    case '>' => Some(Agent(pos, Right))
    case '<' => Some(Agent(pos, Left))
    case '^' => Some(Agent(pos, Up))
    case 'v' => Some(Agent(pos, Down))
    case  _  => None
  }

  def toField(lines: Iterable[String]): Field = {
    val edges = for {
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.iterator.zipWithIndex if c != ' '
    } yield (x,y) -> (c match {
      case '>'|'<' => '-'
      case '^'|'v' => '|'
      case other => other
    })
    val agents = for {
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.iterator.zipWithIndex
      agent <- toAgent((x,y), c)
    } yield agent
    Field(edges.toMap, agents.toVector)
  }

  case class Field(edges: Map[Pos, Char], agents: Vector[Agent]) {
    val agentPos: Map[Pos, Vector[Agent]] = agents.groupBy(_.pos)

    val collision: Option[Pos] = agentPos.find(_._2.size > 1).map(_._1)

    def debug(): this.type = {
      val left: Int = edges.keys.map(_._1).min
      val top: Int = edges.keys.map(_._2).min
      val right: Int = edges.keys.map(_._1).max
      val bottom: Int = edges.keys.map(_._2).max
      Disabled(s"Collision: $collision")
      for (y <- top to bottom) {
        val line = for (x <- left to right) yield {
          val pos = (x, y)
          if (agentPos contains pos) agentPos(pos) match {
            case Seq(single) => single.char
            case _ => 'x'
          }
          else edges.getOrElse(pos, ' ')
        }
        Disabled(line.mkString)
      }
      this
    }

    def withoutCollisions: Field = copy(agents = agentPos.values.collect { case Seq(single) => single }.toVector)

    def simulationStep: Field = {
      val step = agents.foldLeft(agentPos) { (ps, agent) =>
        if (ps(agent.pos).size > 1) ps // collided, no changes
        else {
          val next = agent goBy edges(agent.next)
          val nextPos = ps.getOrElse(next.pos, Vector.empty) :+ next
          ps - agent.pos + (next.pos -> nextPos)
        }
      }
      copy(agents = step.values.flatten.toVector)
    }
  }

  def simulateUntilCollision(f: Field): Field = iterate(f)(_.simulationStep).dropWhile(_.collision.isEmpty).next()

  simulateUntilCollision(toField(test)).collision.get shouldBe(7, 3)
  simulateUntilCollision(toField(Seq("><"))).collision.get shouldBe (1,0)

  def simulateUntilOne(f: Field): Field = iterate(f)(_.simulationStep.withoutCollisions).dropWhile(_.agents.size > 1).next()

  private val test2 =
    """
      #/>-<\
      #|   |
      #| /<+-\
      #| | | v
      #\>+</ |
      #  |   ^
      #  \<->/
    """.trim.stripMargin('#').lines.toSeq

  simulateUntilOne(toField(test2)).agents.head.pos shouldBe (6,4)

  override type Input = Field

  override def input: Input = toField(inputLines)

  override def answer1: Answer = simulateUntilCollision(_).collision.get

  override def answer2: Answer = simulateUntilOne(_).agents.head.pos
}
