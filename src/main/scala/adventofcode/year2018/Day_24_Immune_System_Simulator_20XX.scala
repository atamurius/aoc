package adventofcode.year2018

import adventofcode.common.Puzzle

case object Day_24_Immune_System_Simulator_20XX extends Puzzle {

  case class Group(
    var units: Int,
    hitPoints: Int,
    weakTo: Set[String],
    immuneTo: Set[String],
    damage: Int,
    attackType: String,
    initiative: Int
  ) {
    def isAlive: Boolean = units > 0

    def effectivePower: Int = units * damage

    def damageBy(attacher: Group): Int =
      if (immuneTo(attacher.attackType)) 0
      else if (weakTo(attacher.attackType)) attacher.effectivePower * 2
      else attacher.effectivePower

    def killedBy(attacker: Group): Int = damageBy(attacker) / hitPoints min units
  }

  object Group {
    private val Format =
      """(\d+) units each with (\d+) hit points (\(.*\))?\s*with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r

    def parse: String => Group = {
      case Format(units, hp, spec, damage, kind, init) =>
        val specs = Option(spec).toSeq.flatMap(_ stripPrefix "(" stripSuffix ")" split "; ")
        val weak = specs.filter(_ startsWith "weak").flatMap(_ stripPrefix "weak to " split ", ")
        val immune = specs.filter(_ startsWith "immune").flatMap(_ stripPrefix "immune to " split ", ")
        Group(
          units = units.toInt,
          hitPoints = hp.toInt,
          weakTo = weak.toSet,
          immuneTo = immune.toSet,
          damage = damage.toInt,
          attackType = kind,
          initiative = init.toInt
        )
    }
  }

  case class Fight(
    immune: Seq[Group],
    infection: Seq[Group]
  ) {
    def boost(b: Int): Fight = Fight(
      immune.map(g => g.copy(damage = g.damage + b, units = g.units)).toVector,
      infection.map(g => g.copy(units = g.units)).toVector
    )

    def debug(): Unit = {
      Debug("Immune System:")
      immune.foreach(g => Debug(s"${if (g.isAlive) "  " else "- "} $g"))
      Debug("Infection:")
      infection.foreach(g => Debug(s"${if (g.isAlive) "  " else "- "} $g"))
      Debug("")
    }

    def winner: Option[Seq[Group]] =
      if (immuneWins) Some(immune)
      else if (infectionWins) Some(infection)
      else None

    def immuneWins: Boolean = !infection.exists(_.isAlive)
    def infectionWins: Boolean = !immune.exists(_.isAlive)

    def selectTargets: Map[Group, Group] = selectTargets(immune, infection) ++ selectTargets(infection, immune)

    def selectTargets(attackers: Seq[Group], defenders: Seq[Group]): Map[Group, Group] = {
      attackers
        .filter(_.isAlive)
        .sorted(AttackerSelectorOrder)
        .foldLeft(Map.empty[Group, Group]) { (attack, attacker) =>
          defenders
            .filterNot(attack.valuesIterator.toSet)
            .filter(_.isAlive)
            .sorted(defenderSelectorOrder(attacker))
            .find(_.damageBy(attacker) > 0)
            .fold(attack) { defender =>
              attack + (attacker -> defender)
            }
        }
    }

    def attackOrder: Seq[(Group, Group)] = selectTargets.toSeq.sortBy(_._1)(orderByLargest(_.initiative))

    def runToWinner: Option[Seq[Group]] = {
      while (winner.isEmpty && attack() > 0) { }
      winner
    }

    def attack(debug: Boolean = false): Int = {
      val res = attackOrder.foldLeft(0) {
        case (killed, (attacker, _)) if !attacker.isAlive => killed
        case (killed, (attacker, defender)) =>

          def title(g: Group) = {
            val inf = infection.indexOf(g)
            val imm = immune.indexOf(g)
            if (inf == -1) s"Immune System group ${imm + 1}" else s"Infection group ${inf + 1}"
          }

          if (debug) {
            Debug(
              s"""
                 |${title(attacker)}
                 | attacks ${title(defender)}
                 | causing ${defender damageBy attacker} damage,
                 | killing ${defender killedBy attacker}
                 |""".trim.stripMargin.replaceAll("\\s+", " "))
          }
          val k = defender.killedBy(attacker)
          defender.units -= k
          killed + k
      }
      if (debug) Debug("")
      res
    }
  }

  def orderByLargest[T: Ordering](f: Group => T): Ordering[Group] = Ordering.by(f).reverse

  private val AttackerSelectorOrder = Ordering.comparatorToOrdering {
    orderByLargest(_.effectivePower) thenComparing orderByLargest(_.initiative)
  }

  private def defenderSelectorOrder(attacker: Group) = Ordering.comparatorToOrdering {
    orderByLargest(_.damageBy(attacker)) thenComparing
      orderByLargest(_.effectivePower) thenComparing
      orderByLargest(_.initiative)
  }

  def parseGroups(lines: Seq[String]): Fight = Fight(
    lines.dropWhile(_ != "Immune System:").drop(1).takeWhile(_.nonEmpty).map(Group.parse),
    lines.dropWhile(_ != "Infection:").drop(1).takeWhile(_.nonEmpty).map(Group.parse)
  )

  locally {
    val example =
      """
        |Immune System:
        |17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2
        |989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3
        |
        |Infection:
        |801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1
        |4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4
      """.trim.stripMargin.lines.toSeq

    val fight = parseGroups(example)
    val boosted = fight.boost(1570)
//    while (!fight.immuneWins && !fight.infectionWins) {
//      fight.debug()
//      fight.attack()
//    }
//    fight.debug()
    fight.runToWinner.map(_.map(_.units).sum) shouldBe Some(5216)

    boosted.runToWinner.map(_.map(_.units).sum) shouldBe Some(51)
  }

  override type Input = Fight

  override def input: Input = parseGroups(inputLines.toSeq)

  override def answer1: Answer = _.runToWinner.get.map(_.units).sum

  override def answer2: Answer = { f =>
    Iterator.from(1).flatMap { boost =>
      val b = f.boost(boost)
      b.runToWinner.map(_ => b)
    }.find(_.immuneWins).flatMap(_.winner).get.map(_.units).sum
  }
}
