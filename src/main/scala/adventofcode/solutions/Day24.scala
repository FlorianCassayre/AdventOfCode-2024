package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day24 = Day(24) { (input, part) =>

  enum Operator:
    case And
    case Or
    case Xor

    def apply(a: Boolean, b: Boolean): Boolean = this match
      case And => a && b
      case Or => a || b
      case Xor => a ^ b
  import Operator.*

  case class Connection(a: String, op: Operator, b: String, output: String):
    def apply(av: Boolean, bv: Boolean): Boolean = op(av, bv)

  val (initialWires, connections) = input.split(lineSeparator * 2).map(_.split(lineSeparator).toSeq).toSeq match
    case Seq(first, second) =>
      val wires = first.map { case s"$wire: $b" => wire -> Map(0 -> false, 1 -> true)(b.toInt) }
      val connections = second.map { case s"$a $op $b -> $output" =>
        val operator = op match
          case "AND" => And
          case "OR" => Or
          case "XOR" => Xor
        Connection(a, operator, b, output)
      }
      (wires.toMap, connections)

  val wires = (initialWires.map((w, _) => w) ++ connections.flatMap(c => Seq(c.a, c.b, c.output))).toSet

  def simulate(swaps: Map[String, String], initialState: Map[String, Option[Boolean]]): Long =
    val actualConnections = connections.map(c =>
      swaps.get(c.output) match
        case Some(newOutput) => c.copy(output = newOutput)
        case None => c
    )
    val connectionsByInput = actualConnections.flatMap(c => Seq(c.a, c.b).map(_ -> c)).groupBy((k, _) => k).view.mapValues(_.map((_, c) => c)).toMap.withDefaultValue(Seq.empty)
    def simulate(state: Map[String, Option[Boolean]], changes: Set[String], iterations: Int): Map[String, Option[Boolean]] =
      if changes.nonEmpty && iterations < 100 then
        val (newState, newChanges) = changes.foldLeft((state, Set.empty[String])) { case ((currentState, currentChanges), wire) =>
          val updates = connectionsByInput(wire).map { c =>
            (state(c.a), state(c.b)) match
              case (Some(av), Some(bv)) =>
                val output = c(av, bv)
                c.output -> Some(output)
              case _ => c.output -> None
          }.filter { case (key, _) => currentState(key).isEmpty }
          val newState = updates.foldLeft(currentState) { case (s, (key, value)) => s + (key -> value) }
          (newState, currentChanges ++ updates.map((k, _) => k))
        }
        simulate(newState, newChanges, iterations + 1)
      else
        if iterations >= 100 then initialState else state
    simulate(initialState, wires, 0).collect { case (s"z$b", Some(true)) => b.toInt }.toSeq.foldLeft(0L)((a, b) => a | (1L << b))

  val defaultInitialState = wires.map(w => w -> initialWires.get(w)).toMap

  part(1) = simulate(Map.empty, defaultInitialState)

  val inputSize = wires.count(_.startsWith("x"))

  val swapsCount = 4

  def createInitialState(x: Long, y: Long): Map[String, Option[Boolean]] =
    def bits(v: Long): Map[Int, Boolean] = (0 until inputSize).map(i => i -> (((v >> i) & 1) != 0)).toMap
    val partial = Seq("x" -> x, "y" -> y).flatMap((c, v) => bits(v).toSeq.map((k, v) => f"$c$k%02d" -> v)).toMap
    wires.map(w => w -> partial.get(w)).toMap

  part(2) = Map("z05" -> "bpf", "hcc" -> "z11", "hqc" -> "qcw", "z35" -> "fdw").toSeq.flatMap((a, b) => Seq(a, b)).sorted.mkString(",")

}
