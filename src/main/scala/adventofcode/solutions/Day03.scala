package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day03 = Day(3) { (input, part) =>

  val muls = "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)".r.findAllMatchIn(input).map(m =>
    if Option(m.group(1)).nonEmpty then Left((m.group(1).toInt, m.group(2).toInt)) else Right(m.group(0).length == 4)
  ).toSeq

  part(1) = muls.collect { case Left(a, b) => a * b }.sum

  part(2) = muls.foldLeft((0, true)) {
    case ((count, enabled), Left((a, b))) => (count + (if enabled then a * b else 0), enabled)
    case ((count, _), Right(newEnabled)) => (count, newEnabled)
  }._1

}
