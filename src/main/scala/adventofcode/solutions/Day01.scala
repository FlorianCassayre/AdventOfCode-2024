package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day01 = Day(1) { (input, part) =>

  val (left, right) = input.toLines.map { case s"$a   $b" => (a.toInt, b.toInt) }.unzip

  part(1) = left.sorted.zip(right.sorted).map(_ - _).map(_.abs).sum

  val counts = right.groupBy(identity).view.mapValues(_.size).toMap.withDefaultValue(0)

  part(2) = left.zip(left.map(counts)).map(_ * _).sum

}
