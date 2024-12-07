package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day07 = Day(7) { (input, part) =>

  val equations = input.toLines.map { case s"$result: $numbers" => (result.toLong, numbers.split(" ").map(_.toLong).toSeq) }

  type Operators = Seq[(Long, Long) => Long]

  def valid(operators: Operators)(target: Long, numbers: Seq[Long]): Boolean = numbers match
    case Seq(v) => v == target
    case a +: _ if a > target => false
    case a +: b +: rest => operators.view.map(_(a, b) +: rest).exists(valid(operators)(target, _))

  def solve(operators: Operators): Long = equations.filter(valid(operators).tupled).map((r, _) => r).sum

  val baseOperators: Operators = Seq(_ + _, _ * _)

  part(1) = solve(baseOperators)

  part(2) = solve(baseOperators :+ ((a, b) => s"$a$b".toLong))

}
