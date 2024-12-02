package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day02 = Day(2) { (input, part) =>

  val values = input.toLines.map(_.split(" ").map(_.toInt).toSeq)

  def isSafe(report: Seq[Int]): Boolean =
    val differences = report.zip(report.tail).map(_ - _)
    differences.map(_.sign).distinct.sizeIs == 1 && differences.map(_.abs).forall((1 to 3).contains)

  part(1) = values.count(isSafe)

  part(2) = values.count(v => (v +: v.indices.map(i => v.take(i) ++ v.drop(i + 1))).exists(isSafe))

}
