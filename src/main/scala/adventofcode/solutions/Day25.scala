package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day25 = Day(25) { (input, part) =>

  val patterns = input.split(lineSeparator * 2).map(_.split(lineSeparator))
    .map(_.toIndexedSeq.map(_.map {
      case '#' => true
      case '.' => false
    }.toIndexedSeq)).toSeq

  val top = patterns.filter(_.head.forall(identity))
  val bottom = patterns.filter(_.last.forall(identity)).map(_.transpose.map(_.reverse).transpose)

  val patternHeight = patterns.head.size

  def numbers(pattern: IndexedSeq[IndexedSeq[Boolean]]): IndexedSeq[Int] =
    pattern.transpose.map(_.takeWhile(identity).size)

  val (topNumbers, bottomNumbers) = (top.map(numbers), bottom.map(numbers))

  val counts =
    for
      a <- topNumbers
      b <- bottomNumbers
    yield a.zip(b).forall(_ + _ <= patternHeight)

  part(1) = counts.count(identity)

  part(2) = ""

}
