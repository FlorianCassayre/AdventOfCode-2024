package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.View

@main def Day11 = Day(11) { (input, part) =>

  val initialStones = input.toLines.head.split(" ").map(_.toLong -> 1L).toMap

  def evolve(count: Int): Long =
    View.iterate(initialStones, count + 1)(_.toSeq.flatMap((value, count) =>
      value match
        case 0 => Seq(1L)
        case v if v.toString.length % 2 == 0 =>
          val (l, r) = v.toString.splitAt(v.toString.length / 2)
          Seq(l, r).map(_.toLong)
        case v => Seq(v * 2024)
      match
        case seq => seq.map(_ -> count)
    ).groupBy((k, _) => k).view.mapValues(_.map((_, c) => c).sum).toMap).last.values.sum

  part(1) = evolve(25)

  part(2) = evolve(75)

}
