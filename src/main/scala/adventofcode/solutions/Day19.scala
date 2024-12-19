package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day19 = Day(19) { (input, part) =>

  val (patterns, designs) = input.split(lineSeparator * 2).toSeq match
    case Seq(a, b) => (a.split(", ").toSeq, b.split(lineSeparator).toSeq)

  def countPossible(remaining: String, cache: Map[String, Long]): (Long, Map[String, Long]) =
    if !cache.contains(remaining) then
      if remaining.isEmpty then
        (1, cache)
      else
        val (count, newCache) = patterns.filter(remaining.startsWith).foldLeft((0L, cache)) { case ((accCount, accCache), p) =>
          val (newCount, newCache) = countPossible(remaining.substring(p.length, remaining.length), accCache)
          (accCount + newCount, newCache)
        }
        (count, newCache + (remaining -> count))
    else
      (cache(remaining), cache)

  val counts = designs.map(countPossible(_, Map.empty)._1)

  part(1) = counts.count(_ > 0)

  part(2) = counts.sum

}
