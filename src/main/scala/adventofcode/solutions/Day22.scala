package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day22 = Day(22) { (input, part) =>

  val initialSecrets = input.toLines.map(_.toLong)

  def mix(value: Long, secret: Long): Long = value ^ secret
  def prune(secret: Long): Long = secret % 16777216

  def nextSecret(secret: Long): Long =
    val s1 = prune(mix(secret * 64, secret))
    val s2 = prune(mix(s1 / 32, s1))
    prune(mix(s2 * 2048, s2))

  val window = 2000

  def secrets(secret: Long): Seq[Long] = Seq.iterate(secret, window + 1)(nextSecret)

  part(1) = initialSecrets.map(secrets).map(_.last).sum

  val patternSize = 4

  def patternsMap(secret: Long): Map[Seq[Long], Long] =
    val prices = secrets(secret).map(_ % 10)
    val differences = prices.tail.zip(prices).map(_ - _)
    val pricesWithPatterns = prices.drop(patternSize).zip(differences.sliding(patternSize))
    pricesWithPatterns.groupBy((_, pattern) => pattern).view.mapValues(_.view.map((price, _) => price).head).toMap

  val allPatternsMaps = initialSecrets.map(patternsMap)
  val allPatterns = allPatternsMaps.flatMap(_.keySet).toSet

  part(2) = allPatterns.map(pattern => allPatternsMaps.flatMap(_.get(pattern)).sum).max

}
