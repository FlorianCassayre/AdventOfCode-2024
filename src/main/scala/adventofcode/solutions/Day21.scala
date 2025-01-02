package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day21 = Day(21) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    infix def -(that: Vec): Vec = Vec(i - that.i, j - that.j)
    def manhattan: Int = i.abs + j.abs

  def parseLayout(s: String): Map[Char, Vec] =
    s.split(lineSeparator).zipWithIndex.flatMap((row, i) => row.zipWithIndex.collect { case (c, j) if c != ' ' => c -> Vec(i, j) }).toMap

  val layout1 = parseLayout(
    """789
      |456
      |123
      | 0A
      |""".stripMargin
  )

  val layout2 = parseLayout(
    """ ^A
      |<v>
      |""".stripMargin
  )

  val directions = Map(
    '^' -> Vec(-1, 0),
    '<' -> Vec(0, -1),
    'v' -> Vec(1, 0),
    '>' -> Vec(0, 1)
  )
  val directionsInverse = directions.toSeq.map(_.swap).toMap

  val A = 'A'

  def isValidPath(position: Vec, path: Seq[Vec], validPositions: Set[Vec]): Boolean =
    if validPositions.contains(position) then
      path match
        case head +: tail => isValidPath(position + head, tail, validPositions)
        case _ => true
    else
      false

  def optimalPaths(from: Vec, to: Vec): Set[Seq[Vec]] =
    val d = to - from
    val path = Seq(Vec(d.i, 0), Vec(0, d.j)).map(v => Vec(v.i.sign, v.j.sign) -> (v.i.abs + v.j.abs)).filter((_, d) => d != 0)
    Seq(path, path.reverse).toSet.map(_.flatMap((vec, count) => Seq.fill(count)(vec)))

  def buildLevelMap(layout: Map[Char, Vec]): Map[(Char, Char), Set[Seq[Vec]]] =
    val validPositions = layout.values.toSet
    (for
      a <- layout.keys
      b <- layout.keys
      (pa, pb) = (layout(a), layout(b))
      paths = optimalPaths(pa, pb).filter(isValidPath(pa, _, validPositions))
    yield (a, b) -> paths).toMap

  val (digitsLevelMap, arrowsLevelMap) = (buildLevelMap(layout1), buildLevelMap(layout2))

  type Cache = Map[(Char, Char, Int), Long]

  def count(level: Map[(Char, Char), Set[Seq[Vec]]], start: Char, end: Char, depth: Int, cache: Cache): (Long, Cache) =
    val cacheKey = (start, end, depth)
    cache.get(cacheKey) match
      case Some(value) => (value, cache)
      case _ =>
        if depth >= 0 then
          val paths = level((start, end)).map(_.map(directionsInverse) :+ A)
          val (values, cache3) = paths.foldLeft((Seq.empty[Long], cache)) { case ((costs, cache1), path) =>
            val (c, cache2) = countAll(arrowsLevelMap, path, depth - 1, cache1)
            (costs :+ c, cache2)
          }
          val value = values.min
          (value, cache3 + (cacheKey -> value))
        else
          (1, cache)

  def countAll(level: Map[(Char, Char), Set[Seq[Vec]]], keys: Seq[Char], depth: Int, cache: Cache): (Long, Cache) =
    (A +: keys)
      .sliding(2).collect { case Seq(start, end) => (start, end) }
      .foldLeft((0L, cache)) { case ((sum, cache1), (start, end)) =>
        val (result, cache2) = count(level, start, end, depth, cache1)
        (sum + result, cache2)
      }

  def solve(robots: Int): Long =
    input.toLines.map(code => countAll(digitsLevelMap, code, robots, Map.empty)._1 * code.init.mkString.toInt).sum

  part(1) = solve(2)

  part(2) = solve(25)

}
