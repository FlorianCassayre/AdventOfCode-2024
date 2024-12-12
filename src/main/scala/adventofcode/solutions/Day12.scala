package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day12 = Day(12) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    infix def -(that: Vec): Vec = Vec(i - that.i, j - that.j)
    infix def rotateCW: Vec = Vec(-j, i)

  val grid = input.toLines

  val adjacent =
    for
      i <- -1 to 1
      j <- -1 to 1
      if i.abs + j.abs == 1
    yield Vec(i, j)

  def inBounds(vec: Vec): Boolean = grid.indices.contains(vec.i) && grid(vec.i).indices.contains(vec.j)

  def bfsAll[T](nodes: Seq[T], adjacency: T => Set[T]): Set[Set[T]] =
    def bfs(current: Set[T], visited: Set[T], excluded: Set[T]): Set[T] =
      if current.nonEmpty then
        val nextVisited = visited ++ current
        val nextCurrent = current.flatMap(adjacency).diff(nextVisited).diff(excluded)
        bfs(nextCurrent, nextVisited, excluded)
      else
        visited
    val (result, _) = nodes.foldLeft((Set.empty[Set[T]], Set.empty[T])) { case ((regions, visited), node) =>
      if !visited.contains(node) then
        val sideRegion = bfs(Set(node), Set.empty, visited)
        (regions + sideRegion, visited ++ sideRegion)
      else
        (regions, visited + node)
    }
    result

  def area(region: Set[Vec]): Int = region.size

  def perimeter(region: Set[Vec]): Int =
    region.toSeq.flatMap(p => adjacent.map(p + _)).count(!region.contains(_))

  def sides(region: Set[Vec]): Int =
    val allSides = region.toSeq.flatMap(p => adjacent.map(p + _).filter(!region.contains(_)).map(p -> _))
    val sidesGraph = allSides.map { side =>
      val (a, b) = side
      side -> Set((a - b).rotateCW, (b - a).rotateCW).map(v => (a + v, b + v))
    }.toMap
    bfsAll(allSides, sidesGraph.withDefaultValue(Set.empty)).size

  val regions = bfsAll(
    grid.indices.flatMap(i => grid(i).indices.map(j => Vec(i, j))),
    p => {
      val region = grid(p.i)(p.j)
      adjacent.map(p + _).filter(inBounds).filter(p => grid(p.i)(p.j) == region).toSet
    }
  )

  def cost(f: Set[Vec] => Int): Int = regions.toSeq.map(r => area(r) * f(r)).sum

  part(1) = cost(perimeter)

  part(2) = cost(sides)

}
