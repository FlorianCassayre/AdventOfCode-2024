package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day20 = Day(20) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)

  val (grid, start, end) =
    val rawGrid = input.toLines.map(_.toIndexedSeq)
    def find(c: Char): Vec = rawGrid.view.zipWithIndex.flatMap((row, i) => row.view.zipWithIndex.collect { case (`c`, j) => Vec(i, j) }).head
    (rawGrid.map(_.map {
      case '#' => false
      case _ => true
    }), find('S'), find('E'))

  val adjacent =
    for
      i <- -1 to 1
      j <- -1 to 1
      if i.abs + j.abs == 1
    yield Vec(i, j)

  def inBounds(p: Vec): Boolean = grid.indices.contains(p.i) && grid(p.i).indices.contains(p.j)
  def walkable(p: Vec): Boolean = grid(p.i)(p.j)

  def bfs(current: Set[Vec], history: Map[Vec, Int], steps: Int): Map[Vec, Int] =
    val nextHistory = history ++ current.map(_ -> steps)
    if current.contains(end) then
      nextHistory
    else
      val nextCurrent = current.flatMap(p => adjacent.map(p + _)).filter(inBounds).filter(walkable).diff(history.keySet)
      bfs(nextCurrent, nextHistory, steps + 1)

  val distances = bfs(Set(start), Map.empty, 0)
  val distancesSeq = distances.toSeq

  def around(positions: Set[Vec], history: Map[Vec, Int], distance: Int, duration: Int): Map[Vec, Int] =
    val nextHistory = history ++ positions.diff(history.keySet).map(_ -> distance).toMap
    if distance < duration then
      val nextPositions = positions.diff(history.keySet).flatMap(p => adjacent.map(p + _))
      around(nextPositions, nextHistory, distance + 1, duration)
    else
      nextHistory

  def count100(duration: Int): Int =
    val arounds = around(Set(Vec(0, 0)), Map.empty, 0, duration).toSeq
    distancesSeq.map((p, d) => arounds.view.map((q, e) => (q + p, e)).count((q, e) => inBounds(q) && walkable(q) && distances(q) - d - e >= 100)).sum

  part(1) = count100(2)

  part(2) = count100(20)

}
