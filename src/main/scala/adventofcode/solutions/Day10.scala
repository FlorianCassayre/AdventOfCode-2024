package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day10 = Day(10) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)

  val grid = input.toLines.map(_.map(_.asDigit))

  val zeros = grid.indices.flatMap(i => grid(i).indices.filter(j => grid(i)(j) == 0).map(j => Vec(i, j)))

  val adjacent =
    for
      i <- -1 to 1
      j <- -1 to 1
      if i.abs + j.abs == 1
    yield Vec(i, j)

  def inBounds(vec: Vec): Boolean = grid.indices.contains(vec.i) && grid(vec.i).indices.contains(vec.j)

  def explore(nodes: Iterable[Vec], i: Int): Int =
    if i < 10 then
      val nextNodes = nodes
        .flatMap(n => adjacent.map(n + _))
        .filter(inBounds)
        .filter(v => grid(v.i)(v.j) == i)
      explore(nextNodes, i + 1)
    else
      nodes.size

  def count(factory: Vec => Iterable[Vec]): Int = zeros.map(zero => explore(factory(zero), 1)).sum

  part(1) = count(Set.apply(_))

  part(2) = count(Seq.apply(_))

}
