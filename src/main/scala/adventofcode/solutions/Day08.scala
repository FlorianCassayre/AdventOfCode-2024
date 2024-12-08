package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day08 = Day(8) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    infix def -(that: Vec): Vec = Vec(i - that.i, j - that.j)
    infix def *(v: Int): Vec = Vec(i * v, j * v)

  val grid = input.toLines.map(_.map {
    case '.' => None
    case c => Some(c)
  })
  val frequencies = grid.zipWithIndex
    .flatMap((row, i) => row.zipWithIndex.collect { case (Some(c), j) => c -> Vec(i, j) })
    .groupBy((f, _) => f).view.mapValues(_.map((_, p) => p)).toMap

  def antennasCount(range: Range) =
    val set =
      for
        f <- frequencies.keySet
        pa <- frequencies(f)
        pb <- frequencies(f)
        if pa != pb
        v = pa - pb
        k <- range
        p = pa + v * k
        if grid.indices.contains(p.i) && grid(p.i).indices.contains(p.j)
      yield p
    set.size

  part(1) = antennasCount(1 to 1)

  part(2) = antennasCount(0 to Math.max(grid.size, grid.head.size))

}
