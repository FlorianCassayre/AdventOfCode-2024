package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day18 = Day(18) { (input, part) =>

  case class Vec(x: Int, y: Int):
    infix def +(that: Vec): Vec = Vec(x + that.x, y + that.y)

  val corruptionsSeq = input.toLines.map { case s"$x,$y" => Vec(x.toInt, y.toInt) }

  val (initialPosition, finalPosition) = (Vec(0, 0), Vec(70, 70))

  def inBounds(p: Vec): Boolean = (initialPosition.x to finalPosition.x).contains(p.x) && (initialPosition.y to finalPosition.y).contains(p.y)

  val adjacent =
    for
      y <- -1 to 1
      x <- -1 to 1
      if x.abs + y.abs == 1
    yield Vec(x, y)

  def bfs(current: Set[Vec], visited: Set[Vec], steps: Int, corruptions: Set[Vec]): Option[Int] =
    if current.isEmpty then
      None
    else if current.contains(finalPosition) then
      Some(steps)
    else
      val nextVisited = visited ++ current
      val nextCurrent = current.flatMap(p => adjacent.map(p + _)).filter(inBounds).diff(corruptions).diff(nextVisited)
      bfs(nextCurrent, nextVisited, steps + 1, corruptions)

  part(1) = bfs(Set(initialPosition), Set.empty, 0, corruptionsSeq.take(1024).toSet).get

  def binarySearch(mapping: Int => Boolean, first: Int, last: Int): Int =
    def find(start: Int, end: Int): Int =
      val mid = (start + end) / 2
      if start == end then
        mid
      else
        if mapping(mid) then
          find(mid + 1, end)
        else
          find(start, mid - 1)
    find(first, last)

  val point = corruptionsSeq(binarySearch(i => bfs(Set(initialPosition), Set.empty, 0, corruptionsSeq.take(i).toSet).nonEmpty, 0, corruptionsSeq.size) - 1)

  part(2) = s"${point.x},${point.y}"

}
