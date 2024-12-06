package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day06 = Day(6) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    def turnRight: Vec = Vec(j, -i)

  val (grid, initialPosition) =
    val matrix = input.toLines
    matrix.map(_.map {
      case '#' => false
      case _ => true
    }) -> matrix.indices.flatMap(i => matrix(i).indices.collect { case j if matrix(i)(j) == '^' => Vec(i, j) }).head
  val initialDirection = Vec(-1, 0)

  def inBounds(vec: Vec): Boolean = grid.indices.contains(vec.i) && grid(vec.i).indices.contains(vec.j)

  def visit(position: Vec, direction: Vec, history: Set[(Vec, Vec)], grid: IndexedSeq[IndexedSeq[Boolean]]): Option[Set[(Vec, Vec)]] =
    val key = (position, direction)
    if history.contains(key) then
      None
    else
      val nextHistory = history + key
      val nextCandidatePosition = position + direction
      if inBounds(nextCandidatePosition) then
        if grid(nextCandidatePosition.i)(nextCandidatePosition.j) then
          visit(nextCandidatePosition, direction, nextHistory, grid)
        else
          visit(position, direction.turnRight, nextHistory, grid)
      else
        Some(nextHistory)

  val explored = visit(initialPosition, initialDirection, Set.empty, grid).get

  part(1) = explored.map((p, _) => p).size

  val candidates = explored.map(_ + _).filter(inBounds).filter(p => grid(p.i)(p.j))

  part(2) = candidates.count(p => visit(initialPosition, initialDirection, Set.empty, grid.updated(p.i, grid(p.i).updated(p.j, false))).isEmpty)

}
