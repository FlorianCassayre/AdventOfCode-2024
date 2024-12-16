package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day16 = Day(16) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    infix def rotateCW: Vec = Vec(-j, i)
    infix def rotateCCW: Vec = Vec(j, -i)

  case class State(position: Vec, direction: Vec)

  val (grid, initialPosition, finalPosition) =
    val lines = input.toLines
    def find(c: Char): Vec = lines.zipWithIndex.flatMap((row, i) => row.zipWithIndex.collect { case (`c`, j) => Vec(i, j) }).head
    (lines.map(_.map {
      case '#' => false
      case _ => true
    }), find('S'), find('E'))

  val adjacent =
    for
      i <- -1 to 1
      j <- -1 to 1
      if i.abs + j.abs == 1
    yield Vec(i, j)

  def walkable(p: Vec): Boolean = grid(p.i)(p.j)

  val initialState = State(initialPosition, Vec(0, 1))

  def shortestPathDijkstra(adjacency: State => Set[(State, Int)], source: State, earlyStopping: Set[State] = Set.empty): (Map[State, Int], Map[State, Set[State]]) =
    import scala.collection.immutable.TreeSet
    def search(queue: TreeSet[(State, (Int, Int))], distances: Map[State, Int], predecessors: Map[State, Set[State]], visited: Set[State], k: Int): (Map[State, Int], Map[State, Set[State]]) =
      queue.headOption match
        case Some((u, (du, _))) =>
          if earlyStopping.contains(u) then
            (distances, predecessors)
          else if !visited.contains(u) then
            val newVisited = visited + u
            val edges = adjacency(u).filter { case (b, w) => !visited.contains(b) && (!distances.contains(b) || distances(b) > du + w) }
            val (newQueue, newDistances, newPredecessors, newK) = edges.foldLeft((queue.tail, distances, predecessors, k)) { case ((currentQueue, currentDistances, currentPredecessors, currentK), (v, w)) =>
              val newDistance = du + w
              val newPredecessors = currentPredecessors + (v -> (currentDistances.get(v).filter(_ > newDistance).flatMap(_ => currentPredecessors.get(v)).getOrElse(Set.empty) + u))
              (currentQueue + (v -> (newDistance, currentK)),
                currentDistances - v + (v -> newDistance),
                newPredecessors,
                currentK + 1)
            }
            search(newQueue, newDistances, newPredecessors, newVisited, newK)
          else
            search(queue.tail, distances, predecessors, visited, k)
        case None => (distances, predecessors)
    search(TreeSet((source, (0, 0)))(Ordering.by(_._2)), Map(source -> 0), Map.empty, Set.empty, 1)

  def adjacency(state: State): Set[(State, Int)] =
    Set(state.position + state.direction).filter(walkable).map(p => state.copy(position = p) -> 1) ++
      Set(state.direction.rotateCW, state.direction.rotateCCW).map(d => state.copy(direction = d) -> 1000)

  val finalStates = adjacent.map(d => State(finalPosition, d)).toSet

  val (distances, predecessors) = shortestPathDijkstra(adjacency, initialState, finalStates)

  part(1) = finalStates.flatMap(distances.get).head

  def reconstructPath(target: State, predecessors: Map[State, Set[State]]): Set[State] =
    def reconstruct(set: Set[State], acc: Set[State]): Set[State] =
      if set.nonEmpty then
        val newAcc = acc ++ set
        reconstruct(set.flatMap(predecessors.get).flatten.diff(newAcc), newAcc)
      else
        acc
    reconstruct(Set(target), Set.empty)

  part(2) = finalStates.filter(distances.contains).map(reconstructPath(_, predecessors)).head.size + 1

}
