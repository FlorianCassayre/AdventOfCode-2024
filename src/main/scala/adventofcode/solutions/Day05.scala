package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day05 = Day(5) { (input, part) =>

  val (ordering, updates) =
    val parts = input.split(lineSeparator * 2)
    parts.head.split(lineSeparator).map {
      case s"$a|$b" => (a.toInt, b.toInt)
    }.toIndexedSeq -> parts.last.split(lineSeparator).map(_.split(",").map(_.toInt).toIndexedSeq).toIndexedSeq

  val orderingMap = ordering.groupBy(_._1).view.mapValues(_.map(_._2).toSet).toMap.withDefaultValue(Set.empty)

  def isOrdered(seq: IndexedSeq[Int]): Boolean = seq.zip(seq.tail).forall((a, b) => orderingMap.get(b).forall(!_.contains(a)))
  def middle(seq: IndexedSeq[Int]): Int = seq(seq.size / 2)

  part(1) = updates.filter(isOrdered).map(middle).sum

  def topologicalSort(adjacency: Map[Int, Set[Int]]): IndexedSeq[Int] =
    def dfs(stack: Seq[(Int, Set[Int])], marks: Map[Int, Boolean], remaining: Set[Int], sorted: Seq[Int]): (Map[Int, Boolean], Set[Int], Seq[Int]) =
      stack match
        case (u, adjacent) +: tail =>
          adjacent.headOption match
            case Some(v) =>
              marks.get(v) match
                case Some(true) => dfs((u, adjacent.tail) +: tail, marks, remaining, sorted)
                case _ => dfs((v, adjacency.getOrElse(v, Set.empty)) +: (u, adjacent.tail) +: tail, marks + (v -> false), remaining, sorted)
            case None => dfs(tail, marks + (u -> true), remaining - u, u +: sorted)
        case _ => (marks, remaining, sorted)
    def iterate(marks: Map[Int, Boolean], remaining: Set[Int], sorted: Seq[Int]): Seq[Int] =
      if remaining.nonEmpty then
        val u = remaining.head
        val (newMarks, newRemaining, newSorted) = dfs(Seq((u, adjacency.getOrElse(u, Set.empty))), marks + (u -> false), remaining - u, sorted)
        iterate(newMarks, newRemaining, newSorted)
      else
        sorted
    iterate(Map.empty, adjacency.keySet ++ adjacency.values.flatten, Seq.empty).toIndexedSeq

  part(2) = updates
    .filterNot(isOrdered)
    .map(as => topologicalSort(as.map(k => k -> orderingMap(k).intersect(as.toSet)).toMap))
    .map(middle).sum

}
