package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day23 = Day(23) { (input, part) =>

  val graph =
    val edges = input.toLines.map { case s"$a-$b" => a -> b }.flatMap(e => Seq(e, e.swap))
    edges.groupBy((a, _) => a).view.mapValues(_.map((_, b) => b).toSet).toMap
  val nodes = graph.keys.toSeq

  val cliques3 =
    for
      (a, i) <- nodes.zipWithIndex
      (b, j) <- nodes.zipWithIndex.drop(i + 1)
      c <- nodes.drop(j + 1)
      if graph(a).contains(b) && graph(a).contains(c) && graph(b).contains(c)
    yield Set(a, b, c)

  part(1) = cliques3.count(_.exists(_.startsWith("t")))

  def bronKerbosch(r: Set[String], p: Set[String], x: Set[String]): Set[Set[String]] =
    if p.isEmpty && x.isEmpty then
      Set(r)
    else
      val (_, _, result) = p.foldLeft((p, x, Set.empty[Set[String]])) { case ((p1, x1, acc), v) =>
        val nextAcc = acc ++ bronKerbosch(r + v, p1.intersect(graph(v)), x1.intersect(graph(v)))
        val (nextP, nextX) = (p1 - v, x1 + v)
        (nextP, nextX, nextAcc)
      }
      result

  val maxClique = bronKerbosch(Set.empty, nodes.toSet, Set.empty).maxBy(_.size)

  part(2) = maxClique.toSeq.sorted.mkString(",")

}
