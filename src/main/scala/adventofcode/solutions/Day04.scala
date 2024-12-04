package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day04 = Day(4) { (input, part) =>

  val grid = input.toLines.map(_.toIndexedSeq)

  part(1) =
    val r = "XMAS".r
    val transpositions = Seq(grid, grid.transpose)
    def count(seq: Seq[IndexedSeq[IndexedSeq[Char]]]): Int =
      seq.flatMap(_.flatMap(t => Seq(t, t.reverse).map(s => r.findAllMatchIn(s.mkString).size))).sum
    val linear = count(transpositions)
    val diagonal = count(
      Seq(false, true).map(o => grid.zipWithIndex.map((r, i) => {
        val space = "."
        val (a, b) = (space * i, space * (grid.size - i - 1))
        val s = r.mkString
        if o then a + s + b else b + s + a
      }).transpose)
    )
    linear + diagonal

  part(2) =
    val sourcePattern =
      """M.S
        |.A.
        |M.S""".stripMargin.split("\n").map(_.map {
        case '.' => None
        case c => Some(c)
      }).toIndexedSeq
    val patterns = Seq(sourcePattern, sourcePattern.map(_.reverse)).flatMap(p => Seq(p, p.transpose))
    patterns.flatMap(p =>
      grid.indices.dropRight(p.size - 1).map(i =>
        grid(i).indices.dropRight(p.head.size - 1).indices.count(j =>
          p.indices.forall(i0 => p(i0).indices.forall(j0 => p(i0)(j0).forall(_ == grid(i + i0)(j + j0))))
        )
      )
    ).sum

}
