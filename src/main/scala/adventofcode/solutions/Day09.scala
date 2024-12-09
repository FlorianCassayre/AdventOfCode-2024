package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day09 = Day(9) { (input, part) =>

  val layout: IndexedSeq[Int] = input.toLines.head.map(_.asDigit)
  val files = layout.zipWithIndex.map((v, i) => (v, if i % 2 == 0 then Some(i / 2) else None))
  val memory = files.flatMap((v, e) => IndexedSeq.fill(v)(e))

  def move(memory: IndexedSeq[Option[Int]], i: Int, j: Int): IndexedSeq[Int] =
    if i < j then
      (memory(i), memory(j)) match
        case (None, None) => move(memory, i, j - 1)
        case (None, vj @ Some(_)) => move(memory.updated(i, vj).updated(j, None), i + 1, j - 1)
        case (Some(_), _) => move(memory, i + 1, j)
    else
      memory.flatten

  part(1) = move(memory, 0, memory.size - 1).zipWithIndex.map(_.toLong * _).sum

  def merge(seq: IndexedSeq[(Int, Option[Int])], result: IndexedSeq[(Int, Option[Int])]): IndexedSeq[(Int, Option[Int])] =
    (seq, result) match
      case ((0, None) +: rs, _) => merge(rs, result)
      case ((i, None) +: rs, (j, None) +: rr) => merge(rs, (i + j, None) +: rr)
      case (a +: rs, _) => merge(rs, a +: result)
      case (Seq(), _) => result

  def mergeAll(files: IndexedSeq[(Int, Option[Int])], indices: Seq[Int]): IndexedSeq[(Int, Option[Int])] =
    val indicesSorted = indices.sorted.reverse
    indicesSorted.foldLeft(files) { case (acc, i) =>
      val l = acc.take(i - 1)
      val m = acc.slice(i - 1, i + 2)
      val r = acc.drop(i + 2)
      val mr = merge(m.reverse, IndexedSeq.empty)
      l ++ mr ++ r
    }

  def moveAll(files: IndexedSeq[(Int, Option[Int])], id: Int): IndexedSeq[(Int, Option[Int])] =
    if id >= 0 then
      val i = files.indexWhere(_._2.contains(id))
      val newFilesMerged = files(i) match
        case (needed, otherId @ Some(_)) =>
          (0 until i).find(j => files(j) match
            case (size, None) => size >= needed
            case _ => false
          ) match
            case Some(j) =>
              val (l, rest) = files.splitAt(j)
              val (m, r) = rest.tail.splitAt(i - j - 1)
              val newFiles = l ++ IndexedSeq((needed, otherId)) ++ IndexedSeq((files(j)._1 - needed, None)) ++ m ++ IndexedSeq((needed, None)) ++ r.tail
              val newFilesMerged = mergeAll(newFiles, Seq(j, i + 1))
              newFilesMerged
            case _ => files
        case _ => files
      moveAll(newFilesMerged, id - 1)
    else
      files

  part(2) = moveAll(files, files.collect { case (_, Some(id)) => id }.max).flatMap((v, o) => IndexedSeq.fill(v)(o)).zipWithIndex.collect { case (Some(v), i) => i.toLong * v }.sum

}
