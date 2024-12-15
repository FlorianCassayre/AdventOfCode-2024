package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day15 = Day(15) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    infix def *(v: Int): Vec = Vec(i * v, j * v)
    def gps: Int = i * 100 + j
    def twice: Vec = Vec(i, j * 2)
    def directions2: Seq[Vec] = Seq(this, this + Vec(0, 1))
    def collidingFromBox2: Seq[Vec] = if i != 0 then Seq(this + Vec(0, -1), this) else if j > 0 then Seq(this) else Seq(this * 2)
    def collidingBoxes2: Seq[Vec] = if i != 0 then Seq(this, this + Vec(0, 1)) else if j > 0 then Seq(this * 2) else Seq(this)

  case class State(robot: Vec, boxes: Set[Vec])

  val (((initialRobot, initialBoxes), grid), instructions) =
    input.split(lineSeparator * 2).toSeq match
      case Seq(first, second) =>
        val rawGrid = first.split(lineSeparator).map(_.toIndexedSeq).toIndexedSeq
        def collectPositions(c: Char): Set[Vec] =
          rawGrid.zipWithIndex.flatMap((row, i) => row.zipWithIndex.collect { case (`c`, j) => Vec(i, j) }).toSet
        collectPositions('@').head -> collectPositions('O') -> rawGrid.map(_.map {
          case '#' => false
          case _ => true
        }) -> second.split(lineSeparator).mkString.map {
          case '^' => Vec(-1, 0)
          case '>' => Vec(0, 1)
          case 'v' => Vec(1, 0)
          case '<' => Vec(0, -1)
        }

  val initialState = State(initialRobot, initialBoxes)

  def walkable(p: Vec): Boolean = grid(p.i)(p.j)

  def move(state: State, direction: Vec): State =
    val nextRobot = state.robot + direction
    if walkable(nextRobot) then
      val pushBoxes = LazyList.from(1).map(i => state.robot + direction * i).takeWhile(state.boxes.contains)
      val canBePushed = pushBoxes.lastOption.forall(p => walkable(p + direction))
      if canBePushed then
        State(nextRobot, state.boxes -- pushBoxes ++ pushBoxes.map(_ + direction))
      else
        state
    else
      state

  val finalState = instructions.foldLeft(initialState)(move)

  part(1) = finalState.boxes.toSeq.map(_.gps).sum

  val initialState2 = State(initialRobot.twice, initialBoxes.map(_.twice))

  def walkable2(p: Vec): Boolean = grid(p.i)(p.j / 2)

  def bfsBoxes(direction: Vec, positions: Set[Vec], accBoxes: Set[Vec], boxes: Set[Vec]): Set[Vec] =
    val newBoxes = boxes.intersect(positions)
    if newBoxes.nonEmpty then
      val nextPositions = newBoxes.flatMap(_.directions2).flatMap(b => direction.collidingFromBox2.map(b + _))
      bfsBoxes(direction, nextPositions, accBoxes ++ newBoxes, boxes)
    else
      accBoxes

  def move2(state: State, direction: Vec): State =
    val nextRobot = state.robot + direction
    if walkable2(nextRobot) then
      val pushBoxes = bfsBoxes(direction, direction.collidingFromBox2.map(state.robot + _).toSet, Set.empty, state.boxes)
      val canBePushed = pushBoxes.flatMap(p => direction.collidingBoxes2.map(p + _)).forall(walkable2)
      if canBePushed then
        State(nextRobot, state.boxes -- pushBoxes ++ pushBoxes.map(_ + direction))
      else
        state
    else
      state

  val finalState2 = instructions.foldLeft(initialState2)(move2)

  part(2) = finalState2.boxes.toSeq.map(_.gps).sum

}
