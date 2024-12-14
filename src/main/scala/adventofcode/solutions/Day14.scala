package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day14 = Day(14) { (input, part) =>

  case class Vec(x: Int, y: Int)

  case class Robot(position: Vec, velocity: Vec)

  val robots = input.toLines.map {
    case s"p=$xp,$yp v=$xv,$yv" => Robot(Vec(xp.toInt, yp.toInt), Vec(xv.toInt, yv.toInt))
  }

  val (width, height) = (101, 103)

  def mod(v: Int, m: Int): Int = ((v % m) + m) % m

  def simulate(robots: Seq[Robot], steps: Int): Seq[Robot] =
    robots.map(r => {
      r.copy(position = Vec(mod(r.position.x + r.velocity.x * steps, width), mod(r.position.y + r.velocity.y * steps, height)))
    })

  def count(robots: Seq[Robot]): Int =
    robots.map(_.position)
      .filter(p => p.x != width / 2 && p.y != height / 2)
      .map(p => Vec(p.x * 2 / width, p.y * 2 / height)).groupBy(identity)
      .values.map(_.size).product

  part(1) = count(simulate(robots, 100))

  def findEasterEgg(i: Int): Int =
    val positions = simulate(robots, i).map(_.position).toSet
    if positions.exists(p => (0 until 10).forall(i => positions.contains(Vec(p.x + i, p.y)))) then i
    else findEasterEgg(i + 1)

  part(2) = findEasterEgg(0)

}
