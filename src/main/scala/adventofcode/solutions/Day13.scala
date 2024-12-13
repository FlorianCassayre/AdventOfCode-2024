package adventofcode.solutions

import adventofcode.Definitions.*

import java.math.MathContext

@main def Day13 = Day(13) { (input, part) =>

  sealed case class Rational[N] private (a: N, b: N)(implicit ev: Integral[N])

  object Rational {
    def apply[N](a: N, b: N)(implicit ev: Integral[N]): Rational[N] = {
      import ev._
      require(b != zero)
      def gcd(a: N, b: N): N = if (b == zero) a.abs else gcd(b, a % b)
      val n = gcd(a, b)
      new Rational(sign(a * b) * a.abs / n, b.abs / n)
    }

    def apply[N](a: N)(implicit ev: Integral[N]): Rational[N] = apply(a, ev.one)

    class RationalIntegral[N](implicit ev: Integral[N]) extends Fractional[Rational[N]] {
      import scala.math.Integral.Implicits._
      override def plus(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.a * y.b + x.b * y.a, x.b * y.b)
      override def minus(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.a * y.b - x.b * y.a, x.b * y.b)
      override def times(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.a * y.a, x.b * y.b)
      override def div(x: Rational[N], y: Rational[N]): Rational[N] = Rational(x.a * y.b, x.b * y.a)
      override def negate(x: Rational[N]): Rational[N] = Rational(-x.a, x.b)
      override def fromInt(x: Int): Rational[N] = Rational(ev.fromInt(x), ev.one)
      override def parseString(str: String): Option[Rational[N]] = str match {
        case s"$a/$b" =>
          (ev.parseString(a), ev.parseString(b)) match {
            case (Some(na), Some(nb)) => Some(Rational(na, nb))
            case _ => None
          }
        case _ => None
      }
      override def toInt(x: Rational[N]): Int = ev.quot(x.a, x.b).toInt
      override def toLong(x: Rational[N]): Long = ev.quot(x.a, x.b).toLong
      override def toFloat(x: Rational[N]): Float = x.a.toFloat / x.b.toFloat
      override def toDouble(x: Rational[N]): Double = x.a.toDouble / x.b.toDouble
      override def compare(x: Rational[N], y: Rational[N]): Int = ev.compare(x.a * y.b, x.b * y.a)
    }

    implicit def RationalFractional[N](implicit ev: Integral[N]): Fractional[Rational[N]] = new RationalIntegral[N]
  }

  type BigRational = Rational[BigInt]
  val BigRationalFractional: Fractional[Rational[BigInt]] = Rational.RationalFractional[BigInt]

  implicit class BigRationalWrapper(br: BigRational) {
    def toBigDecimal(mc: MathContext): BigDecimal = BigDecimal(br.a, mc) / BigDecimal(br.b, mc)
  }

  import scala.math.Fractional.Implicits.infixFractionalOps // Mandatory upon usage

  def gcd[N](a: N, b: N)(implicit ev: Integral[N]): N = {
    import ev._
    if (b == zero) a.abs else gcd(b, a % b)
  }

  def lcm[N](a: N, b: N)(implicit ev: Integral[N]): N = {
    import ev._
    val p = (a * b).abs
    if (p == zero) p else p / gcd(a, b)
  }


  val systems = input.split(lineSeparator * 2).map(_.split(lineSeparator).toSeq).map {
    case Seq(s"Button A: X+$ax, Y+$ay", s"Button B: X+$bx, Y+$by", s"Prize: X=$x, Y=$y") =>
      IndexedSeq(IndexedSeq(BigInt(ax.toLong), BigInt(bx.toLong), BigInt(x.toLong)), IndexedSeq(BigInt(ay.toLong), BigInt(by.toLong), BigInt(y.toLong)))
  }

  val costs = IndexedSeq(3, 1)

  def solve(system: IndexedSeq[IndexedSeq[BigInt]], maxPresses: Option[BigInt]): Option[BigInt] =
    val IndexedSeq(IndexedSeq(ax, bx, x), IndexedSeq(ay, by, y)) = system
    val j = (Rational(x, ax) - Rational(y, ay)) / (Rational(bx, ax) - Rational(by, ay))
    val i = (Rational(x) - j * Rational(bx)) / Rational(ax)
    if lcm(i.b, j.b) == BigInt(1) then
      val presses = Seq(i, j).map(_.a)
      if maxPresses.forall(max => presses.forall(_ <= max)) then
        Some(presses.zip(costs.map(BigInt.apply)).map(_ * _).sum)
      else
        None
    else
      None

  part(1) = systems.flatMap(solve(_, Some(100))).sum

  part(2) = systems.map(_.map(r => r.init :+ (r.last + 10000000000000L))).flatMap(solve(_, None)).sum

}
