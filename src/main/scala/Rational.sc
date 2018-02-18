class Rational(n: Int, d: Int) {
  // Like assertion for a class
  require(d != 0)

  private val g = gcd(n, d)
  private val number = n / g
  private val denom = d / g

  // Auxiliary constructor
  def this(n: Int) = this(n, 1)

  // Overriding toString function for a class
  override def toString: String =
    if(denom != 1) getNumber + "/" + getDenom
    else getNumber.toString

  // Adding two rational number
  def +(that: Rational): Rational =
    new Rational(number * that.denom + that.number * denom,
      denom * that.denom)

  def + (that: Int): Rational =
    new Rational(number + that * denom, denom)

  // Multiplying
  def * ( that : Rational): Rational =
    new Rational(number * that.getNumber, denom * that.getDenom)

  // Less than operator
  def <(that: Rational): Boolean =
    if (this.number * that.getNumber < this.denom * that.getDenom) true
    else false

  def max(that: Rational): Rational =
    if (this < that) that else this

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  // Retruning Number
  def getNumber = this.number

  // Returning Denom
  def getDenom = this.denom

}

//Supporting left side operations as well
implicit def intToRational(x : Int): Rational = new Rational(x)


val a = new Rational(1, 2)
val b = new Rational(3, 4)
val f = new Rational(2,4)
a < b
a max b
a + b
a * b
2 * a
3 * a
