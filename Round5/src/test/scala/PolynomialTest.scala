import org.scalatest.{BeforeAndAfter, FlatSpec}


class PolynomialTest extends FlatSpec with BeforeAndAfter{
  before{
    params.security_level = "R5N1_1KEM_0d"
  }

  "the multiplication of 2 polynomials" should "be ok" in {
    val a = new Polynomial(Array[Char]('A'),false,64)
    val b = new Polynomial(Array[Char]('B'),false,64)
    val c = new Polynomial(Array[Char](1.toChar,2.toChar), false,64)
    val d = new Polynomial(Array[Char](2.toChar,3.toChar), false,64)
    val e = new Polynomial(Array[Char](1.toChar,2.toChar), true,64)
    val f = new Polynomial(Array[Char](2.toChar,3.toChar), true,64)
    val y = (e*f)
    assert(a.*(b) coef(0) equals 0.toChar)
    y.coef foreach println
    assert(((e*f).coef zip Array[Char](8.toChar,7.toChar)) forall (x => x._1 equals x._2))
  }

  "the addition  of 2 polynomials" should "be ok" in {
    val a = new Polynomial(Array[Char]('A'),false,65)
    val b = new Polynomial(Array[Char]('A'),false,65)
    val zero = new BitString("")
    zero.+("0")
    assert(a.*(b) coef(0) equals 0.toChar )
  }

}
