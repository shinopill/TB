import org.scalatest.FlatSpec

class BitStringTest extends FlatSpec{


  "A BitString " should "take a string and get a correct bitmap" in {
    assert(new BitString("B").toString == Integer.toBinaryString(66).reverse.patch(7,"0",0))
  }

  "A BitString " should "take a string and get the correct String back" in {
    assert(new BitString("B").bitStringToString == "B")
    assert(new BitString("ABCDEFGHT").bitStringToString == "ABCDEFGHT")
  }
  "A BitString" should "be able to get back to the right integer" in {
    assert(new BitString("A").toChar == 65)
  }

  "An Int" should "be converted in the correct bitString" in {
    assert(BitString.intToBitString(3).toString == Integer.toBinaryString(3).reverse)
    assert(BitString.intToBitString(1112234).toString == Integer.toBinaryString(1112234).reverse)
  }

  "A negative int " should "be in the correct bitString" in {
    assert(BitString.intToBitString(-1).toString == Integer.toBinaryString(-1).reverse)
  }

  "The concatenation of 2 BitString" should "Be correct" in {
    assert(new BitString("A").::(new BitString("A")).toString == new BitString("AA").toString)
    assert(new BitString("A").::(new BitString("B")).toString == new BitString("AB").toString)
  }



}
