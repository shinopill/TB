import org.scalatest.FlatSpec

class PackTest extends FlatSpec {
  "The packing " should "be done nicely with a length that is not a multiple of 8" in {
    val array = new Array[BitString](3)
    array(0) = new BitString("")
    array(1) = new BitString("")
    array(2) = new BitString("")
    array(0).:+("1")
    array(0).:+("1")
    array(0).:+("1")
    array(0).:+("1")
    array(1).:+("1")
    array(1).:+("1")
    array(1).:+("0")
    array(1).:+("0")
    array(2).:+("1")
    array(2).:+("1")
    array(2).:+("1")
    array(2).:+("1")
    assert(pack.pack_default(array).bitStringToString == 63.toChar+""+15.toChar)
  }

  "The packing " should "be done nicely with a lenght that is a multiple of 8" in{
    val array = new Array[BitString](2)
    array(0) = new BitString("A")
    array(1) = new BitString("B")

    assert(pack.pack_default(array).bitStringToString == "AB")
  }

  "The packing" should "be done nicely with matrix U an v"
}
