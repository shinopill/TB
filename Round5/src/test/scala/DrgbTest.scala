import org.scalatest.{BeforeAndAfter, FlatSpec}
import DRGB.{drgb, drgb_init, drgb_init_customization, drgb_sample_16_2, drgb_sample_16_2_all_size}

class DrgbTest extends FlatSpec with BeforeAndAfter{
  before{
    params.security_level = "R5N1_1KEM_0d"
  }

  "A true DPRG" should "give the same output between 2 initializations" in {
    drgb_init_customization(0.toByte :: Nil toArray,0.toByte :: Nil toArray)
    val x = drgb(2)
    drgb_init_customization(0.toByte :: Nil toArray,0.toByte :: Nil toArray)
    val y = drgb(2)
    assert(x.length == y.length)
    val z = x zip y
    val equal  = z.forall( a => a._1 == a._2)
    assert(equal)
  }

  "A true DRGB" should "not give the same output with different seed" in {
    drgb_init("seed".toCharArray map (c => c.toByte))
    val x = drgb(2)
    drgb_init("init2".toCharArray map (c => c.toByte))
    val y = drgb(2)
    assert(x.length == y.length)
    val z = x zip y
    val equal  = z.forall( a => a._1 == a._2)
    assert(!equal)
  }

  " drgb_sample_16_2_all_size" should "give the same random as drgb_sample_16_2 with the exact same size and initilization" in {
    drgb_init("seed".toCharArray map (c => c.toByte))
    val x = drgb_sample_16_2_all_size(16,8)
    drgb_init("seed".toCharArray map (c => c.toByte))
    val y = Array.ofDim[Char](8)
    0 until 8 foreach (y(_) = drgb_sample_16_2(16))
    val z = x zip y
    val equal  = z.forall( a => a._1 == a._2)
    assert(equal)
  }


}
