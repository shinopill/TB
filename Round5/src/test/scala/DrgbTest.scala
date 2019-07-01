import org.scalatest.FlatSpec
import DRGB.{drgb,drgb_init,drgb_init_customization}

class DrgbTest extends FlatSpec{
  "A true DPRG" should "give the same output between 2 initializations" in {
    drgb_init_customization("djdjd","jfjf")
    val x = drgb(2)
    drgb_init_customization("djdjd","jfjf")
    val y = drgb(2)
    assert(x.length == y.length)
    val z = x zip y

    val equal  = z.forall( a => a._1 == a._2)
    assert(equal)
  }

  "A true DRGB" should "not give the same output with different seed" in {
    drgb_init("seed")
    val x = drgb(2)
    drgb_init("init2")
    val y = drgb(2)
    assert(x.length == y.length)
    val z = x zip y
    val equal  = z.forall( a => a._1 == a._2)
    assert(equal)
  }
}
