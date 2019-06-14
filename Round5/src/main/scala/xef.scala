import params.{xe,kappa,f}
object xef {

  val register_length = Array(
    Array(Array(11, 13), // XE1-24
          Array(13, 15), // XE1-28
          Array(16, 16)), // XE1-32),
    Array(Array(11, 13, 14, 15), // XE2-53
          Array(13, 15, 16, 17), // XE2-61
          Array(16, 16, 17, 19)), // XE2-68)
    Array(Array(11, 13, 15, 16, 17, 19), // XE3-91
          Array(13, 15, 16, 17, 19, 23), // XE3-103
          Array(16, 16, 17, 19, 21, 23)), // XE3-112),
    Array(Array(11, 13, 16, 17, 19, 21, 23, 29), // XE4-149
          Array(13, 15, 16, 17, 19, 23, 29, 31), // XE4-163
          Array(16, 16, 17, 19, 21, 23, 25, 29)), // XE4-166),
    Array(Array(16, 11, 13, 16, 17, 19, 21, 23, 25, 29), // XE5-190
          Array(24, 13, 16, 17, 19, 21, 23, 25, 29, 31), // XE5-218
          Array(16, 16, 17, 19, 21, 23, 25, 29, 31, 37))) // XE5-234))

  var reg_to_choose = kappa match {
    case 128 => f match {
      case 2 => (1, 0)
      case 4 => (3, 0)
      case 5 => (4, 0)
    }
    case 192 => (4, 1)
    case 256 => (4, 2)
  }


  def compute(m: BitString) : BitString = {

    if(f <= 0 || f > 5){
      m
    }else {
      val xe_left = m.bits.slice(0, xe)
      val xe_right = m.bits.slice(m.bits.length - xe, m.bits.length)
      val registers = Array.fill[BitString](2 * f)(new BitString(""))

      val tuples = registers zip register_length(reg_to_choose._1)(reg_to_choose._2)
      tuples.indices.foreach(i => i match {
        case 0 => f match {
          case 5 => tuples(i)._1.::(getBlockXor(m, i, tuples(i)._2))
          case _ => (0 until tuples(i)._2)
            .foreach(j => tuples(i)._1.:+(getXor(m, j, tuples(i)._2)))
        }
        case _ =>
          (0 until tuples(i)._2)
            .foreach(j => tuples(i)._1.:+(getXor(m, j, tuples(i)._2)))
      })

      val zeros = new BitString("")
      (0 until kappa).foreach(_ => zeros.:+("0"))

      tuples.foreach(tuple => zeros.::(tuple._1))
      m.xor(zeros)
      m
    }
  }

  def fixerr(m : BitString):BitString ={
    val m1 = compute(m)
    val msg =  new BitString("")
    (0 until kappa).foreach(i =>{
      var sum = 0
      (0 until 2*f).foreach(j => {
        var lengRegisters = 0
        (0 until j ).foreach(k => lengRegisters += register_length(reg_to_choose._1)(reg_to_choose._2)(k))
        m1.bits(kappa + lengRegisters + kappa % register_length(reg_to_choose._1)(reg_to_choose._2)(j)) match {
          case true  => sum += 1
          case _ => sum
        }
      })
      msg.bits :+ (if (sum > f ) !m1.bits(i) else m1.bits(i))
    })
    msg
  }

  private def getXor(m : BitString,j : Int,reg_len : Int) : String = {
    var b = m.bits(j)
    (1 until reg_len).foreach(a => b =  m.bits(j + (math.floor((kappa-1-j)/reg_len).toInt*reg_len)) match {
      case  true => if (b) false else true
      case false => if(b) true else false
    })
    b match {
      case true => "1"
      case false => "0"
    }
  }

  private def getBlockXor(m : BitString,j : Int,reg_len : Int) : BitString = {
    var b = new BitString("")
    (0 until reg_len).foreach(a => b.:+(m.bits(a) match {
      case true => "1"
      case false => "0"
    }))

    (1 until math.floor(m.bits.length/reg_len).toInt).foreach(a => b = {
      val r = new BitString("")
      (0 until reg_len).foreach(c => r.:+(m.bits(a*reg_len + c) match{
        case true => "1"
        case false => "0"
      }))
      r.xor(b)
    })
    b
  }
}
