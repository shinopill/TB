import params.{xe, kappa, f, mu}

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
      Array(13, 15, 16, 17, 19, 23, 29, 31),  // XE4-163
      Array(16, 16, 17, 19, 21, 23, 25, 29)), // XE4-166),
    Array(Array(16, 11, 13, 16, 17, 19, 21, 23, 25, 29), // XE5-190
      Array(24, 13, 16, 17, 19, 21, 23, 25, 29, 31), // XE5-218
      Array(16, 16, 17, 19, 21, 23, 25, 29, 31, 37))) // XE5-234))

  var reg_to_choose = kappa match {
    case 128 => f match {
      case 2 => (1, 0)
      case 4 => (3, 0)
      case 5 => (4, 0)
      case _ => (6, 6)
    }
    case 192 => xe match {
      case 163 => (3, 1)
      case 218 => (4, 1)
      case _ => (6,6)
    }
    case 256 => (4, 2)

  }

  /*
  Code from the reference implementation in C
   */

  def computeC(m : BitString) : BitString = {

    if (f <= 0 || f > 5) {
      m
    } else {
      var r = Array.ofDim[Long](10)
      val m_array = m.toByteArray
      var t = 0L

      var bits = 0
      0 until kappa / 8 foreach (i => {
        var j = 0
        var x = m_array(i).toLong & 0xFF
        if (f == 5 || kappa / 8 == 32) {
          t = x
          t = t ^ (t >> 4)
          t = t ^ (t >> 2)
          t = t ^ (t >> 1)
          if (kappa / 8 == 32) {
            r(0) = r(0) ^ ((t & 1) << (i >> 1))
          } else {
            r(0) = r(0) ^ ((t & 1) << i)
          }
          j = 1
        }

        (j until f * 2) foreach (k => r(k) = r(k) ^ (x << (bits % register_length(reg_to_choose._1)(reg_to_choose._2)(k))))
        bits += 8
      })

      (0 until (Math.ceil(xe - kappa) / 8).toInt) foreach m_array :+ 0
      (0 until 2*f) foreach (i => {
        val len = register_length(reg_to_choose._1)(reg_to_choose._2)(i)
        var x = r(i)
        x = x ^ (x >> len)

        0 until len foreach(j =>{
          m_array(bits >> 3) = (m_array(bits >> 3)  ^ (((x >> j) & 1) << (bits & 7)) ).toByte
          bits += 1
        })
      })
      BitString.byteArrayToBitString(m_array,8)
    }
  }


  def fixerr(m: BitString): BitString = {

    if (f > 0 && f <= 5) {
      val t = m.toByteArray
      val msg = new BitString("")
      (0 until kappa).foreach(i => {
        var sum = 0
        (0 until 2 * f).foreach(j => {
          var lengRegisters = 0
          (0 until j).foreach(k =>
            lengRegisters += register_length(reg_to_choose._1)(reg_to_choose._2)(k))
            m.bits(kappa + lengRegisters + kappa % register_length(reg_to_choose._1)(reg_to_choose._2)(j)) match {
            case true => sum += 1
            case _ => sum
          }
        })
        msg.bits = msg.bits :+ (if (sum > f) !m.bits(i) else m.bits(i))
      })
      msg
    } else {
      m.bits = m.bits.take(kappa)
      m
    }
  }

}
