
import params.{d, kappa, m_bar, mu, n, p_bits, t_bits}

object r5_cca_pke {

  def keygen:(BitString,BitString) = {
    val (pk,sk) = r5_cca_kem.keygen
    (pk,sk)
  }

  def  encrypt(m : Array[Byte], pk : BitString):(BitString,Int) = {
    val (c1, k) = r5_cca_kem.encapsulate(pk)
    val c2 = r5_dem.dem(k, kappa/8, m)
    val ct = c1.::(BitString.byteArrayToBitString(c2,8))
    val clen = c2.length + math.ceil(kappa/8 + ( m_bar * d/n.toDouble * n * p_bits  + mu * t_bits)/8).toInt
    (ct,clen)
  }
    def decrypt(ct : BitString, clen: Int , sk : BitString):Array[Byte] = {
      val c2_len = clen - math.ceil(kappa/8 + ( m_bar * d/n.toDouble * n * p_bits  + mu * t_bits)/8)
      val c1 = new BitString("")
      c1.bits = ct.bits.take(math.ceil(kappa/8 + ( m_bar * d/n.toDouble * n * p_bits  + mu * t_bits)/8).toInt * 8)
      val k = r5_cca_kem.decapsulate(c1,sk)
      val ct_byte_array = ct.toByteArray
      val m = r5_dem.dem_inverse(k,kappa/8,ct_byte_array.drop(ct_byte_array.length - c2_len.toInt))
      m

  }
}
