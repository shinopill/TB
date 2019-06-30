import org.bouncycastle.crypto.prng.EntropySource
import params.{d, kappa, m_bar, mu, n, p_bits, t_bits}

object r5_cca_pke_char {

  def keygen() = {
    r5_cpa_pke_char.keygen()
  }

  def  encrypt(m : Array[Byte], pk : BitString) = {
    val (c1, k) = r5_cca_kem_char.encapsulate(pk)

    val c2 = r5_dem.dem(k, 16, m)
    val ct = c1.::(BitString.byteArrayToBitString(c2,8))
    val clen = c2.length + math.ceil(kappa/8 + ( m_bar * d/n * n * p_bits  + mu * t_bits)/8).toInt
    (ct,clen)
  }
    def decrypt(ct : BitString, clen: Int , sk : BitString) = {
      val c2_len = clen - math.ceil(kappa/8 + ( m_bar * d/n * n * p_bits  + mu * t_bits)/8)
      val c1 = new BitString("")
      c1.bits = ct.bits.slice(0,math.ceil(kappa/8 + ( m_bar * d/n * n * p_bits  + mu * t_bits)/8).toInt * 8 )
      val k = r5_cca_kem_char.decapsulate(c1,sk)
      val c2 = new BitString("")
      c2.bits = ct.bits.slice(math.ceil(kappa/8 + ( m_bar * d/n * n * p_bits  + mu * t_bits)/8).toInt,clen * 8 )
      val m = r5_dem.dem_inverse(k,kappa/8,c2.toByteArray)
      m

  }
}
