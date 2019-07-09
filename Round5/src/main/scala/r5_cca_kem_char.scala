import java.security.SecureRandom

import params.{kappa}
import DRGB.hash
import NIST_RNG._
object r5_cca_kem_char {

  def keygen :(BitString,BitString) = {
    val (pk,sk_cpa_pke) = r5_cpa_pke_char.keygen()
    val y = randombytes(kappa/8)
    val sk = BitString.byteArrayToBitString(sk_cpa_pke,8)
    y foreach(i =>  Util.appendZeros(i.toBinaryString,8).reverse foreach(c => sk.:+(c.toString)))
    sk.bits = sk.bits ::: pk.bits
    (pk,sk)
  }



  def encapsulate(pk:BitString) : (BitString, Array[Byte]) = {
    val m = randombytes(kappa/8)
    val sb = new StringBuilder
    m.foreach(a => sb.append(a.toChar))
    val t = sb.toString()
    val L_g_rho = hash(3 * kappa/8,t.concat(pk.bitStringToString),"")
    val rho = L_g_rho.slice(kappa/8*2, kappa/8*3)
    val g =  BitString.byteArrayToBitString(L_g_rho.slice(kappa/8, kappa/8*2),8)
    val ct = r5_cpa_pke_char.encrypt(pk,m,rho)//(U_t,v)
    val q = ct.toByteArray
    val L = BitString.byteArrayToBitString(L_g_rho.slice(0,kappa/8),8)
    ct.::(g)
    val g_byte = g.toByteArray
    val q2 = ct.toByteArray
    val ct_byte =  q ++ g_byte
    val k = hash(kappa/8,L.::(ct).bitStringToString,"")
    (ct,k)
  }

  def decapsulate(ct : BitString, sk : BitString) = {
    val m_prime = r5_cpa_pke_char.decrypt(sk.toByteArray,ct)
    val pk = new BitString("")
    pk.bits = sk.bits.slice(kappa * 2 , sk.bits.length)
    val y = new BitString("")
    y.bits = sk.bits.slice(kappa, kappa * 2 )
    val L_prime_g_prime_rho_prime = hash(3 * kappa/8,m_prime.concat(pk.bitStringToString),"")
    val rho_prime = L_prime_g_prime_rho_prime slice(kappa/8*2, kappa/8*3)
    val g_prime = L_prime_g_prime_rho_prime slice(kappa/8, kappa/8*2)
    val m_prime_array = m_prime map (a => a.toByte) toArray // getBytes doesn't work gives us 25 bytes and not 16
    val ct_prime = r5_cpa_pke_char.encrypt(pk,m_prime_array,rho_prime)
    ct_prime.::(BitString.byteArrayToBitString(g_prime,8))
    val AreTheSame = ct.bits zip ct_prime.bits forall (a => a._1 == a._2 )
    val L_prime = BitString.byteArrayToBitString(L_prime_g_prime_rho_prime.slice(0,kappa/8),8)
    L_prime.cond_mem_copy(y,AreTheSame)//Either L_Prime if true else Y
    val test = L_prime.toByteArray
    val k = hash(kappa/8,L_prime.::(ct_prime).bitStringToString,"")
    k
  }

}
