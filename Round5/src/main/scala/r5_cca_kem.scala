/**
  * @author Florent Piller
  */
import params.kappa
import DRGB.hash
import NIST_RNG._

import scala.collection.mutable.ListBuffer
object r5_cca_kem {

  def keygen :(BitString,BitString) = {
    val (pk,sk_cpa_pke) = r5_cpa_pke.keygen()
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
    val ct = r5_cpa_pke.encrypt(pk,m,rho)//(U_t,v)
    val L = BitString.byteArrayToBitString(L_g_rho.slice(0,kappa/8),8)
    val k = hash(kappa/8,L.::(ct.::(g)).bitStringToString,"")
    (ct,k)
  }

  def decapsulate(ct : BitString, sk : BitString): Array[Byte] = {
    val m_prime = r5_cpa_pke.decrypt(sk.toByteArray,ct)
    val pk = new BitString("")
    pk.bits = sk.bits.slice(kappa * 2 , sk.bits.length)
    val y = new BitString("")
    y.bits = sk.bits.slice(kappa, kappa * 2 )
    val L_prime_g_prime_rho_prime = hash(3 * kappa/8,m_prime.concat(pk.bitStringToString),"")
    val rho_prime = L_prime_g_prime_rho_prime slice(kappa/8*2, kappa/8*3)
    val g_prime = L_prime_g_prime_rho_prime slice(kappa/8, kappa/8*2)
    val m_prime_array = m_prime map (a => a.toByte) toArray
    val ct_prime = r5_cpa_pke.encrypt(pk,m_prime_array,rho_prime)
    val AreTheSame = ct.bits zip  ct_prime.::(BitString.byteArrayToBitString(g_prime,8)).bits forall (a => a._1 == a._2 )
    val L_prime = BitString.byteArrayToBitString(L_prime_g_prime_rho_prime.slice(0,kappa/8),8)
    L_prime.cond_mem_copy(y,AreTheSame) // Either L_Prime if true else Y
    val k = hash(kappa/8,L_prime.::(ct_prime).bitStringToString,"")
    k
  }
}
