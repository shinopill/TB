import java.security.SecureRandom

import params.{kappa}
import DRGB.hash
import NIST_RNG._
object r5_cca_kem_char {
  val random = new SecureRandom()

  def keygen() :(BitString,BitString) = {
    val (pk,sk_cpa_pke) = r5_cpa_pke_char.keygen
    val y = random.generateSeed(kappa/8)
    val sk = BitString.byteArrayToBitString(sk_cpa_pke,8)
    y foreach( i => sk.::(BitString.intToBitString(i)))
    sk.bits = sk.bits ::: pk.bits
    (pk,sk)
  }



  def encapsulate(pk:BitString) : (BitString, Array[Byte]) = {
    val m = random.generateSeed(kappa/8)
    val L_g_rho = hash(3 * kappa/8,m.mkString.concat(pk.bitStringToString),"")
    val rho = L_g_rho.slice(kappa/8*2, kappa/8*3)
    val g =  BitString.byteArrayToBitString(L_g_rho.slice(kappa/8, kappa/8*2),8)
    val ct = r5_cpa_pke_char.encrypt(pk,m,rho) //(U_t,v)
    val L = BitString.byteArrayToBitString(L_g_rho.slice(0,kappa/8),8)
    ct.::(g)
    val k = hash(kappa/8,L.::(ct).bitStringToString,"")
    (ct,k)
  }

  def decapsulate(ct : BitString, sk : BitString) = {
    val m_prime = r5_cpa_pke_char.decrypt(sk.toByteArray,ct)
    val pk = new BitString("")
    pk.bits = sk.bits.slice(kappa * 2 , sk.bits.length)
    val y = new BitString("")
    y.bits = sk.bits.slice(kappa, kappa * 2 )
    val L_prime_g_prime_rho_prime = hash(3 * kappa/8,m_prime.mkString.concat(pk.bitStringToString),"")
    val rho_prime = L_prime_g_prime_rho_prime .slice(kappa/8*2, kappa/8*3)
    val ct_prime = r5_cpa_pke_char.encrypt(pk,m_prime.getBytes(),rho_prime)
    val AreTheSame = ct.==(ct_prime)
    val L_prime = BitString.byteArrayToBitString(L_prime_g_prime_rho_prime.slice(0,kappa/8),8)
    L_prime.cond_mem_copy(y,AreTheSame) // Either L_Prime or Y like in C ref impl
    val k = hash(kappa/8,L_prime.::(ct_prime).bitStringToString,"")
    k
  }

}
