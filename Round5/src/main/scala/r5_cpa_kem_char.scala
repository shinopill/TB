import java.security.SecureRandom
import params.{kappa}
import DRGB._

object r5_cpa_kem_char {
  val random = new SecureRandom()

  def keygen()  ={
    r5_cpa_pke_char.keygen
  }

  def encapsulate(pk:BitString) ={
    val m = NIST_RNG.randombytes(kappa/8)
    Util.printHex(m)
    val rho = NIST_RNG.randombytes(kappa/8)
    Util.printHex(rho)
    val  ct = r5_cpa_pke_char.encrypt(pk,m,rho)
    val k = hash(kappa/8,BitString.byteArrayToBitString(m,8).::(ct).bitStringToString,"")
    (ct,k)
  }

  def decapsulate(ct:BitString,sk:Array[Byte]) ={
    val m = r5_cpa_pke_char.decrypt(sk,ct)
    val k = hash(kappa/8,m.concat(ct.bitStringToString),"")
    k
  }


}