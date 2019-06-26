import java.security.SecureRandom
import params.{kappa}
import DRGB._

object r5_cpa_kem_char {
  val random = new SecureRandom()

  def keygen()  ={
    r5_cpa_pke_char.keygen
  }

  def encapsulate(pk:BitString) ={
    val m = random.generateSeed(kappa)
    val rho = random.generateSeed(kappa)
    val  ct = r5_cpa_pke_char.encrypt(pk,new BitString(m.map(_.toChar).mkString),rho)
    val k = hash(kappa/8,"","")
    (ct,k)
  }

  def decapsulate(ct:BitString,sk:Array[Byte]) ={
    val m = r5_cpa_pke_char.decrypt(sk,ct)
    val k = hash(kappa,"","")
    k
  }


}
