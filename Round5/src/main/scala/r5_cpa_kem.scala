import java.security.SecureRandom
import params.{kappa}
import DRGB._

object r5_cpa_kem {
  val random = new SecureRandom()

  def keygen()  ={
    r5_cpa_pke.keygen
  }

  def encapsulate(pk:BitString) ={
    val m = random.generateSeed(kappa)
    val rho = random.generateSeed(kappa)
    val  ct = r5_cpa_pke.encrypt(pk,new BitString(m.map(_.toChar).mkString),new BitString(rho.map(_.toChar).mkString))
    val k = hash(kappa/8,"","")
    k
  }

  def decapsulate(ct:BitString,sk:BitString) ={
    val m = r5_cpa_pke.decrypt(sk,ct)
    val k = hash(kappa,"","")
    k
  }

  def encapsulate_test(pk:BitString,m: BitString,rho:BitString) = {
    val  ct = r5_cpa_pke.encrypt(pk,m,rho)
    val k = hash(kappa/8,"","")
    k
  }

  def keygen_test(sigma : String) {
    r5_cpa_pke.keygen_test(sigma)
  }

}
