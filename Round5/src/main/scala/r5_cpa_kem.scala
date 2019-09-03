/**
  * @author Florent Piller
  */

import params.{kappa}
import DRGB._

object r5_cpa_kem {

  def keygen() :(BitString, Array[Byte])  ={
    r5_cpa_pke.keygen
  }

  def encapsulate(pk:BitString) :(BitString, Array[Byte])  ={
    val m = NIST_RNG.randombytes(kappa/8)
    val rho = NIST_RNG.randombytes(kappa/8)
    val ct = r5_cpa_pke.encrypt(pk,m,rho)
    val q = BitString.byteArrayToBitString(m,8).::(ct).bitStringToString.toCharArray map (a => a.toByte)
    val k = hash(kappa/8,BitString.byteArrayToBitString(m,8).::(ct).bitStringToString,"")
    (ct,k)
  }

  def decapsulate(ct:BitString,sk:Array[Byte]) : Array[Byte] ={
    val m = r5_cpa_pke.decrypt(sk,ct)
    val k = hash(kappa/8,m.concat(ct.bitStringToString),"")
    k
  }


}
