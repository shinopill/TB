
import javax.xml.bind.DatatypeConverter

object main extends App {

  params.tau = 0
  params.security_level = "R5ND_1PKE_0d"



    val seed = "061550234D158C5EC95595FE04EF7A25767F2E24CC2BC479D09D86DC9ABCFDE7056A8C266F9EF97ED08541DBD2E1FFA1"
    val seed_array = DatatypeConverter.parseHexBinary(seed)
    NIST_RNG.init(seed_array)
    val msg = "D81C4D8D734FCBFBEADE3D3F8A039FAA"
    val msg_array = DatatypeConverter.parseHexBinary(msg)

    val (pk, sk) = r5_cca_pke_char.keygen
    val (ct, clen) = r5_cca_pke_char.encrypt(msg_array, pk)
    val msg_check = r5_cca_pke_char.decrypt(ct,clen ,sk)


/*

  val seed = "061550234D158C5EC95595FE04EF7A25767F2E24CC2BC479D09D86DC9ABCFDE7056A8C266F9EF97ED08541DBD2E1FFA1"
  val seed_array = DatatypeConverter.parseHexBinary(seed)
  NIST_RNG.init(seed_array)


  val (pk, sk) = r5_cpa_kem_char.keygen
  val (ct, k) = r5_cpa_kem_char.encapsulate(pk)
  val check = r5_cpa_kem_char.decapsulate(ct, sk)

*/

}