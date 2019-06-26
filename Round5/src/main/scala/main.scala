import java.math.BigInteger

import com.google.protobuf.ByteString
import javax.xml.bind.DatatypeConverter

object main extends App{

  params.tau = 2
  params.security_level = "R5N1_1_KEM_0d"
  def hexToByteArray(hex : String) = {
    val r  = hex.split(" ") map (a => (Integer.parseInt(a,16)  & 0xFF).toByte )
    r foreach (a => println(a.toString))
    r
  }
  /*
  val sigma = new BigInteger(args(0)).toByteArray.mkString
  val rho = new BigInteger(args(1)).toByteArray.mkString
  val m1 = new BigInteger(args(2)).toByteArray
  val m = new BigInteger(args(3)).toByteArray
  */
  println("RUNNING MAIN")

  val seed = "061550234D158C5EC95595FE04EF7A25767F2E24CC2BC479D09D86DC9ABCFDE7056A8C266F9EF97ED08541DBD2E1FFA1"
  val seed_array = DatatypeConverter.parseHexBinary(seed)
  Random.init(seed_array)
  //DRGB.drgb_init(seed)
  /*
  val sigma = hexToByteArray("7C 99 35 A0 B0 76 94 AA 0C 6D 10 E4 DB 6B 1A DD")
  val rho = hexToByteArray("D1 13 B6 E7 8A 8E D8 2B 04 16 80 ED 13 4E 88 39")
  val m1 = hexToByteArray("42 49 E0 45 8B 87 4D 2C F0 EE 70 7D E4 06 8E 75 00")
  val m = hexToByteArray("42 49 E0 45 8B 87 4D 2C F0 EE 70 7D E4 06 8E 75")
   */
  val (pk,sk) = r5_cpa_kem_char.keygen
  pk.bitStringToString foreach(x => print(x.toInt + " "))
  println
  sk foreach (x => print(x.toInt + " "))
  val (ct,k) = r5_cpa_kem_char.encapsulate(pk)
  ct.bitStringToString foreach(x => print(x.toInt + " "))
  println
  k foreach (x => print(x.toInt + " "))
  println("End  of test")

/*
  println(" length = " + t._1.bits.length)
  println("--------------------")
  print(t._2.length)


  val ct = r5_cpa_pke_char.encrypt(t._1,new BitString(msg.map(_.toChar).mkString),rho)
  println(ct)
  val q = r5_cpa_pke_char.decrypt(t._2,ct)
  println("Result = " + q)
*/
/*
r5_cpa_pke_keygen:tau=2
r5_cpa_pke_keygen:sigma[16]=7C9935A0B07694AA0C6D10E4DB6B1ADD
r5_cpa_pke_encrypt:rho[16]=D113B6E78A8ED82B041680ED134E8839
r5_cpa_pke_encrypt:sigma[16]=7C9935A0B07694AA0C6D10E4DB6B1ADD
r5_cpa_pke_encrypt:m1[17]=4249E0458B874D2CF0EE707DE4068E7500
r5_cpa_pke_decrypt:m[16]=4249E0458B874D2CF0EE707DE4068E75
*/

}
