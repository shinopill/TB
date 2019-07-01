import javax.xml.bind.DatatypeConverter
import org.scalatest.FlatSpec

import scala.io.Source
class KATTest extends FlatSpec{

  "R5N1_1KEM_0d" should "give the same result as the KAT" in {
    val file_int = "KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.int"
    val file_req = "KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.req"
    val file_res = "KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.rsp"

    val lines_int = (for(i <- Source.fromURL(getClass.getResource(file_int)).getLines()) yield i).toArray
    val lines_req = (for(i <- Source.fromURL(getClass.getResource(file_req)).getLines()) yield i).toArray
    val lines_rsp = (for(i <- Source.fromURL(getClass.getResource(file_res)).getLines()) yield i).toArray

    (0 until 100) foreach(i => {
      val count     = lines_req(0 + i * 7).split(" ")(2)
      val name      = lines_rsp(0).split(" ")(1)
      val tau       = lines_int(0 + i * 6).split("=")(1)
      val seed_kat  = lines_req(1 + i*7).split(" ")(2)
      val pk_kat    = lines_rsp(4 + i*7).split(" ")(2)
      val sk_kat    = lines_rsp(5 + i*7).split(" ")(2)
      val ct_kat    = lines_rsp(6 + i*7).split(" ")(2)
      val ss_kat    = lines_rsp(7 + i*7).split(" ")(2)

      params.tau = Integer.valueOf(tau)
      params.security_level=name
      NIST_RNG.init(DatatypeConverter.parseHexBinary(seed_kat))

      println(count)


      val(pk,sk) = r5_cpa_kem_char.keygen()
      val(ct,ss) = r5_cpa_kem_char.encapsulate(pk)
      val ss_check = r5_cpa_kem_char.decapsulate(ct,sk)

      assert(pk_kat.equalsIgnoreCase(Util.toHexString(pk)))
      assert(sk_kat.equalsIgnoreCase(Util.toHexString(sk)))
      assert(ct_kat.equalsIgnoreCase(Util.toHexString(ct)))
      assert(ss_kat.equalsIgnoreCase(Util.toHexString(ss)))
      assert(Util.toHexString(ss).equalsIgnoreCase(Util.toHexString(ss)))

    })

  }

  "R5N1_3KEM_0d" should "give the same result as the KAT" in {
    val file_int = "KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.int"
    val file_req = "KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.req"
    val file_res = "KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.rsp"

    val lines_int = (for (i <- Source.fromURL(getClass.getResource(file_int)).getLines()) yield i).toArray
    val lines_req = (for (i <- Source.fromURL(getClass.getResource(file_req)).getLines()) yield i).toArray
    val lines_rsp = (for (i <- Source.fromURL(getClass.getResource(file_res)).getLines()) yield i).toArray

    (0 until 100) foreach (i => {
      val count = lines_req(0 + i * 7).split(" ")(2)
      val name = lines_rsp(0).split(" ")(1)
      val tau = lines_int(0 + i * 6).split("=")(1)
      val seed_kat = lines_req(1 + i * 7).split(" ")(2)
      val pk_kat = lines_rsp(4 + i * 7).split(" ")(2)
      val sk_kat = lines_rsp(5 + i * 7).split(" ")(2)
      val ct_kat = lines_rsp(6 + i * 7).split(" ")(2)
      val ss_kat = lines_rsp(7 + i * 7).split(" ")(2)

      params.tau = Integer.valueOf(tau)
      params.security_level = name
      NIST_RNG.init(DatatypeConverter.parseHexBinary(seed_kat))

      println(count)


      val (pk, sk) = r5_cpa_kem_char.keygen()
      val (ct, ss) = r5_cpa_kem_char.encapsulate(pk)
      val ss_check = r5_cpa_kem_char.decapsulate(ct, sk)

      assert(pk_kat.equalsIgnoreCase(Util.toHexString(pk)))
      assert(sk_kat.equalsIgnoreCase(Util.toHexString(sk)))
      assert(ct_kat.equalsIgnoreCase(Util.toHexString(ct)))
      assert(ss_kat.equalsIgnoreCase(Util.toHexString(ss)))
      assert(Util.toHexString(ss).equalsIgnoreCase(Util.toHexString(ss)))

    })
  }
    "R5N1_5KEM_0d" should "give the same result as the KAT" in {
      val file_int = "KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.int"
      val file_req = "KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.req"
      val file_res = "KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.rsp"

      val lines_int = (for (i <- Source.fromURL(getClass.getResource(file_int)).getLines()) yield i).toArray
      val lines_req = (for (i <- Source.fromURL(getClass.getResource(file_req)).getLines()) yield i).toArray
      val lines_rsp = (for (i <- Source.fromURL(getClass.getResource(file_res)).getLines()) yield i).toArray

      (0 until 100) foreach (i => {
        val count = lines_req(0 + i * 7).split(" ")(2)
        val name = lines_rsp(0).split(" ")(1)
        val tau = lines_int(0 + i * 6).split("=")(1)
        val seed_kat = lines_req(1 + i * 7).split(" ")(2)
        val pk_kat = lines_rsp(4 + i * 7).split(" ")(2)
        val sk_kat = lines_rsp(5 + i * 7).split(" ")(2)
        val ct_kat = lines_rsp(6 + i * 7).split(" ")(2)
        val ss_kat = lines_rsp(7 + i * 7).split(" ")(2)

        params.tau = Integer.valueOf(tau)
        params.security_level = name
        NIST_RNG.init(DatatypeConverter.parseHexBinary(seed_kat))

        println(count)


        val (pk, sk) = r5_cpa_kem_char.keygen()
        val (ct, ss) = r5_cpa_kem_char.encapsulate(pk)
        val ss_check = r5_cpa_kem_char.decapsulate(ct, sk)

        assert(pk_kat.equalsIgnoreCase(Util.toHexString(pk)))
        assert(sk_kat.equalsIgnoreCase(Util.toHexString(sk)))
        assert(ct_kat.equalsIgnoreCase(Util.toHexString(ct)))
        assert(ss_kat.equalsIgnoreCase(Util.toHexString(ss)))
        assert(Util.toHexString(ss).equalsIgnoreCase(Util.toHexString(ss)))

      })
    }


}
