import javax.xml.bind.DatatypeConverter
import org.scalatest.FlatSpec

import scala.io.Source
class KATTest extends FlatSpec{

  "R5N1_1KEM_0d" should "give the same result as the KAT" in {
    testKatKEM("KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.req","KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.int","KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.rsp")
  }

  "R5N1_3KEM_0d" should "give the same result as the KAT" in {
    testKatKEM("KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.req","KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.int","KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.rsp")

  }

  "R5N1_5KEM_0d" should "give the same result as the KAT" in {
      testKatKEM("KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.req","KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.int","KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.rsp")
  }

  "R5ND_1KEM_0d" should "give the same result as the KAT" in {
    testKatKEM("KAT/kem/R5ND_1KEM_0d/PQCkemKAT_16.req","KAT/kem/R5ND_1KEM_0d/PQCkemKAT_16.int","KAT/kem/R5ND_1KEM_0d/PQCkemKAT_16.rsp")

  }

  "R5ND_3KEM_0d" should "give the same result as the KAT" in {
    testKatKEM("KAT/kem/R5ND_3KEM_0d/PQCkemKAT_24.req","KAT/kem/R5ND_3KEM_0d/PQCkemKAT_24.int","KAT/kem/R5ND_3KEM_0d/PQCkemKAT_24.rsp")

  }

  "R5ND_5KEM_0d" should "give the same result as the KAT" in {
    testKatKEM("KAT/kem/R5ND_5KEM_0d/PQCkemKAT_32.req","KAT/kem/R5ND_5KEM_0d/PQCkemKAT_32.int","KAT/kem/R5ND_5KEM_0d/PQCkemKAT_32.rsp")
  }

  "R5ND_1KEM_5d" should "give the same result as the KAT" in {
    testKatKEM("KAT/kem/R5ND_1KEM_5d/PQCkemKAT_16.req","KAT/kem/R5ND_1KEM_5d/PQCkemKAT_16.int","KAT/kem/R5ND_1KEM_5d/PQCkemKAT_16.rsp")

  }

  "R5ND_3KEM_5d" should "give the same result as the KAT" in {
    testKatKEM("KAT/kem/R5ND_3KEM_5d/PQCkemKAT_24.req","KAT/kem/R5ND_3KEM_5d/PQCkemKAT_24.int","KAT/kem/R5ND_3KEM_5d/PQCkemKAT_24.rsp")

  }

  "R5ND_5KEM_5d" should "give the same result as the KAT" in {
    testKatKEM("KAT/kem/R5ND_5KEM_5d/PQCkemKAT_32.req","KAT/kem/R5ND_5KEM_5d/PQCkemKAT_32.int","KAT/kem/R5ND_5KEM_5d/PQCkemKAT_32.rsp")
  }


  "R5N1_1PKE_0d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5N1_1PKE_0d/PQCencryptKAT_5772.req","KAT/encrypt/R5N1_1PKE_0d/PQCencryptKAT_5772.int","KAT/encrypt/R5N1_1PKE_0d/PQCencryptKAT_5772.rsp")
  }

  "R5N1_3PKE_0d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5N1_3PKE_0d/PQCencryptKAT_9708.req","KAT/encrypt/R5N1_3PKE_0d/PQCencryptKAT_9708.int","KAT/encrypt/R5N1_3PKE_0d/PQCencryptKAT_9708.rsp")
  }

  "R5N1_5PKE_0d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5N1_5PKE_0d/PQCencryptKAT_14700.req","KAT/encrypt/R5N1_5PKE_0d/PQCencryptKAT_14700.int","KAT/encrypt/R5N1_5PKE_0d/PQCencryptKAT_14700.rsp")
  }

  "R5ND_1PKE_0d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5ND_1PKE_0d/PQCencryptKAT_708.req","KAT/encrypt/R5ND_1PKE_0d/PQCencryptKAT_708.int","KAT/encrypt/R5ND_1PKE_0d/PQCencryptKAT_708.rsp")
  }

  "R5ND_3PKE_0d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5ND_3PKE_0d/PQCencryptKAT_1031.req","KAT/encrypt/R5ND_3PKE_0d/PQCencryptKAT_1031.int","KAT/encrypt/R5ND_3PKE_0d/PQCencryptKAT_1031.rsp")
  }

  "R5ND_5PKE_0d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5ND_5PKE_0d/PQCencryptKAT_1413.req","KAT/encrypt/R5ND_5PKE_0d/PQCencryptKAT_1413.int","KAT/encrypt/R5ND_5PKE_0d/PQCencryptKAT_1413.rsp")
  }

  "R5ND_1PKE_5d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5ND_1PKE_5d/PQCencryptKAT_493.req","KAT/encrypt/R5ND_1PKE_5d/PQCencryptKAT_493.int","KAT/encrypt/R5ND_1PKE_5d/PQCencryptKAT_493.rsp")
  }

  "R5ND_3PKE_5d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5ND_3PKE_5d/PQCencryptKAT_828.req","KAT/encrypt/R5ND_3PKE_5d/PQCencryptKAT_828.int","KAT/encrypt/R5ND_3PKE_5d/PQCencryptKAT_828.rsp")
  }

  "R5ND_5PKE_5d" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5ND_5PKE_5d/PQCencryptKAT_1042.req","KAT/encrypt/R5ND_5PKE_5d/PQCencryptKAT_1042.int","KAT/encrypt/R5ND_5PKE_5d/PQCencryptKAT_1042.rsp")
  }

  "R5ND_0KEM_2iot" should "give the same result as the KAT "in {
    testKatKEM("KAT/kem/R5ND_0KEM_2iot/PQCkemKAT_16.req","KAT/kem/R5ND_0KEM_2iot/PQCkemKAT_16.int","KAT/kem/R5ND_0KEM_2iot/PQCkemKAT_16.rsp")
  }

  "R5ND_1KEM_4longkey" should "give the same result as the KAT "in {
    testKatKEM("KAT/kem/R5ND_1KEM_4longkey/PQCkemKAT_24.req", "KAT/kem/R5ND_1KEM_4longkey/PQCkemKAT_24.int", "KAT/kem/R5ND_1KEM_4longkey/PQCkemKAT_24.rsp")
  }

  "R5N1_3PKE_0smallCT" should "give the same result as the KAT "in {
    testKatPKE("KAT/encrypt/R5N1_3PKE_0smallCT/PQCencryptKAT_163584.req","KAT/encrypt/R5N1_3PKE_0smallCT/PQCencryptKAT_163584.int","KAT/encrypt/R5N1_3PKE_0smallCT/PQCencryptKAT_163584.rsp")
  }




  def testKatKEM(request : String, inter : String, response : String): Unit ={


    val lines_int = (for(i <- Source.fromURL(getClass.getResource(inter)).getLines()) yield i).toArray
    val lines_req = (for(i <- Source.fromURL(getClass.getResource(request)).getLines()) yield i).toArray
    val lines_rsp = (for(i <- Source.fromURL(getClass.getResource(response)).getLines()) yield i).toArray

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
      assert(Util.toHexString(ss).equalsIgnoreCase(Util.toHexString(ss_check)))

    })

  }


  def testKatPKE(request : String, inter : String, response : String): Unit = {
    val lines_int = (for (i <- Source.fromURL(getClass.getResource(inter)).getLines()) yield i).toArray
    val lines_req = (for (i <- Source.fromURL(getClass.getResource(request)).getLines()) yield i).toArray
    val lines_rsp = (for (i <- Source.fromURL(getClass.getResource(response)).getLines()) yield i).toArray

    (0 until 75) foreach (i => {
      val count = lines_req(0 + i * 9).split(" ")(2)
      val name = lines_rsp(0).split(" ")(1)
      val tau = lines_int(0 + i * 17).split("=")(1)
      val seed_kat = lines_req(1 + i * 9).split(" ")(2)
      val msg_kat = lines_req(3 + i * 9).split(" ")(2)

      val pk_kat = lines_rsp(6 + i * 9).split(" ")(2)
      val sk_kat = lines_rsp(7 + i * 9).split(" ")(2)
      val clen_kat = lines_rsp(8 + i * 9).split(" ")(2)
      val c_kat = lines_rsp(9 + i * 9).split(" ")(2)

      params.tau = Integer.valueOf(tau)
      params.security_level = name
      NIST_RNG.init(DatatypeConverter.parseHexBinary(seed_kat))

      println(count)

      val (pk, sk) = r5_cca_pke_char.keygen
      val (c, clen) = r5_cca_pke_char.encrypt(DatatypeConverter.parseHexBinary(msg_kat), pk)
      val msg_check = r5_cca_pke_char.decrypt(c, clen, sk)

      assert(pk_kat.equalsIgnoreCase(Util.toHexString(pk)))
      assert(sk_kat.equalsIgnoreCase(Util.toHexString(sk)))
      //assert(Integer.parseInt(clen_kat, 10) == clen)
      assert(c_kat.equalsIgnoreCase(Util.toHexString(c)))
      assert(msg_kat.equalsIgnoreCase(Util.toHexString(msg_check)))

    })
  }
}
