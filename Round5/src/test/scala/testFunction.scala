import javax.xml.bind.DatatypeConverter

import scala.io.Source

object testFunction {
  def testKatKEM(request : String, inter : String, response : String): Unit = {
    val source_int = Source.fromURL(getClass.getResource(inter))
    val source_req = Source.fromURL(getClass.getResource(request))
    val source_rsp = Source.fromURL(getClass.getResource(response))

    val lines_int = (for (i <- source_int.getLines()) yield i).toArray
    val lines_req = (for (i <- source_req.getLines()) yield i).toArray
    val lines_rsp = (for (i <- source_rsp.getLines()) yield i).toArray

    source_int.close()
    source_req.close()
    source_rsp.close()

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

      val (pk, sk) = r5_cpa_kem.keygen()
      val (ct, ss) = r5_cpa_kem.encapsulate(pk)
      val ss_check = r5_cpa_kem.decapsulate(ct, sk)

      assert(Util.toHexString(pk).equalsIgnoreCase(pk_kat))
      assert(Util.toHexString(sk).equalsIgnoreCase(sk_kat))
      assert(Util.toHexString(ct).equalsIgnoreCase(ct_kat))
      assert(Util.toHexString(ss).equalsIgnoreCase(ss_kat))
      assert(Util.toHexString(ss).equalsIgnoreCase(Util.toHexString(ss_check)))

    })
  }

  def testKatPKE( request : String,  inter : String,   response : String) : Unit = {
    val source_int = Source.fromURL(getClass.getResource(inter))
    val source_req = Source.fromURL(getClass.getResource(request))
    val source_rsp = Source.fromURL(getClass.getResource(response))

    val lines_int = (for (i <- source_int.getLines()) yield i).toArray
    val lines_req = (for (i <- source_req.getLines()) yield i).toArray
    val lines_rsp = (for (i <- source_rsp.getLines()) yield i).toArray

    source_int.close()
    source_req.close()
    source_rsp.close()

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

      val (pk, sk) = r5_cca_pke.keygen

      val (c, clen) = r5_cca_pke.encrypt(DatatypeConverter.parseHexBinary(msg_kat), pk)

      val msg_check = r5_cca_pke.decrypt(c, clen, sk)


      assert(DatatypeConverter.parseHexBinary(pk_kat) zip pk.toByteArray forall (a => a._1 == a._2)) //Much faster for
      assert(DatatypeConverter.parseHexBinary(sk_kat) zip sk.toByteArray forall (a => a._1 == a._2))
      assert(Util.toHexString(c).equalsIgnoreCase(c_kat))
      assert(clen == Integer.parseInt(clen_kat, 10))
      assert(Util.toHexString(msg_check).equalsIgnoreCase(msg_kat))

    })

  }
}
