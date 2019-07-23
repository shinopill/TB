import org.scalatest.FlatSpec

class KATTest_R5N1_3KEM_0d extends FlatSpec {
  "R5N1_3KEM_0d" should "give the same result as the KAT" in  {

    testFunction.testKatKEM("KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.req","KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.int","KAT/kem/R5N1_3KEM_0d/PQCkemKAT_24.rsp")

  }
}
