import org.scalatest.FlatSpec

class KATTest_R5N1_1KEM_0d extends FlatSpec {
  "R5N1_1KEM_0d" should "give the same result as the KAT" in{
    testFunction.testKatKEM("KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.req","KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.int","KAT/kem/R5N1_1KEM_0d/PQCkemKAT_16.rsp")
  }
}
