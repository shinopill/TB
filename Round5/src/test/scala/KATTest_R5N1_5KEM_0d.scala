import org.scalatest.FlatSpec

class KATTest_R5N1_5KEM_0d extends FlatSpec {
  "R5N1_5KEM_0d" should "give the same result as the KAT" in {
    testFunction.testKatKEM("KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.req","KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.int","KAT/kem/R5N1_5KEM_0d/PQCkemKAT_32.rsp")
  }

}
