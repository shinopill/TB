import org.scalatest.FlatSpec

class KATTest_R5ND_5KEM_5d extends FlatSpec {
  "R5ND_5KEM_5d" should "give the same result as the KAT" in {
    testFunction.testKatKEM("KAT/kem/R5ND_5KEM_5d/PQCkemKAT_32.req","KAT/kem/R5ND_5KEM_5d/PQCkemKAT_32.int","KAT/kem/R5ND_5KEM_5d/PQCkemKAT_32.rsp")
  }
}
