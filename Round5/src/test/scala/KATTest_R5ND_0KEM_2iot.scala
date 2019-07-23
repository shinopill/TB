import org.scalatest.FlatSpec

class KATTest_R5ND_0KEM_2iot extends FlatSpec {
  "R5ND_0KEM_2iot" should "give the same result as the KAT "in {
    testFunction.testKatKEM("KAT/kem/R5ND_0KEM_2iot/PQCkemKAT_16.req","KAT/kem/R5ND_0KEM_2iot/PQCkemKAT_16.int","KAT/kem/R5ND_0KEM_2iot/PQCkemKAT_16.rsp")
  }
}
