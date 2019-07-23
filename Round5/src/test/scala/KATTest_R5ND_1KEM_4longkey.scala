import org.scalatest.FlatSpec

class KATTest_R5ND_1KEM_4longkey extends FlatSpec {
  "R5ND_1KEM_4longkey" should "give the same result as the KAT "in {
    testFunction.testKatKEM("KAT/kem/R5ND_1KEM_4longkey/PQCkemKAT_24.req", "KAT/kem/R5ND_1KEM_4longkey/PQCkemKAT_24.int", "KAT/kem/R5ND_1KEM_4longkey/PQCkemKAT_24.rsp")
  }
}
