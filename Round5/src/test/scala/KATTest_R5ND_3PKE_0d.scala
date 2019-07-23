import org.scalatest.FlatSpec

class KATTest_R5ND_3PKE_0d extends FlatSpec {
  "R5ND_3PKE_0d" should "give the same result as the KAT "in {
    testFunction.testKatPKE("KAT/encrypt/R5ND_3PKE_0d/PQCencryptKAT_1031.req","KAT/encrypt/R5ND_3PKE_0d/PQCencryptKAT_1031.int","KAT/encrypt/R5ND_3PKE_0d/PQCencryptKAT_1031.rsp")
  }

}
