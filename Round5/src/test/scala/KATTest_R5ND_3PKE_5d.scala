import org.scalatest.FlatSpec

class KATTest_R5ND_3PKE_5d extends FlatSpec {
  "R5ND_3PKE_5d" should "give the same result as the KAT "in {
    testFunction.testKatPKE("KAT/encrypt/R5ND_3PKE_5d/PQCencryptKAT_828.req","KAT/encrypt/R5ND_3PKE_5d/PQCencryptKAT_828.int","KAT/encrypt/R5ND_3PKE_5d/PQCencryptKAT_828.rsp")
  }

}
