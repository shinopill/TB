import org.scalatest.FlatSpec

class KATTest_R5ND_1PKE_0d extends FlatSpec {

  "R5ND_1PKE_0d" should "give the same result as the KAT "in {
    testFunction.testKatPKE("KAT/encrypt/R5ND_1PKE_0d/PQCencryptKAT_708.req","KAT/encrypt/R5ND_1PKE_0d/PQCencryptKAT_708.int","KAT/encrypt/R5ND_1PKE_0d/PQCencryptKAT_708.rsp")
  }

}
