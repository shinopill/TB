import org.scalatest.FlatSpec

class KATTest_R5N1_1PKE_0d extends FlatSpec {
  "R5N1_1PKE_0d" should "give the same result as the KAT "in {
    testFunction.testKatPKE("KAT/encrypt/R5N1_1PKE_0d/PQCencryptKAT_5772.req","KAT/encrypt/R5N1_1PKE_0d/PQCencryptKAT_5772.int","KAT/encrypt/R5N1_1PKE_0d/PQCencryptKAT_5772.rsp")
  }
}
