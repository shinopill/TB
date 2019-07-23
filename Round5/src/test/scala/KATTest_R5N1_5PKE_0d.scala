import org.scalatest.FlatSpec

class KATTest_R5N1_5PKE_0d extends FlatSpec {
  "R5N1_5PKE_0d" should "give the same result as the KAT "in {
    testFunction.testKatPKE("KAT/encrypt/R5N1_5PKE_0d/PQCencryptKAT_14700.req","KAT/encrypt/R5N1_5PKE_0d/PQCencryptKAT_14700.int","KAT/encrypt/R5N1_5PKE_0d/PQCencryptKAT_14700.rsp")
  }
}
