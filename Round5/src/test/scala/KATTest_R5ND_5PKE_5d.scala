import org.scalatest.FlatSpec

class KATTest_R5ND_5PKE_5d extends FlatSpec {

  "R5ND_5PKE_5d" should "give the same result as the KAT "in {
    testFunction.testKatPKE("KAT/encrypt/R5ND_5PKE_5d/PQCencryptKAT_1042.req","KAT/encrypt/R5ND_5PKE_5d/PQCencryptKAT_1042.int","KAT/encrypt/R5ND_5PKE_5d/PQCencryptKAT_1042.rsp")
  }

}
