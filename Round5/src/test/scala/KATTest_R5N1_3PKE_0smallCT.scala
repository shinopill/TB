import org.scalatest.FlatSpec

class KATTest_R5N1_3PKE_0smallCT extends FlatSpec {
  "R5N1_3PKE_0smallCT" should "give the same result as the KAT "in {
    testFunction.testKatPKE("KAT/encrypt/R5N1_3PKE_0smallCT/PQCencryptKAT_163584.req","KAT/encrypt/R5N1_3PKE_0smallCT/PQCencryptKAT_163584.int","KAT/encrypt/R5N1_3PKE_0smallCT/PQCencryptKAT_163584.rsp")
  }
}
