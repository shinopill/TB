import org.scalatest.FlatSpec

class KATTest_R5ND_5PKE_0d extends FlatSpec {
  "R5ND_5PKE_0d" should "give the same result as the KAT "in {
  testFunction.testKatPKE("KAT/encrypt/R5ND_5PKE_0d/PQCencryptKAT_1413.req","KAT/encrypt/R5ND_5PKE_0d/PQCencryptKAT_1413.int","KAT/encrypt/R5ND_5PKE_0d/PQCencryptKAT_1413.rsp")
}
}
