
import javax.xml.bind.DatatypeConverter
import System._
object main extends App {

  args(0) match {
    case "R5N?_????_?"
  }

  params.tau = 2
  params.security_level = "R5N1_1KEM_0d"


val seed = "061550234D158C5EC95595FE04EF7A25767F2E24CC2BC479D09D86DC9ABCFDE7056A8C266F9EF97ED08541DBD2E1FFA1"
  val seed_array = DatatypeConverter.parseHexBinary(seed)
  NIST_RNG.init(seed_array)
  core.create_A(Array[Byte](1,2,3,4,5,6,7))



}