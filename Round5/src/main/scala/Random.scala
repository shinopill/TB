import java.security.SecureRandom

object Random {
  var random = new SecureRandom()

  def init(seed : Array[Byte]) = {
    random = new SecureRandom(seed)
  }
}
