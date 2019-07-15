import java.security.Security
import java.util

import scala.util.control.Breaks.break
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import org.bouncycastle.jcajce.provider.symmetric.AES
import org.bouncycastle.jce.provider.BouncyCastleProvider


object NIST_RNG {

  Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider())
  var aes_ecb : Cipher = _
  var iv_bytes : Array[Byte] = _
  var keyBytes : Array[Byte] = _

  def init(seed : Array[Byte]) = {
    keyBytes = Array.fill[Byte](32)(0)
    iv_bytes = Array.fill[Byte](16)(0)
    AES_CTR_DRGB_Update(seed)
  }

  def randombytes(length : Int ):Array[Byte] ={
    var random_bytes = Array.empty[Byte]
    var byte_to_get = length
    while(byte_to_get != 0 ) {
      incIV
      val rand = AES256_ECB
      if(byte_to_get > 15){
        random_bytes =  random_bytes ++  rand
        byte_to_get -= 16
      }else {
        random_bytes = random_bytes ++ rand.slice(0,byte_to_get)
        byte_to_get = 0
      }
    }
    val bytes = Array.ofDim[Byte](length)
    AES_CTR_DRGB_Update(Array.empty[Byte])
    random_bytes
  }

  def AES256_ECB: Array[Byte] = {
    aes_ecb = Cipher.getInstance("AES/ECB/NoPadding",BouncyCastleProvider.PROVIDER_NAME)
    aes_ecb.init(Cipher.ENCRYPT_MODE,new SecretKeySpec(keyBytes, "AES"))
    aes_ecb.doFinal(iv_bytes)
  }

  def AES_CTR_DRGB_Update(seed : Array[Byte]) = {
    var temp = Array.ofDim[Byte](0)
    (0 until 3) foreach(i => {
      incIV
      temp = temp ++ AES256_ECB
    })
    if(seed.length == 48){
      temp.indices foreach(i => {
        temp(i) = (temp(i) ^ seed(i)).toByte
      })
    }
    keyBytes = temp.slice(0,32)
    iv_bytes = temp.slice(32,48)
  }

  def incIV : Unit ={
    var done = false
    for (j <- 15 until 0 by -1; if !done) {
        if (iv_bytes(j) == -1)
          iv_bytes(j) = 0
        else{
          iv_bytes(j) = (iv_bytes(j) + 1).toByte
          done = true
      }
    }
  }
}

