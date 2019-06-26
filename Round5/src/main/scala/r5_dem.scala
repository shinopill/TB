import java.security.Key

import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, SecretKey}
import org.bouncycastle.jcajce.provider.symmetric.AES
import org.bouncycastle.jce.provider.BouncyCastleProvider

object r5_dem {
  private val ALGO = "AES/GCM/NoPadding"
  private val Bouncy = BouncyCastleProvider.PROVIDER_NAME
  def dem(key: Array[Byte],key_len : Int , toEncrypt : Array[Byte]) ={
    val encyption  = Cipher.getInstance(ALGO,Bouncy)
    val key_final = DRGB.hash(16,(key map (a => a.toChar)).mkString,"")
    encyption.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key,"AES"))
    val cipher = encyption.doFinal(toEncrypt)
    cipher
  }

  def dem_inverse(key : Array[Byte], key_len : Int, cipher : Array[Byte]) = {
    val encyption  = Cipher.getInstance(ALGO,Bouncy)
    val key_final = DRGB.hash(16,(key map (a => a.toChar)).mkString,"")
    encyption.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key,"AES"))
    val msg = encyption.doFinal(cipher)
    msg
  }
}
