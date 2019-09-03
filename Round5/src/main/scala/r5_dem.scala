/**
  * @author Florent Piller
  */

import javax.crypto.spec.{GCMParameterSpec, IvParameterSpec, SecretKeySpec}
import javax.crypto.{Cipher, SecretKey}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import javax.crypto.spec.GCMParameterSpec
object r5_dem {


  private val ALGO = "AES/GCM/NoPadding"
  private val Bouncy = BouncyCastleProvider.PROVIDER_NAME


  def dem(key: Array[Byte],key_len : Int , toEncrypt : Array[Byte]) ={
    val cipher  = Cipher.getInstance(ALGO,Bouncy)
    val key_final_iv = DRGB.hash(key_len+12,(key map (a => a.toChar)).mkString,"")
    val GCM_params = new GCMParameterSpec(128,key_final_iv.slice(key_len,key_len+12) )
    cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key_final_iv.slice(0,key_len),"AES"), GCM_params)
    cipher.doFinal(toEncrypt)
  }

  def dem_inverse(key : Array[Byte], key_len : Int, cipher : Array[Byte]) = {
    val encyption  = Cipher.getInstance(ALGO,Bouncy)
    val key_final_iv = DRGB.hash(key_len + 12 ,(key map (a => a.toChar)).mkString,"")
    val GCM_params = new GCMParameterSpec(128,key_final_iv.slice(key_len,key_len+12) )
    encyption.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key_final_iv.slice(0,key_len),"AES"),GCM_params)
    val msg = encyption.doFinal(cipher)
    msg
  }
}
