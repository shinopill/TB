
import java.nio.ByteBuffer
import java.security.{Key, Security}

import params.kappa
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, SecretKeyFactory}
import _root_.org.bouncycastle.crypto.digests.{CSHAKEDigest, SHAKEDigest}
import org.bouncycastle.jce.provider.BouncyCastleProvider



//TODO try to see how we can find if AES instruction is on

object DRGB {
  var random_generator : SHAKEDigest = _

  /**
    * Initialize the DRBG
    * @param seed the mandatory seed
    * @param custom_string the custom string (optional)
    */
  def drgb_init_customization(seed: String, custom_string : String) = {
    custom_string match {
      case ("") => drgb_init(seed)
      case _ =>
        random_generator = new CSHAKEDigest(kappa, "".getBytes(), custom_string.getBytes())
        random_generator.update(seed.getBytes(), 0, seed.length)
        random_generator.update(custom_string.getBytes(),0,custom_string.length)
    }
  }

  /**
    * Initialze the DRBG with a seed
    * @param seed the wanted seed
    */
  def drgb_init(seed: String) = {
    random_generator = new SHAKEDigest(kappa)
    random_generator.update(seed.getBytes(), 0, seed.length)
  }

  /**
    * Function to return a fiexed number a pseudorandom bytes
    * @param len the number of bytes wanted
    * @return an array of len bytes
    */

  def drgb(len : Int): Array[Byte] = {
    var random  = new Array[Byte](len)
    random_generator.doOutput(random,0,len)
    random
  }


  /**
    * Function to return a random number in a given range. the drgb have to be initialized before use
    * @param range the range you want to use must be smaller than 2^16^
    * @return a random number
    */
  def drgb_sampler16(range : Int): Short ={
    val range_divisor = (math.pow(2,16)/range).asInstanceOf[Int]

    var x : Int = 0

    do{
      x = BigInt(drgb(2)).toInt
      x += Short.MaxValue
    }while(x >= range * range_divisor)

    Integer.toUnsignedLong(x/range_divisor).toShort
  }

  /**
    * Function to return a random number given a rang that is a power of 2
    * @param range the range you want
    * @return a random number
    */
  def drgb_sample_16_2(range : Int): Short = {
    val nbr_bits = math.ceil(math.log(range)/math.log(2)).toInt
    var array = drgb(2)
    var x = BigInt(array).toShort
    val bits = BitString.intToBitString(x)
    (nbr_bits until 16).foreach(a => if(a < bits.bits.length) bits.bits =  bits.bits.updated(a,false) else bits.:+("0"))
    bits.toInt.toShort
  }

  /**
    * Function to get an output_len_bytes hash from an input
    * @param output_len_bytes     Wanted length ot the output
    * @param input                The input you want the
    * @param customisation_string The customisation string for the DRGB
    * @return a ouput_len_bytes long array of bytes
    */
  def hash(output_len_bytes:Int, input:String,customisation_string:String):Array[Byte] ={
    var array =  Array.ofDim[Byte](output_len_bytes)
    val shake = if (customisation_string == "" ) new SHAKEDigest(kappa) else new CSHAKEDigest(kappa,"".getBytes(),customisation_string.getBytes())
    shake.doFinal(array,output_len_bytes)
    array
  }
/*
  /**
    *
    * https://stackoverflow.com/questions/36737643/determinisric-aes-ctr-in-java-bouncycastle
    *
    * OUTPUT MUST BE REVERSED ???
    * @param
    * @return
    */

  def AESCTR(len: Int ): String ={

    Security.addProvider(new BouncyCastleProvider)

    var random  = new Array[Byte](len)
    val key = new SecretKeySpec(hash(kappa,"","").toString)
    val aes = Cipher.getInstance("AES/CTR/NoPadding",BouncyCastleProvider.PROVIDER_NAME)
    aes.init(Cipher.ENCRYPT_MODE,key)
    aes.doFinal()
    random.toString
  }

*/
}
