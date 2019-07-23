import params.kappa
import _root_.org.bouncycastle.crypto.digests.{CSHAKEDigest, SHAKEDigest}

import scala.collection.mutable.ListBuffer


object DRGB {
  var random_generator : SHAKEDigest = _
  lazy val kapp_for_drng = if (kappa > 128) 256 else 128
  /**
    * Initialize the DRBG
    * @param seed the mandatory seed
    * @param custom_string the custom string (optional)
    */
  def drgb_init_customization(seed: Array[Byte], custom_string : Array[Byte]) = {
    custom_string.isEmpty match {
      case true => drgb_init(seed)
      case false =>
        random_generator = new CSHAKEDigest(kapp_for_drng, "".getBytes(), custom_string)
        random_generator.update(seed, 0, seed.length)
    }
  }

  /**
    * Initialze the DRBG with a seed
    * @param seed the wanted seed
    */
  def drgb_init(seed: Array[Byte]) = {
    random_generator = new SHAKEDigest(kapp_for_drng)
    random_generator.update(seed, 0, seed.length)
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
  def drgb_sampler16(range : Int): Char ={
    val range_divisor = (math.pow(2,16)/range).asInstanceOf[Int]

    var x : Int = 0

    do{
      val array = drgb(2)
      x = ((array(1).toInt << 8) & 0xFF00 ) + (array(0) & 0xFF)
    }while(x >= range * range_divisor)

    Integer.toUnsignedLong(x/range_divisor).toChar
  }

  /**
    * Function to return a random number given a rang that is a power of 2
    * @param range the range you want
    * @return a random number
    */
  def drgb_sample_16_2(range : Int): Char = {
    val nbr_bits = math.ceil(math.log(range)/math.log(2)).toInt
    var array = drgb(2)
    (((( array(1).toInt << 8) & 0xFF00 ) + (array(0) & 0xFF)) & getRange(nbr_bits)).toChar
  }

  def drgb_sample_16_2_all_size(range:Int, nb_elem:Int): Array[Char] = {
    val nbr_bits = math.ceil(math.log(range)/math.log(2)).toInt
    var array = drgb(nb_elem*2)

    def loop(xs : List[Byte], acc_array : List[Byte],  list : ListBuffer[Char]): Array[Char] ={
      xs match {
        case Nil => list.toList.toArray
        case el :: els =>
          acc_array match {
            case Nil => loop(els,List[Byte](el), list)
            case _ =>
              list.append(( ((( el.toInt << 8) & 0xFF00 ) + (acc_array.head & 0xFF)) & getRange(nbr_bits)).toChar)
              loop(els,List[Byte](), list)
          }
      }
    }

   loop(array.toList,List[Byte](),ListBuffer[Char]())
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
    val shake =  new CSHAKEDigest(kapp_for_drng,"".getBytes(),customisation_string.getBytes())
    val bytes = input.toCharArray map (a => a.toByte)
    shake.update(bytes,0,bytes.length)
    shake.doFinal(array,0,output_len_bytes)
    array
  }

  private def getRange(range : Int): Int  = {
    val s = range match{
      case 0  => 0x0000
      case 1  => 0x0001
      case 2  => 0x0003
      case 3  => 0x0007
      case 4  => 0x000F
      case 5  => 0x001F
      case 6  => 0x003f
      case 7  => 0x007f
      case 8  => 0x01FF
      case 9  => 0x01FF
      case 10 => 0x03FF
      case 11 => 0x07FF
      case 12 => 0x0FFF
      case 13 => 0x1FFF
      case 14 => 0x3FFF
      case 15 => 0x7FFF
      case 16 => 0xFFFF
    }
    s
  }

}
