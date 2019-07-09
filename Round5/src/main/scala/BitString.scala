import scala.collection.mutable.ListBuffer
import scala.util.Random



/**
  * Class to represent a string as a bitString where each bit is a boolean value
  * @param string
  */
class BitString(string:String) {

  // List representing each bits
  var bits = List[Boolean]()
  if(string.length != 0) {
    bits = (for {
      char <- string.toCharArray
      bits <- 0 until 8
    } yield char >> bits & 1 match {
      case 1 => true
      case 0 => false
    }).toList
  }
  /**
    * Function that make xor between 2 bitString
    * @param bitString the second bitString to xor with
    * @return the BitString corresponding to the xor of the two bitString
    */
  def xor(bitString: BitString)  ={
    val result = new BitString("")
    if(bits.length == bitString.bits.length){
      var result_bool = bits zip bitString.bits map (a => a._1 ^ a._2)
      val s = booleanToString(result_bool)
      s.foreach(x =>  result :+ x.toString )
      result
    }else{
      result
    }
  }

  /**
    * Function to get the Integer value of the bitString
    * @return the signed value of the BitString
    */
  def toInt: Int = {
    java.lang.Long.parseLong(booleanToString(bits.reverse),2).toInt
  }

  def toByteArray : Array[Byte]  = {
    var list = (0 until math.floor(bits.length/8).toInt) map (x => Integer.parseInt(BitString.booleanToString(bits.slice(x*8,(x+1)*8).reverse),2).toByte)
    if(bits.length%8 != 0) list = list :+ Integer.parseInt(BitString.booleanToString(bits.slice(bits.length - bits.length%8, bits.length).reverse),2).toByte
    list.toArray
  }
  /**
    *
    * @return the string corresponding to the value of the BitString example 1000001 = A
   */
  def bitStringToString : String ={
    var b = List[Boolean]()

    val  sb = StringBuilder.newBuilder
    (0 until bits.length/8).foreach(i => {
      sb.append(Integer.parseInt(booleanToString(bits.slice(i*8,(1+i)*8).reverse),2).toChar)
    })
    if(bits.length%8 != 0) sb.append(Integer.parseInt(booleanToString(bits.slice(bits.length - bits.length%8,bits.length).reverse),2).toChar)
    sb.toString()
  }

  /**
    *
    * @return The actual bit representation of the BitString
    *         Example 011011
    */
  override def toString: String = {
   booleanToString(bits).toString
  }

  /**
    * Concatenate the BitString in parameters to this bitString
    * @param bitString the BitString  to concatenate
    * @return the concatenation of both BitString
    */
  def ::(bitString: BitString): BitString ={
    bits =  bitString.bits.:::(bits)
    this
  }

  /**
    * Add a prepand a bit to the BitSting
    * @param bit the value of the Bit either 0 or 1
    */
  def +:(bit :String): Unit ={
    bit match {
      case "1" => bits =  true +: bits
      case "0" => bits =   false +: bits
    }
  }

  /**
  * Add a append a bit to the BitSting
  * @param bit the value of the Bit either 0 or 1
  */
  def :+ (bit :String): Unit ={
    bit match {
      case "1" =>  bits = bits :+ true
      case "0" =>  bits = bits :+ false
    }
  }

  /**
    * Get the string value of the List[boolean] where true = 1 and false = 0
    * @param l The list you want to get the string value
    * @return the string value of l
    */
  def booleanToString(l : List[Boolean]) : String = {
    val sb = StringBuilder.newBuilder
    l.indices foreach ( x => if(l(x)) {
      sb.append("1")
    }else{
      sb.append("0")
    })
    sb.toString()
  }

  def == (bitString: BitString) : Boolean = {
    bits zip bitString.bits forall( i => i._1 == i._2 )
  }

  def cond_mem_copy(bitString : BitString, b : Boolean) = {
    bits = if (!b) bitString.bits else bits
    this
  }


}


object BitString{
  /**
    * Create a BitString from an Interger
    * @param i the integer
    * @return the BitString corresponding to the Integer
    */
  def intToBitString(i : Int ) = {
    val b = new BitString("")
    Integer.toBinaryString(i) foreach  (x => b.+:(x.toString))
    b
  }

  def booleanToString(l : List[Boolean]) : String = {
    val sb = StringBuilder.newBuilder
    l.indices foreach ( x => if(l(x)) {
      sb.append("1")
    }else{
      sb.append("0")
    })
    sb.toString()
  }

  def byteArrayToBitString(a :Array[Byte], elem_size : Int ) : BitString = {
    val b = new BitString("")
    a.reverse foreach (i => Util.appendZeros(Integer.toBinaryString(i),elem_size) foreach (bit => b.+:(bit.toString)))
    b
  }



}


