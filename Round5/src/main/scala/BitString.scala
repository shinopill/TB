import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer



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
    * Function to get the Integer value of the bitString
    * @return the signed value of the BitString
    */
  def toChar(): Char = {
    java.lang.Long.parseLong(booleanToString(bits.reverse),2).toChar
  }

  def toByteArray : Array[Byte]  = {
    @tailrec
    def loop(xs : List[Boolean], acc : ListBuffer[Boolean], result : ListBuffer[Byte]): Array[Byte] = {
      xs match{
        case Nil =>
          if(acc.nonEmpty) result.append(Integer.parseInt(BitString.booleanToString(acc.toList),2).toByte)
          result.toList.toArray
        case el :: els => if(acc.length == 8){
          result.append(Integer.parseInt(BitString.booleanToString(acc.toList),2).toByte)
          loop(els,ListBuffer[Boolean](el),result)
        }else{
          acc.prepend(el)
          loop(els,acc,result)
        }
      }
    }
    loop(bits,ListBuffer[Boolean](),ListBuffer[Byte]())
  }
  /**
    *
    * @return the string corresponding to the value of the BitString example 1000001 = A
   */
  def bitStringToString : String ={
  @tailrec
  def loop(xs : List[Boolean], acc: ListBuffer[Boolean], stringBuilder: StringBuilder): String = {
      xs  match {
        case Nil => if (acc.nonEmpty)  stringBuilder.append(Integer.parseInt(booleanToString(acc.toList),2).toChar)
          stringBuilder.toString()
        case el :: els  => if(acc.length == 8){
          stringBuilder.append(Integer.parseInt(booleanToString(acc.toList),2).toChar)
          loop(els,ListBuffer[Boolean](el),stringBuilder)
        }else{
          acc.prepend(el)
          loop(els,acc,stringBuilder)
        }
      }
    }

    loop(bits,new ListBuffer[Boolean](),StringBuilder.newBuilder)
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
    * Concatenate the BitString in parameters to this bitString in a new BitString
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
    @tailrec
    def loop(xs : List[Boolean], stringBuilder: StringBuilder) : String = {
      xs match {
        case Nil => stringBuilder.toString()
        case el :: els =>
          stringBuilder.append(el match {
            case true => "1"
            case _ => "0"
          })
          loop(els, stringBuilder)
      }
    }
    loop(l,StringBuilder.newBuilder)
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
    val list = ListBuffer[Boolean]()
    Integer.toBinaryString(i) foreach  (_ match {
      case '1' => list.prepend(true)
      case '0' => list.prepend(false)
    })
    b.bits = list.toList
    b
  }

  def booleanToString(l : List[Boolean]) : String = {
    @tailrec
    def loop(xs : List[Boolean], stringBuilder: StringBuilder) : String = {
      xs match {
        case Nil => stringBuilder.toString()
        case el :: els =>
          stringBuilder.append(el match {
            case true => "1"
            case _ => "0"
          })
          loop(els, stringBuilder)
      }
    }
    loop(l,StringBuilder.newBuilder)
  }

  def byteArrayToBitString(a :Array[Byte], elem_size : Int ) : BitString = {
    @tailrec
    def loop(xs : List[Byte], acc : ListBuffer[Boolean]) : BitString = {
      xs match {
        case Nil =>
          val b = new BitString("")
          b.bits = acc.toList
          b
        case el :: els  =>
          (0 until elem_size) foreach (i => acc.append((((el & 0xFF) >>> i) & 1) == 1))
          loop(els,acc)
      }
    }

    loop(a.toList,ListBuffer[Boolean]())
  }



}


