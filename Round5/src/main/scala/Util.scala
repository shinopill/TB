/**
  * @author Florent Piller
  */

object Util {
    /**
      * Fuinction to print a byte array to a Hex String
      * @param b the byte array
      */
    def printHex(b : Array[Byte]) : Unit = {
        b foreach(a => print("%02X".format(a) + " "))
        println
    }

    /**
      * Fuinction to print a Char array to a Hex String
      * @param b the Char array
      */
    def printHex(b : Array[Char]) : Unit = {
        b foreach(a => print("%02X".format(a >>> 8) + "" +  "%02X".format(a & 0xFF) + " "))
        println
    }

    /**
      * Function to get the hex string corresponding to the byte array
      * @param b the byte  array
      */
    def toHexString(b : Array[Byte]) : String ={
        var hex = ""
        b foreach (x =>  hex += "%02X".format(x.toInt & 0xFF))
        hex
    }
    /**
      * Function to get the hex string corresponding to the  char array
      * @param b the char array
      */
    def toHexString(b : BitString): String  = {
        var hex = ""
        b.bitStringToString foreach (x =>  hex += "%02X".format(x.toInt & 0xFF))
        hex
    }

    /**
      * Function to append zeroes to a string representing a bitString
      * @param bitString   the String in which the zeros are append
      * @param wanted_len  the size of the bitString wanted
      * @return            A String of length wanted_length
      */
    def appendZeros(bitString : String, wanted_len : Int ) : String = {
        if(bitString.length == wanted_len){
            bitString
        }else if(bitString.length < wanted_len){
            var sb = new StringBuilder(bitString)
            (0 until (wanted_len - bitString.length)) foreach (_ => sb = sb.insert(0,"0"))
            sb.toString()
        }else { // we have a 32 bits Integer for representing negative bytes or char
            bitString.slice(32-wanted_len, 32)
        }
    }
}
