
object Util {

    def printHex(b : Array[Byte]) = {
        b foreach(a => print("%02X".format(a) + " "))
        println
    }

    def printHex(b : Array[Char]) = {
        b foreach(a => print("%02X".format(a >>> 8) + "" +  "%02X".format(a & 0xFF) + " "))
        println
    }

    def appendZeros(bitString : String, wanted_len : Int ) : String = {
        if(bitString.length == wanted_len){
            bitString
        }else if(bitString.length < wanted_len){
            var sb = new StringBuilder(bitString)
            (0 until (wanted_len - bitString.length)) foreach (_ => sb = sb.insert(0,"0"))
            sb.toString()
        }else { // we have a 32 bits Integer for representing negative bytes
            bitString.slice(32-wanted_len, 32)
        }
    }
}