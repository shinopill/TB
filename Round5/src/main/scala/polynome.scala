import params.n

class Polynome(val coef : Array[BitString],val isN : Boolean ,val mod : Int ) {
  def +(b : Polynome):Polynome = {
    //check if in the same ring
    new Polynome((coef zip b.coef).map(a => BitString.intToBitString((a._1.toInt + a._2.toInt) % mod)),isN,mod)
  }


  def*(b:Polynome):Polynome = {
    if(isN) {
      mult_poly_ntru(b)
    }else{
      var p = lift_poly(b)
      p = mult_poly_ntru(p)
      unlift_poly(p)
    }
  }

  private def mult_poly_ntru(b:Polynome):Polynome = {
    val array = Array.ofDim[BitString](coef.length)
    for(i <- 0 until coef.length){
      for(j <- 0 until b.coef.length){
        var power = (i + j) % 1
        var tmp = coef(i).toInt + b.coef(j).toInt
        array(power) = BitString.intToBitString(tmp % mod)
      }
    }
    new Polynome(array,isN,mod)
  }

  private def lift_poly(a: Polynome):Polynome = {
    val array = Array.ofDim[BitString](a.coef.length)
    array(0) = BitString.intToBitString(-1 * a.coef(0).toInt)
    (1 until a.coef.length -1 ) foreach (i => array(i) = BitString.intToBitString((a.coef(i-1).toInt - a.coef(i).toInt)  % a.mod))
    array(a.coef.length-1) = a.coef.last
    new Polynome(array,isN,mod)
  }



  private def unlift_poly(a:Polynome):Polynome = {
    val array = Array.ofDim[BitString](a.coef.length)
    array(0) = BitString.intToBitString(-1 * a.coef(0).toInt)
    for(i <- 1 until a.coef.length){
      array(i) = BitString.intToBitString(array(i-1).toInt - a.coef(i).toInt)
    }
    new Polynome(array,isN,mod)
  }

  /**
  private def areInSameRing(p :Polynome): Boolean = {
    ring zip p.ring forall(x => x._1.toInt == x._2.toInt)
  }


  private def isXi() : Boolean = {
    if(ring.last == BitString.intToBitString(1) && ring(0) == BitString.intToBitString(-1)){
      if ((1 until ring.length-1).forall(i => ring(i) ==BitString.intToBitString(0))){
         true
      }else{
        false
      }
    }else {
      false
    }
  }
    **/



/**
  private def areInSameRing(p :Polynome): Boolean = {
    ring zip p.ring forall(x => x._1.toInt == x._2.toInt)
  }


  private def isXi() : Boolean = {
    if(ring.last == BitString.intToBitString(1) && ring(0) == BitString.intToBitString(-1)){
      if ((1 until ring.length-1).forall(i => ring(i) ==BitString.intToBitString(0))){
         true
      }else{
        false
      }
    }else {
      false
    }
  }
  **/
}
