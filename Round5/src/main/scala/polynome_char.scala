import params.n

class Polynome_char (val coef : Array[Char],val isN : Boolean ,val mod : Int ) {
  def +(b : Polynome_char):Polynome_char = {
    //check if in the same ring
    new Polynome_char((coef zip b.coef).map(a => ((a._1 + a._2) % mod).toChar),isN,mod)
  }


  def*(b:Polynome_char):Polynome_char = {
    if(isN) {
      mult_poly_ntru(b)
    }else{
      var p = lift_poly(b)
      p = mult_poly_ntru(p)
      unlift_poly(p)
    }
  }

  private def mult_poly_ntru(b:Polynome_char):Polynome_char = {
    val array = Array.ofDim[Char](coef.length)
    (0 until coef.length) foreach(i => {
      (0 until b.coef.length) foreach(j => {
          var power = (i + j) % n
          var tmp = coef(i) + b.coef(j)
          array(power) = (tmp % mod).toChar
        })
      })
    new Polynome_char(array,isN,mod)
  }

  private def lift_poly(a: Polynome_char):Polynome_char = {
    val array = Array.ofDim[Char](a.coef.length)
    array(0) = ((-1 * a.coef(0)) % mod ).toChar
    if(a.coef.length > 1) {
      (1 until a.coef.length - 1) foreach (i => array(i) = ((a.coef(i - 1) - a.coef(i)) % a.mod).toChar)
    }
    array(a.coef.length-1) = a.coef.last
    new Polynome_char(array,isN,mod)
  }



  private def unlift_poly(a:Polynome_char):Polynome_char = {
    val array = Array.ofDim[Char](a.coef.length)
    array(0) = ((-1 * a.coef(0))% mod).toChar
    if(a.coef.length > 1 ) {
       (0 until a.coef.length -1 ) foreach  (i => array(i) = ((array(i - 1) - a.coef(i)) % mod).toChar)
    }
    new Polynome_char(array,isN,mod)
  }

}
