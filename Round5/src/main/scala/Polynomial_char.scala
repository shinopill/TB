import params.n

class Polynomial_char(val coef : Array[Char], val isN : Boolean, val mod : Int ) {
  def +(b : Polynomial_char):Polynomial_char = {
    new Polynomial_char((coef zip b.coef).map(a => ((a._1 + a._2) % mod).toChar),isN,mod)
  }


  def*(b:Polynomial_char):Polynomial_char = {
    if(isN) {
      mult_poly_ntru(b)
    }else{
      var p = lift_poly(this)
      p = p.mult_poly_ntru(b)
      unlift_poly(p)
    }
  }

  private def mult_poly_ntru(b:Polynomial_char):Polynomial_char = {
    val array = Array.fill[Char](coef.length)(0)
    (0 until coef.length) foreach(i => {
      (0 until b.coef.length) foreach(j => {
          var power = (i + j) % n
          array(power) = ((array(power) + coef(i) * b.coef(j)) % mod).toChar
        })
      })
    new Polynomial_char(array,isN,mod)
  }

  private def lift_poly(a: Polynomial_char):Polynomial_char = {
    val array = Array.ofDim[Char](a.coef.length)
    array(0) = ((-1 * a.coef(0)).toChar % mod).toChar
    if(a.coef.length > 1) {
      (1 until a.coef.length - 1) foreach (i => array(i) = ((a.coef(i - 1) - a.coef(i)).toChar % a.mod).toChar)
      array(a.coef.length-1) = a.coef.last
    }
    new Polynomial_char(array,isN,mod)
  }



  private def unlift_poly(a:Polynomial_char):Polynomial_char = {
    val array = Array.ofDim[Char](a.coef.length)
    array(0) = ((-1 * a.coef(0)).toChar % mod).toChar
    if(a.coef.length > 1 ) {
       (0 until a.coef.length -1 ) foreach  (i => array(i) = ((array(i - 1) - a.coef(i)).toChar % mod).toChar)
    }
    new Polynomial_char(array,isN,mod)
  }

}
