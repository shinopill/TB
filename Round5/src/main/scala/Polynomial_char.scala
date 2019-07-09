import params.n

class Polynomial_char(val coef : Array[Char], val isN : Boolean, var mod : Int ) {
  def +(b : Polynomial_char):Polynomial_char = {
    new Polynomial_char((coef zip b.coef).map(a => ((a._1 + a._2).toChar % mod).toChar),isN,mod)
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
    val b_coeffs  =  b.coef :+ (0.toChar)                 // Need to add because of lift
    val a_coeffs  =  if(isN) coef :+ (0.toChar) else coef // Need to add an element if a is not lifted
    val array = Array.fill[Char](a_coeffs.length)(0)
    (0 until b_coeffs.length) foreach(i => {
      (0 until a_coeffs.length) foreach(j => {
          var power = (i + j) % b_coeffs.length
          array(power) = ((array(power) + b_coeffs(j) * a_coeffs(i)).toChar % mod).toChar
          //print(array(power).toInt + " ")
      })
     // println
      })
    new Polynomial_char(if(isN) array.tail else array,isN,mod)
  }

  private def lift_poly(a: Polynomial_char):Polynomial_char = {
    val array = Array.ofDim[Char](a.coef.length + 1)
    array(0) = ((-1 * a.coef(0)).toChar % mod).toChar
    if(a.coef.length > 1) {
      (1 until a.coef.length ) foreach (i => array(i) = ((a.coef(i - 1) - a.coef(i)).toChar % a.mod).toChar)
      array(a.coef.length) = a.coef.last
    }
    new Polynomial_char(array,isN,mod)
  }



  private def unlift_poly(a:Polynomial_char):Polynomial_char = {
    val array = Array.ofDim[Char](a.coef.length -1)
    array(0) = ((-1 * a.coef(0)).toChar % mod).toChar
    if(a.coef.length > 1 ) {
       (1 until a.coef.length -1) foreach  (i => array(i) = ((array(i - 1) - a.coef(i)).toChar % mod).toChar)
    }
    new Polynomial_char(array,isN,mod)
  }

}
