/**
  * @author Florent Piller
  */

import params.n


/**
  * Class reprensenting a Polynomial
  * @param coef the coefficients of the polynomials represented as a + bx + .. + x^n
  * @param isN  if the polynimal uses the ring N or Xi
  * @param mod  the modulus of the coefficients
  */
class Polynomial(val coef : Array[Char], val isN : Boolean, var mod : Int ) {

  /**
    * Addition of 2 polynomials
    * @param b the  polynomials to perform the addition with
    * @return the polynomail corresponding to the addition
    */
  def +(b : Polynomial):Polynomial = {
    if(n == 1){
      new Polynomial(Array[Char]((((coef(0) + b.coef(0)) & 0xFFFF ) & mod-1).toChar),isN,mod)
    }else {
      new Polynomial((coef zip b.coef).map(a => (((a._1 + a._2)  & 0xFFFF ) & mod-1).toChar), isN, mod)
    }
  }

  /**
    * This method multiply the polynomial with another
    * @param b the  polynomials to perform the multiplication with
    * @return the polynomail corresponding to the multiplication
    */
  def*(b:Polynomial):Polynomial = {
    if(isN) {
      mult_poly_ntru(b)
    }else{
      var p = lift_poly(this)
      p = p.mult_poly_ntru(b)
     unlift_poly(p)
    }
  }


  /**
    * Multiplication of the polynomail in the ring x^n-1 -1
    * @param b  polynomial to multiply with
    * @return a polynomial corresponding to the multiplication of this polynomial with b
    */
  private def mult_poly_ntru(b:Polynomial):Polynomial = {
    val b_coeffs  =  b.coef :+ 0.toChar                 // Need to add because of lift
    val a_coeffs  =  if(isN) coef :+ 0.toChar else coef // Need to add an element if a is not lifted
    val array = Array.ofDim[Char](a_coeffs.length)
    if(n == 1) {
      array(0) = (((array(0) + b_coeffs(0) * a_coeffs(0)) & 0xFFFF) & mod-1).toChar
    }else{
      b_coeffs.indices foreach(i => {
        a_coeffs.indices foreach(j => {
          val power = (i + j) % b_coeffs.length
          array(power) = (((array(power) + b_coeffs(j) * a_coeffs(i)) & 0xFFFF) & mod-1).toChar
        })
      })
    }

    new Polynomial(if(isN) array.tail else array,isN,mod)
  }


  /**
    * Function to lift a polynomial form ring (x^n-1 -1)/(n-1) to the ring x^n-1 -1
    * @param a the polynomial to be lifted
    * @return the lifted polynomial
    */
 private def lift_poly(a: Polynomial):Polynomial = {
    val array = Array.ofDim[Char](a.coef.length + 1)
    array(0) = (((-1 * a.coef(0)) & 0xFFFF) & mod-1).toChar
    if(a.coef.length > 1) {
      (1 until a.coef.length ) foreach (i => array(i) = (((a.coef(i - 1) - a.coef(i)) & 0xFFFF ) & mod-1).toChar)
      array(a.coef.length) = a.coef.last
    }
    new Polynomial(array,isN,mod)
  }


  /**
    * Function to unlift a polynomial form ring x^n-1 -1 to the ring (x^n-1 -1)/(n-1)
    * @param a the polynomial to be unlifted
    * @return the lifted unpolynomial
    */
  private def unlift_poly(a:Polynomial):Polynomial = {
    val array = Array.ofDim[Char](a.coef.length -1)
    array(0) = ((-1 * a.coef(0)).toChar & mod-1).toChar
    if(a.coef.length > 1 ) {
       (1 until a.coef.length -1) foreach  (i => array(i) = (((array(i - 1) - a.coef(i))  & 0xFFFF ) & mod-1).toChar)
    }
    new Polynomial(array,isN,mod)
  }

}
