import org.apache.commons.math3.complex.Complex
import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType}
import params.n
import org.bouncycastle.pqc.crypto.newhope.NTT

class Polynomial(val coef : Array[Char], val isN : Boolean, var mod : Int ) {
  def +(b : Polynomial):Polynomial = {
    if(n == 1){
      new Polynomial(Array[Char]((((coef(0) + b.coef(0)) & 0xFFFF ) & mod-1).toChar),isN,mod)
    }else {
      new Polynomial((coef zip b.coef).map(a => (((a._1 + a._2)  & 0xFFFF ) & mod-1).toChar), isN, mod)
    }
  }


  def*(b:Polynomial):Polynomial = {
    if(isN) {
      mult_poly_ntru(b)
    }else{
      var p = lift_poly(this)
      p = p.mult_poly_ntru(b)
     unlift_poly(p)
    }
  }



  private def mult_poly_ntru(b:Polynomial):Polynomial = {
    val b_coeffs  =  b.coef :+ 0.toChar                 // Need to add because of lift
    val a_coeffs  =  if(isN) coef :+ 0.toChar else coef // Need to add an element if a is not lifted
    var array = Array.ofDim[Char](a_coeffs.length)
    val array_sur = Array.ofDim[Char](a_coeffs.length)
    if(n == 1) {
      array(0) = (((array(0) + b_coeffs(0) * a_coeffs(0)) & 0xFFFF) & mod-1).toChar
    }else{
      b_coeffs.indices foreach(i => {
        a_coeffs.indices foreach(j => {
          var power = (i + j) % b_coeffs.length
          array(power) = (((array(power) + b_coeffs(j) * a_coeffs(i)) & 0xFFFF) & mod-1).toChar
        })
      })
    }

    new Polynomial(if(isN) array.tail else array,isN,mod)
  }



  private def lift_poly(a: Polynomial):Polynomial = {
    val array = Array.ofDim[Char](a.coef.length + 1)
    array(0) = (((-1 * a.coef(0)) & 0xFFFF) & mod-1).toChar
    if(a.coef.length > 1) {
      (1 until a.coef.length ) foreach (i => array(i) = (((a.coef(i - 1) - a.coef(i)) & 0xFFFF ) & mod-1).toChar)
      array(a.coef.length) = a.coef.last
    }
    new Polynomial(array,isN,mod)
  }



  private def unlift_poly(a:Polynomial):Polynomial = {
    val array = Array.ofDim[Char](a.coef.length -1)
    array(0) = ((-1 * a.coef(0)).toChar & mod-1).toChar
    if(a.coef.length > 1 ) {
       (1 until a.coef.length -1) foreach  (i => array(i) = (((array(i - 1) - a.coef(i))  & 0xFFFF ) & mod-1).toChar)
    }
    new Polynomial(array,isN,mod)
  }

}
