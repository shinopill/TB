import params._
object pack_char {

  /**
    * Function to serialise an array of bitString to a String
    * @param a the array to seraialise
    * @return a string value of the array a
    */
  def pack_default(a : Array[BitString]) : BitString  ={
    val padding_size = (a.length * a(0).bits.length) % 8
    if(padding_size != 0){
      0 until padding_size foreach(_ => a.last.:+("0"))
    }
    var b = new BitString("")
    0 until a.length foreach(x => b.::(a(x)))
    b
  }

  /**
    * Function that serialize the component sigma and matrix B
    * @param sigma
    * @param B
    * @return The string corresponding to sigma||B
    */
  def pack_pk(sigma :Array[Byte] , B : Array[Array[Polynome_char]]) : BitString = {
    var b = new BitString("")
    B.foreach(x => x.foreach(y => y.coef.foreach(z => {
      appendZeros(z.toBinaryString,p_bits).foreach(c => b.+:(c.toString))
    })))
    val result = new BitString("")
    sigma foreach (x => {
      appendZeros(x.toBinaryString.slice(0,8),8) foreach(c => result.+:(c.toString))
    })
    result.bits = result.bits.slice(0,kappa) ++ b.bits
    result
  }

  /**
    * Function to serialize component matrix U and vector v
    * @param U
    * @param v
    * @return a string corresponding to U||v
    */
  def pack_ct(U : Array[Array[Polynome_char]],v : Array[BitString]) : BitString = {
    val U_bits = d / n * m_bar * n * p_bits / 8
    var b = new BitString("")
    U.foreach(x => x.foreach(y => y.coef.foreach(z => {
      appendZeros(z.toBinaryString,p_bits) foreach (c => b.bits.+:(c.toString))
    })))
    v.foreach(x => b.::(x))
    b
  }

  /**
    * Function to deserialize a bitString with elementes of size element_size
    * @param bitString
    * @param element_size
    * @return An array of BitString
    */
  def unpack(bitString: BitString, element_size : Int) : Array[BitString] = {
    val b = Array.empty[BitString]
    0 until bitString.bits.length/8 foreach(x => b :+ Integer.parseInt(bitString.toString.slice(8*x,(x+1)*8).reverse,2))
    b
  }

  /**
    * Function to deserialize the pk componement
    * @param pk             The message to deserialize
    * @param sigma_size     The size of sigma in bits
    * @param B_element      The number of coeffs in B
    * @param B_element_bits the size of every coeff
    * @return a tuple with (sigma,matrix B)
    */
  def unpack_pk(pk: BitString, sigma_size : Int,B_element : Int, B_element_bits : Int):(BitString,Array[Array[Polynome]])  ={
    val b_nb_row = B_element/m_bar
    val matrix = Array.fill[Polynome](b_nb_row,m_bar)(new Polynome(Array.ofDim[BitString](n),false,math.pow(2,B_element_bits).toInt))
    (0 until b_nb_row).foreach( i => {
      (0 until m_bar/n).foreach(j => {
        val poly =  new Polynome(Array.ofDim[BitString](n),false,Short.MaxValue)
        (0 until n).foreach(k => {
          val bits = new BitString("")
          bits.bits =  pk.bits.slice(sigma_size * 8 + (i * m_bar ) * B_element_bits + j * B_element_bits * n + k*B_element_bits, sigma_size * 8 + (i * m_bar ) * B_element_bits + j *  B_element_bits * n + (k+1) * B_element_bits)
          poly.coef(k) = bits
        })
          //(b_nb_row * nb_elem_per_row *nb_coeff_in_poly + where we at the row)
        matrix(b_nb_row - i-1)(m_bar -j -1) = poly
      })

    })
    val t = new BitString("")
    t.bits = pk.bits.slice(0,sigma_size*8)
    (t,matrix)
  }

  /**
    * Function to deserialize the ct component
    * @param ct      the message to deserialize
    * @param u_elem  the number of element in matrix U
    * @param u_size  the size of each element in U
    * @param v_elem  The number of element in vector v
    * @param v_size  The size of element in v
    * @return a tuple (matrix U, vector v)
    */
  def unpack_ct(ct: BitString,u_elem : Int,u_size:Int , v_elem:Int,v_size:Int) = {
    val u_nb_col = u_elem/m_bar/n
    val matrix = Array.ofDim[Polynome_char](m_bar,u_nb_col)
    val v = Array[BitString]()
    (0 until m_bar).foreach( i => {
      (0 until u_nb_col).foreach(j => {
        val poly =  new Polynome_char(Array.ofDim[Char](n),false,Short.MaxValue)
        (0 until n).foreach( k => {
          poly.coef(i) = Integer.parseUnsignedInt(
            BitString.booleanToString(ct.bits.slice((i* n_bar)* u_size +  j*u_size  + k*u_elem ,(i* n_bar )* u_size + j*u_size + (k+1)*u_size))).toChar
            //(b_nb_row * nb_elem_per_row *nb_coeff_in_poly + where we at the row)
        })
        matrix(i)(j) = poly
      })
    })

    (0 until v_elem).foreach(i => new BitString("").bits ::: ct.bits.slice(u_elem*u_size + i * v_size,u_elem*u_size + (i+1)* v_size))

    (matrix,v)
  }

  private def appendZeros(bitString : String, wanted_len : Int ) : String = {
    if(bitString.length == wanted_len){
      bitString
    }else if(bitString.length < wanted_len){
      var sb = new StringBuilder(bitString)
      (0 until (wanted_len - bitString.length)) foreach (_ => sb = sb.insert(0,"0"))
      sb.toString()
    }else {
      throw new RuntimeException("Too many bit for the wanted length")
    }
  }
}
