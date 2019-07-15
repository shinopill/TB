import params.{d, _}

object pack_char {

  /**
    * Function to serialise an array of bitString to a String
    *
    * @param a the array to seraialise
    * @return a string value of the array a
    */
  def pack(a: Array[BitString],element_size:Int): BitString = {
    var b = new BitString("")
    0 until a.length foreach (x => b.::(a(x)))
    b
  }

  /**
    * Function that serialize the component sigma and matrix B
    *
    * @param sigma
    * @param B
    * @return The string corresponding to sigma||B
    */
  def pack_pk(sigma: Array[Byte], B: Array[Array[Polynomial_char]]): BitString = {
    var b = new BitString("")
    B.foreach(x => x.foreach(y => y.coef.foreach(z => {
      val temp = new BitString("")
      Util.appendZeros(z.toBinaryString, p_bits).foreach(c => temp.+:(c.toString))
      b = b.::(temp)
    })))
    val result = new BitString("")
    sigma.reverse foreach (x => {
      Util.appendZeros(x.toBinaryString, 8) foreach (c => result.+:(c.toString))
    })
    result.bits = result.bits.slice(0, kappa) ++ b.bits
    result
  }

  /**
    * Function to serialize component matrix U and vector v
    *
    * @param U
    * @param v
    * @return a string corresponding to U||v
    */
  def pack_ct(U: Array[Array[Polynomial_char]], v: Array[BitString]): BitString = {
    var b = new BitString("")
    U.foreach(x => x.foreach(y => y.coef.foreach(z => {
      val temp = new BitString("")
      Util.appendZeros(z.toBinaryString, p_bits).foreach(c => temp.+:(c.toString))
      b = b.::(temp)
    })))
    (0 until ((8-(b.bits.length % 8)) %8)) foreach (_ => b.:+("0"))
    v.foreach(x => b.::(x))
    (0 until ((8-(b.bits.length % 8)) %8)) foreach (_ => b.:+("0"))
    b
  }

  /**
    * Function to deserialize a bitString with elementes of size element_size
    *
    * @param bitString
    * @param element_size
    * @return An array of BitString
    */
  def unpack(bitString: BitString, element_size: Int): Array[BitString] = {
    val b = Array.empty[BitString]
    0 until bitString.bits.length / 8 foreach (x => b :+ Integer.parseInt(bitString.toString.slice(8 * x, (x + 1) * 8).reverse, 2))
    b
  }

  /**
    * Function to deserialize the pk componement
    *
    * @param pk             The message to deserialize
    * @param sigma_size     The size of sigma in bits
    * @param B_element      The number of coeffs in B
    * @param B_element_bits the size of every coeff
    * @return a tuple with (sigma,matrix B)
    */
  def unpack_pk(pk: BitString, sigma_size: Int, B_element: Int, B_element_bits: Int): (BitString, Array[Array[Polynomial_char]]) = {
    val b_nb_row = B_element / n_bar / n
    val matrix = Array.ofDim[Polynomial_char](b_nb_row, m_bar)
    (0 until b_nb_row).foreach(i => {
      (0 until m_bar).foreach(j => {
        val poly = new Polynomial_char(Array.ofDim[Char](n), f != 0 && xe != 0, q)
        (0 until n).foreach(k => {
          val bits = new BitString("")
          bits.bits = pk.bits.slice(sigma_size * 8 + (i * m_bar)  *B_element_bits * n + j * B_element_bits * n + k * B_element_bits, sigma_size * 8 + (i * m_bar) * B_element_bits * n + j * B_element_bits * n + (k + 1) * B_element_bits)
          poly.coef(k) = bits.toInt.toChar
        })
        //(b_nb_row * nb_elem_per_row *nb_coeff_in_poly + where we at the row)
        matrix(i)(j) = poly
      })

    })
    val t = new BitString("")
    t.bits = pk.bits.slice(0, sigma_size * 8)
    (t, matrix)
  }

  /**
    * Function to deserialize the ct component
    *
    * @param ct     the message to deserialize
    * @param u_elem the number of element in matrix U
    * @param u_size the size of each element in U
    * @param v_elem The number of element in vector v
    * @param v_size The size of element in v
    * @return a tuple (matrix U, vector v)
    */
  def unpack_ct(ct: BitString, u_elem: Int, u_size: Int, v_elem: Int, v_size: Int) = {
    val u_nb_col = u_elem / m_bar / n
    val matrix = Array.ofDim[Polynomial_char](m_bar, u_nb_col)
    val U_size = u_elem * u_size + ((8- (u_elem * u_size % 8)) % 8)

    (0 until m_bar).foreach(i => {
      (0 until u_nb_col).foreach(j => {
        val poly = new Polynomial_char(Array.ofDim[Char](n), false, p)
        (0 until n).foreach(k => {
          poly.coef(k) = Integer.parseInt(
            BitString.booleanToString(ct.bits.slice(i *   u_nb_col  * u_size * n + j * u_size * n + k * u_size, i *  u_nb_col *u_size * n + j * u_size * n + (k + 1) * u_size)).reverse,2).toChar
          //(b_nb_row * nb_elem_per_row *nb_coeff_in_poly + where we at the row)
        })
        matrix(i)(j) = poly
      })
    })

    val v = Array.ofDim[BitString](v_elem)

    (0 until v_elem).foreach(i => {
      val bitstring = new BitString("")
      bitstring.bits = ct.bits.slice(U_size + i * v_size, U_size + (i + 1) * v_size)
      v(i) = bitstring
    })

    (matrix, v)
  }


}
