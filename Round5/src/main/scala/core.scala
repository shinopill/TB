import org.bouncycastle.pqc.math.linearalgebra.Matrix

import params.{n_bar, q, d, h, m_bar,n,mu,len_tau_2,tau}
import DRGB.{drgb_init_customization, drgb, drgb_init, drgb_sample_16_2,drgb_sampler16}

object core {

  /**
    * Multiply 2 matrix of polynome
    * @param a the first matrix
    * @param b the second matrix
    * @param n the length of the polynomes
    * @param r the mod for the coefficiants of the polybomes
    * @return
    */

  def mult_matrix(a: Array[Array[Polynome]], b: Array[Array[Polynome]],n: Int, r : Int) = {
    val C = Array.ofDim[Polynome](a.length,b(0).length)

    0 until a.length foreach(i => {
      0 until b(0).length foreach(j => {
        C(i)(j) = new Polynome(Array.empty[BitString],a(0)(0).isN,a(0)(0).mod)
        0 until a(0).length foreach (k => {
          println(s"i : $i, j : $j, k :$k")
          var tmp  = a(i)(k).*(b(k)(j))
          C(i)(j) = C(i)(j).+(tmp)
        })
      })
    })
    C
  }

  /**
    * Function that returns the mu first coefficients  of M
    * @param M a n_bar x m_bar polynomial matrix
    * @return A vector with mu coeffs
    */

  def sample_mu(M : Array[Array[Polynome]]):Array[BitString] = {
    if (d == n) {
      (0 until mu) map (v => M(0)(0).coef(v)) toArray
    } else {
      val array = Array.ofDim[BitString](0)
      var i = 0
      while (mu - n * (i + 1) > 0) {
        array ++ M(i / n_bar)(i % n_bar).coef
        i += 1
      }
      (0 until mu % n).foreach(j => array :+ M(i / n_bar)(i % n_bar).coef(j))
      array
    }
  }

  /**
    * Function that converts a Bitstring into mu elements
    * @param m The message to ceonvert
    * @param b_bits the size in bits of each elements
    * @param t_bits the wanted size of the mu elements
    * @return A matrix with MU elements
    */
   def add_msg(m : BitString,b_bits: Int,t_bits : Int )= {
    val v = Array.ofDim[BitString](mu)
    v.indices.foreach(i => {
      v(i).bits :: m.bits.slice(i* b_bits,(i+1)*b_bits)
      (0 until t_bits-b_bits).foreach(j => v(i).+:("0"))
    })
    v
  }

  /**
    * Conmpute the difference between  mu elementes
    * @param m1 the first array of element
    * @param m2 the second array of element
    * @param mod the modulo for the substaction
    */
  def diff_msg(m1 :Array[BitString], m2 :Array[BitString], mod : Int): Array[BitString] ={
    var array = Array.ofDim[BitString](m1.length)
    m1.indices.foreach(i => array(i) = BitString.intToBitString( (m1(i).toInt - m2(i).toInt) % mod))
    array
  }

  /**
    * Transpose a matrix
    * @param matrix the matrix to transpose
    * @return the transposed matrix
    */
  def transpose_matrix(matrix: Array[Array[Short]]): Array[Array[Short]] = {
    matrix.transpose
    matrix
  }

  /**
    * Round a coefficiant
    * @param x the coefficient to round
    * @param a_bits the number of bits for the coeffs at the begining
    * @param b_bits length in bits at then coeff end
    * @return the BitString that is rounded
    */

  def round_element(x: BitString, a_bits: Int, b_bits: Int): BitString = {
    BitString.intToBitString(math.floor((x.toInt + h) / math.pow(2, a_bits - b_bits)).toInt)
  }



  /**
    * Function to round a matrix coefficiant wise
    * @param matrix the matrix to round
    * @param a_bits the number of bits for the coeffs at the begining
    * @param b_bits length in bits at then coeff end
    * @return the matrix with rounded coeffs
    */
  def round_matrix(matrix : Array[Array[Polynome]], a_bits : Int, b_bits : Int): Array[Array[Polynome]]  = {
    matrix.foreach(rows => rows.foreach(poly => poly.coef.foreach(coefs => coefs.bits = round_element(coefs,a_bits,b_bits).bits)))
    matrix
  }

  def round_matrix(matrix : Array[Array[BitString]],a_bits : Int, b_bits : Int) : Array[Array[BitString]] ={
    matrix.foreach(rows => rows.foreach(bitString => bitString.bits =  round_element(bitString,a_bits,b_bits).bits))
    matrix
  }

  /**
    * Function to decompress a coefficient
    * @x the coefficiant to decompress
    * @param a_bits length in bits for the coeff at the begining
    * @param b_bits length in bits at then coeff end
    */

  def decompress(x: BitString, a_bits: Int, b_bits: Int) : BitString = {
    BitString.intToBitString((x.toInt * math.pow(2, a_bits - b_bits)).toInt)
  }

  /**
    * Function to decompress coefficients wise a matrix
    * @param matrix the matrix to decompress
    * @param a_bits length in bits for the coeffs at the begining
    * @param b_bits length in bits at then coeffs end
    */

  def decompress_matrix(matrix : Array[Array[Polynome]], a_bits : Int, b_bits : Int):Array[Array[Polynome]] = {
    matrix.foreach(rows => rows.foreach(poly => poly.coef.foreach(coefs => coefs.bits = decompress(coefs,a_bits,b_bits).bits)))
    matrix
  }

  def decompress_matrix(vector : Array[BitString],a_bits : Int, b_bits : Int): Array[BitString] ={
    vector.foreach(v => v.bits = decompress(v,a_bits,b_bits).bits)
    vector
  }

  /**
    * Function used for the permutation when tau = 1
    * @param sigma seed for the function
    * @return a vector of d elements
    */

  def permutation_tau_1(sigma : String):Array[Short]  = {
    val p1 = new Array[Short](0)
    drgb_init_customization(sigma,Array[Short](1).mkString)
    0 until d foreach(_ => {
      p1 :+ drgb_sampler16(d)
    })
    p1
  }

  /**
    * Function used for the permutation when tau = 2
    * @param sigma seed for the function
    * @return a vector of d elements
    */
  def permutation_tau_2(sigma : String):Array[Char] = {
    val p1 = Array.fill[Char](d)(0)
    drgb_init_customization(sigma,Array[Short](1).mkString)

    0 until d foreach(i => {
      var r :Char = 1
      do {
         r = drgb_sample_16_2(q)
      }while(p1.contains(r))
      p1(i) = r
    })
    p1
  }

  /**
    * Create a ternary secret vector of hamming weight hamming_weight and fill the vector
    * @param length the lenght of the vector
    * @param hamming_weight the hamming wight of the vector
    * @return
    */

  def create_secret_vector(length: Int, hamming_weight: Int):Polynome = {
    val secretVector = Array.fill[BitString](length)(BitString.intToBitString(0))

    for (i <- 0 until hamming_weight) {
      var x  = 0
      do {
        x = drgb_sampler16(d)
      } while (secretVector(x).toInt != 0)

      if (i % 2 == 0) secretVector(x) = BitString.intToBitString(1) else secretVector(x) = BitString.intToBitString(-1)
    }
    new Polynome(secretVector,false,Short.MaxValue)
  }

  /**
    * Compute A_Master. This is used for parameter Tau = 1
    * @param sigma
    * @return
    */
  def create_A_master(sigma: Array[Byte]): Array[Array[Polynome]] ={
    create_A(sigma)
  }

  /**
    * Create the secret matrix A
    * @param sigma the seed for the matrix
    * @return the matriy A
    */
  def create_A(sigma: Array[Byte]): Array[Array[Polynome]] = {
    val nb_poly = d/n
    val matrix =  Array.ofDim[Polynome](d,nb_poly)

    tau  match {
      case 0 =>
        n match {
          case 1 =>
            drgb_init(sigma.toString)
            (0 until d).foreach(a => (0 until nb_poly)
              .foreach(b => matrix(a)(b) = new Polynome(Array[BitString](BitString.intToBitString(drgb_sample_16_2(q).toInt)),false,q )))
          case _ =>
            drgb_init(sigma.toString)
            (0 until d).foreach(a => {
              val poly = new Polynome(Array[BitString](),false,q)
              (0 until nb_poly).foreach(b =>{
                matrix(a)(b) = poly
                (0 until d).foreach(c =>{
                      poly.coef :+ BitString.intToBitString(drgb_sample_16_2(q).toInt)
                  }
                )
              })

            })
        }
      case 1 =>
        val permut_vect = permutation_tau_1(sigma.map(_.toChar).mkString)
        (0 until d).foreach(i => (0 until d)
          .foreach(j => matrix(i)(j) = new Polynome(Array[BitString](BitString.intToBitString((j + permut_vect(i)%d))),false,q )))
      case 2 =>
        drgb_init(sigma.map(_.toChar).mkString)
        val a_vector = (for (i <- 0 until len_tau_2) yield drgb_sample_16_2(q)).toArray
        val p2 = permutation_tau_2(sigma.map(_.toChar).mkString)
        (0 until d).foreach(i => (0 until nb_poly)
          .foreach(j => matrix(i)(j) = new Polynome(Array[BitString](BitString.intToBitString((j + p2(i)%q))),false,q )))
    }
    matrix
  }


  /**
    * Create the secret matrix S_T
    */
  def create_S_T(): Array[Array[Polynome]] = {
    var matrix =  Array.ofDim[Polynome](n_bar,d)
    0 until n_bar foreach (a => 0 until d/n foreach (b => matrix(a)(b) = create_secret_vector(d,h)))
      matrix
  }


  /**
    * Create the secret matreix S_T
    */
  def create_R_T(): Array[Array[Polynome]] = {
    var matrix =  Array.ofDim[Polynome](m_bar,d)
    0 until m_bar foreach (a => 0 until d/n foreach (b => matrix(a)(b) = create_secret_vector(d,h)))
    matrix
  }


}