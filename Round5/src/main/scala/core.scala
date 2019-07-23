import DRGB.{drgb_init, drgb_init_customization, drgb_sample_16_2, drgb_sample_16_2_all_size, drgb_sampler16}
import params.{d, f, h, kappa, len_tau_2, m_bar, mu, n, n_bar, p, q, t, tau, xe}
import no.uib.cipr.matrix._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
object core {


  def mult_matrix_n1(a: Array[Array[Polynomial]], b: Array[Array[Polynomial]], n: Int, mod: Int) = {
    val a_lift = Array.ofDim[Double](a.length,a(0).length)
    val b_lift = Array.ofDim[Double](b.length,b(0).length)
    val c_lift = Array.ofDim[Double](a.length,b(0).length)

    val result = Array.ofDim[Polynomial](a.length,b(0).length)

    a.indices foreach(i => a(0).indices  foreach(j => a_lift(i)(j) = (((-1 * a(i)(j).coef(0)) & 0xFFFF) & mod-1).toDouble))
    b.indices foreach(i => b(0).indices  foreach(j => b_lift(i)(j) = (((-1 * b(i)(j).coef(0)) & 0xFFFF) & mod-1).toDouble))

    val t_test = new DenseMatrix(a_lift)
    val q_test = new DenseMatrix(b_lift)
    val c_test = new DenseMatrix(c_lift)
    val d = t_test.mult(q_test,c_test)

    result.indices foreach(i => result(0).indices foreach(j => result(i)(j) = new Polynomial(Array[Char](((math.round(d.get(i,j)) & 0xFFFF ) & mod-1).toChar), a(0)(0).isN, mod)))

    result


  }
  /**
    * Multiply 2 matrix of Polynome_char_char
    *
    * @param a   the first matrix
    * @param b   the second matrix
    * @param n   the length of the Polynome_chars
    * @param mod the mod for the coefficiants of the polybomes
    * @return
    */

  def mult_matrix(a: Array[Array[Polynomial]], b: Array[Array[Polynomial]], n: Int, mod: Int) = {
    if(n == 1){
      mult_matrix_n1(a,b,n,mod)
    }else {
      val C = Array.ofDim[Polynomial](a.length, b(0).length)
      a.indices foreach (i => {
        b(0).indices foreach (j => {
          C(i)(j) = new Polynomial(Array.ofDim[Char](n), a(0)(0).isN, mod)
          a(0).indices foreach (k => {
            a(i)(k).mod = mod
            b(k)(j).mod = mod
            var tmp = a(i)(k).*(b(k)(j))
            C(i)(j) = C(i)(j).+(tmp)
          })
        })
      })
      C
    }

  }

  /**
    * Function that returns the mu first coefficients  of M
    *
    * @param M a n_bar x m_bar polynomial matrix
    * @return A vector with mu coeffs
    */

  def sample_mu(M: Array[Array[Polynomial]]): Array[Char] = {
    if (d == n) {
      (0 until mu) map (v => M(0)(0).coef(v)) toArray
    } else {
      var array = Array.ofDim[Char](mu)
      val nb_col = M(0).length
      val nb_row = M.length
      (0 until mu) foreach (i => array(i) = M(i / (nb_col * n))((i / n) % (nb_col)).coef(i % n))
      array
    }
  }

  /**
    * Function that converts a Bitstring into mu elements
    *
    * @param m      The message to ceonvert
    * @param b_bits the size in bits of each elements
    * @param t_bits the wanted size of the mu elements
    * @return An array with MU elements
    */
  def add_msg(M: Array[Array[Polynomial]], m: BitString, b_bits: Int, t_bits: Int) = {
    val v = Array.fill[BitString](mu)(new BitString(""))
    val nb_col = M(0).length
    val nb_row = M.length
    val poly_size = M(0)(0).coef.length
    //scale m from b to t bits
    v.indices.foreach(i => {
      val list = ListBuffer[Boolean]()
      list.appendAll(m.bits.slice(i * b_bits, (i + 1) * b_bits))
      (0 until t_bits - b_bits).foreach(_ => list.prepend(false))
      v(i).bits = list.toList
    })
    (0 until mu) foreach (i => v(i) = {
      val string = Util.appendZeros(Integer.toBinaryString((M(i / (nb_col * poly_size))((i / poly_size) % (nb_col)).coef(i % poly_size) + v(i).toChar).toChar % t), t_bits)
      val bits = new BitString("")
      val list = ListBuffer[Boolean]()
      string.foreach(_ match {
        case '1' => list.prepend(true)
        case _ => list.prepend(false)
      })
      bits.bits = list.toList
      bits
    })
    v

  }

  /**
    * Conmpute the difference between  mu elementes
    *
    * @param m1  the first array of element
    * @param m2  the second array of element
    * @param mod the modulo for the substaction
    */
  def diff_msg(m1: Array[Char], m2: Array[Char], mod: Int): Array[Char] = {
    var array = Array.ofDim[Char](m1.length)
    m1.indices.foreach(i => array(i) = ((m1(i) - m2(i) % mod)).toChar)
    array
  }

  /**
    * Conmpute the difference between  mu elementes
    *
    * @param m1  the first array of element
    * @param m2  the second array of element
    * @param mod the modulo for the substaction
    */
  def diff_msg(m1: Array[BitString], m2: Array[BitString], mod: Int): Array[BitString] = {
    val x = m1 map (y => y.toChar)
    val x2 = m2 map (y => y.toChar)
    var array = Array.ofDim[BitString](m1.length)
    m1.indices.foreach(i => array(i) = BitString.intToBitString((m1(i).toChar - m2(i).toChar).toChar % mod))
    array
  }


  /**
    * Round a coefficiant
    *
    * @param x      the coefficient to round
    * @param a_bits the number of bits for the coeffs at the begining
    * @param b_bits length in bits at then coeff end
    * @return the BitString that is rounded
    */

  def round_element(x: Char, a_bits: Int, b_bits: Int, rounding_constant: Int): Char = {
    (((x + rounding_constant) >>> (a_bits - b_bits)).toChar % math.pow(2, b_bits).toInt).toChar
  }

  /**
    * Round a coefficiant
    *
    * @param x      the coefficient to round
    * @param a_bits the number of bits for the coeffs at the begining
    * @param b_bits length in bits at then coeff end
    * @return the BitString that is rounded
    */

  def round_element(x: BitString, a_bits: Int, b_bits: Int, rounding_constant: Int): BitString = {
    BitString.intToBitString(round_element(x.toChar, a_bits, b_bits, rounding_constant).toInt)
  }


  /**
    * Function to round a matrix coefficiant wise
    *
    * @param matrix the matrix to round
    * @param a_bits the number of bits for the coeffs at the begining
    * @param b_bits length in bits at then coeff end
    * @return the matrix with rounded coeffs
    */
  def round_matrix(matrix: Array[Array[Polynomial]], a_bits: Int, b_bits: Int, rounding_constant: Int): Array[Array[Polynomial]] = {
    matrix.foreach(rows => rows.foreach(poly => poly.coef.indices.foreach(i => poly.coef(i) = round_element(poly.coef(i), a_bits, b_bits, rounding_constant))))
    matrix
  }


  def round_matrix(matrix: Array[Array[BitString]], a_bits: Int, b_bits: Int, rounding_constant: Int): Array[Array[BitString]] = {
    matrix.foreach(rows => rows.foreach(bitString => bitString.bits = {
      var bits = round_element(bitString, a_bits, b_bits, rounding_constant).bits
      if (bits.length != b_bits) (0 until b_bits - bits.length) foreach (_ => bits = bits :+ false)
      bits
    })
    )
    matrix
  }

  /**
    * Function to decompress a coefficient
    *
    * @x the coefficiant to decompress
    * @param a_bits length in bits for the coeff at the begining
    * @param b_bits length in bits at then coeff end
    */

  def decompress(x: Char, a_bits: Int, b_bits: Int): Char = {
    (x.toInt * math.pow(2, a_bits - b_bits)).toChar
  }

  /**
    * Function to decompress a coefficient
    *
    * @x the coefficciant to decompress
    * @param a_bits length in bits for the coeff at the begining
    * @param b_bits length in bits at then coeff end
    */

  def decompress(x: BitString, a_bits: Int, b_bits: Int): BitString = {
    BitString.intToBitString((x.toChar * math.pow(2, a_bits - b_bits)).toInt)
  }

  /**
    * Function to decompress coefficients wise a matrix
    *
    * @param matrix the matrix to decompress
    * @param a_bits length in bits for the coeffs at the begining
    * @param b_bits length in bits at then coeffs end
    */

  def decompress_matrix(matrix: Array[Array[Polynomial]], a_bits: Int, b_bits: Int): Array[Array[Polynomial]] = {
    matrix.foreach(rows => rows.foreach(poly => poly.coef.indices.foreach(i => poly.coef(i) = decompress(poly.coef(i), a_bits, b_bits))))
    matrix
  }

  def decompress_vector(vector: Array[BitString], a_bits: Int, b_bits: Int): Array[BitString] = {
    vector.foreach(v => v.bits = decompress(v, a_bits, b_bits).bits)
    vector
  }

  /**
    * Function used for the permutation when tau = 1
    *
    * @param sigma seed for the function
    * @return a vector of d elements
    */

  def permutation_tau_1(sigma: Array[Byte]): Array[Short] = {
    val p1 = new Array[Short](0)
    drgb_init_customization(sigma, Array[Byte](0, 1))
    0 until d foreach (_ => {
      p1 :+ drgb_sampler16(d)
    })
    p1
  }

  /**
    * Function used for the permutation when tau = 2
    *
    * @param sigma seed for the function
    * @return a vector of d elements
    */
  def permutation_tau_2(sigma: Array[Byte]): Array[Char] = {
    val p2 = Array.ofDim[Char](d)
    val p2_drawn = Array.fill[Boolean](len_tau_2)(false)
    drgb_init_customization(sigma, Array[Byte](0, 1))

    0 until d foreach (i => {
      var r: Char = 1
      do {
        r = drgb_sample_16_2(len_tau_2)
      } while (p2_drawn(r))
      p2(i) = r
      p2_drawn(r) = true
    })
    p2
  }

  /**
    * Create a ternary secret vector of hamming weight hamming_weight and fill the vector
    *
    * @param length         the lenght of the vector
    * @param hamming_weight the hamming wight of the vector
    * @return
    */

  def create_secret_vector(length: Int, hamming_weight: Int): Array[Char] = {
    val secretVector = Array.fill[Char](length)(0)

    (0 until hamming_weight) foreach (i => {
      var x = 0
      do {
        x = drgb_sampler16(d)
      } while (secretVector(x).toInt != 0)

      if (i % 2 == 0) secretVector(x) = 1.toChar else secretVector(x) = (-1).toChar
    })
    secretVector
  }

  /**
    * Compute A_Master. This is used for parameter Tau = 1
    *
    * @param sigma
    * @return
    */
  def create_A_master(sigma: Array[Byte]): Array[Array[Polynomial]] = {
    create_A(sigma.slice(0,kappa/8))
  }

  /**
    * Create the secret matrix A
    *
    * @param sigma the seed for the matrix
    * @return the matriy A
    */
  def create_A(sigma: Array[Byte]): Array[Array[Polynomial]] = {
    drgb_init(sigma.slice(0,kappa/8))
    val nb_poly = d / n
    val matrix = Array.ofDim[Polynomial](nb_poly, nb_poly)


    tau match {
      case 0 =>
        n match {
          case 1 =>
            drgb_init(sigma)
            (0 until d).foreach(a => (0 until nb_poly)
              .foreach(b => matrix(a)(b) = new Polynomial(Array[Char](drgb_sample_16_2(q)), false, q)))
          case _ =>
            drgb_init(sigma)
            (0 until nb_poly).foreach(a => {
              (0 until nb_poly).foreach(b => {
                matrix(a)(b) =  new Polynomial(drgb_sample_16_2_all_size(q,n), false, q)
              })
            })
        }
      case 1 =>
        val permut_vect = permutation_tau_1(sigma)
        (0 until d).foreach(i => (0 until d)
          .foreach(j => matrix(i)(j) = new Polynomial(Array[Char]((j + permut_vect(i) % d).toChar), false, q)))
      case 2 =>
        val a_master =  DRGB.drgb_sample_16_2_all_size(q,len_tau_2)  //(for (i <- 0 until len_tau_2) yield drgb_sample_16_2(q)).toArray
        val p2 = permutation_tau_2(sigma)
        /*
        (0 until d).foreach(i => (0 until d)
          .foreach(j => matrix(i)(j) = new Polynomial_char(Array[Char](a_master((j + p2(i)) % len_tau_2)), false, q)))
          */
        @tailrec
        def loop(p2 : Array[Char], a_master : Array[Char],i: Int, j : Int , matrix : Array[Array[Polynomial]]):  Array[Array[Polynomial]] ={
          matrix(i)(j) = new Polynomial(Array[Char](a_master((j + p2(i)) % len_tau_2)), false, q)
          j match{
            case _ if d-1 == j =>
              i match {
                case _ if d-1 == i => matrix
                case _ =>  loop(p2,a_master,i+1,0, matrix)
              }
            case _ => loop(p2, a_master, i , j +1, matrix)
          }
        }

        loop(p2,a_master,0,0,matrix)
    }
    matrix
  }


  /**
    * Create the secret matrix S_T
    */
  def create_S_T(seed: Array[Byte]): Array[Array[Polynomial]] = {
    drgb_init(seed.slice(0,kappa/8))
    var matrix = Array.ofDim[Char](n_bar, d)
    0 until n_bar foreach (a => matrix(a) = create_secret_vector(d, h))
    matrix
    charMatrixToPolyMatrix(matrix, q, f != 0 && xe != 0)
  }


  /**
    * Create the secret matreix S_T
    */
  def create_R_T(seed: Array[Byte]): Array[Array[Polynomial]] = {
    drgb_init(seed.slice(0,kappa/8))

    var matrix = Array.ofDim[Char](m_bar, d)
    0 until m_bar foreach (a => matrix(a) = create_secret_vector(d, h))
    matrix
    charMatrixToPolyMatrix(matrix, p, f != 0 && xe != 0 )
  }

  def charArrayToBitStringArray(a: Array[Char], element_size: Int): Array[BitString] = {
    val array = Array.ofDim[BitString](a.length)
    a.indices foreach (i => array(i) = {
      val b = new BitString("")
      val listBuffer = ListBuffer[Boolean]()
      Util.appendZeros(Integer.toBinaryString(a(i)), element_size) foreach (_ match {
        case '1' => listBuffer.prepend(true)
        case _ => listBuffer.prepend(false)
      })
      b.bits = listBuffer.toList
      b
    })
    array
  }

  def charMatrixToPolyMatrix(a: Array[Array[Char]], mod: Int, isXi : Boolean): Array[Array[Polynomial]] = {
    val nb_row = a.length
    val nb_col = a(0).length
    val nbr_elem = nb_col * nb_row
    val nb_poly_per_row = d / n
    val matrix = Array.ofDim[Polynomial](a.length, nb_col / n)
    var coefs = Array.ofDim[Char](n)
    (0 until nbr_elem).foreach(i => {
      coefs(i % n) = a(i / nb_col)(i % nb_col)
      if (n == 1 || i % d == (d-1)) {
        matrix(i/d)( (i/n) % (d/n)) = new Polynomial(coefs,isXi,mod)
        coefs = Array.ofDim[Char](n)
      }
    })
    matrix
  }

}
