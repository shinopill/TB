
import params.{f, m_bar, n, n_bar, p, p_bits, q, xe}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object r5_pack {

  /**
    * Function to serialise an array of bitString to a String
    *
    * @param a the array to seraialise
    * @return a string value of the array a
    */
  def pack(a: Array[BitString],element_size:Int): BitString = {
    var b = new BitString("")
    a.indices foreach (x => b = b.::(a(x)))
    b
  }

  /**
    * Function that serialize the component sigma and matrix B
    *
    * @param sigma
    * @param B
    * @return The string corresponding to sigma||B
    */
  def pack_pk(sigma: Array[Byte], B: Array[Array[Polynomial]]): BitString = {
    val t = ListBuffer[Boolean]()
    B.foreach(x => x.foreach(y => y.coef.foreach(z => {
      Util.appendZeros(z.toBinaryString, p_bits).reverse.foreach(_ match{
        case '1' => t.append(true)
        case '0' => t.append(false)
      })
    })))
    val result = new BitString("")
    sigma.reverse foreach (x => {
      Util.appendZeros(x.toBinaryString, 8) foreach (_ match{
        case '1' => t.prepend(true)
        case '0' => t.prepend(false)
      })
    })
    result.bits =  t.toList
    result
  }


  /**
    * Function to serialize component matrix U and vector v
    *
    * @param U
    * @param v
    * @return a string corresponding to U||v
    */
  def pack_ct(U: Array[Array[Polynomial]], v: Array[BitString]): BitString = {
    var b = new BitString("")
    val t = ListBuffer[Boolean]()
    U.foreach(x => x.foreach(y => y.coef.foreach(z => {
      Util.appendZeros(z.toBinaryString, p_bits).reverse.foreach(_ match{
        case '1' => t.append(true)
        case '0' => t.append(false)
      })
    })))
    (0 until ((8-(t.length % 8)) %8)) foreach (_ => t.append(false))
    v.foreach(x => t.appendAll(x.bits))
    (0 until ((8-(t.length % 8)) %8)) foreach (_ => t.append(false))
    b.bits = t.toList
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
  def unpack_pk(pk: BitString, sigma_size: Int, B_element: Int, B_element_bits: Int): (BitString, Array[Array[Polynomial]]) = {
    val b_nb_row = B_element / n_bar / n
    val matrix = Array.ofDim[Polynomial](b_nb_row, n_bar)
    val unpack = loop_matrix(pk.bits.drop(sigma_size * 8 ), B_element_bits,ListBuffer[Boolean](),0,0,matrix,0,new Polynomial(Array.ofDim[Char](n), f != 0 && xe != 0, q))
    val t = new BitString("")
    t.bits = pk.bits.take(sigma_size * 8)
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
    val matrix = Array.ofDim[Polynomial](m_bar, u_nb_col)
    val U_size = u_elem * u_size + ((8- (u_elem * u_size % 8)) % 8)
    val unpack = loop_matrix(ct.bits.take(u_elem * u_size), u_size , ListBuffer[Boolean](),0,0,matrix,0,new Polynomial(Array.ofDim[Char](n), false, p))

    @tailrec
    def loop_array(xs : List[Boolean], acc : ListBuffer[Boolean],index_vector : Int ,vector: Array[BitString]): Array[BitString] = {
      xs match {
        case Nil =>
         if (acc.length == v_size) { // check if last is padding or not
            val b = new BitString("")
            b.bits = acc.toList
            vector(index_vector) = b
          }
          vector
        case el :: els =>  if (acc.length == v_size){
          val b = new BitString("")
          b.bits = acc.toList
          vector(index_vector)  = b
          loop_array(els, ListBuffer[Boolean](el),index_vector + 1,vector)
        }else{
          acc.append(el)
          loop_array(els, acc,index_vector,vector)
        }
      }
    }

    val v = Array.ofDim[BitString](v_elem)
    val unpack_vect = loop_array(ct.bits slice(U_size, U_size + v_size * v_elem), ListBuffer[Boolean](),0,v)
    (unpack, unpack_vect)
  }


   @tailrec
   private def loop_matrix(xs : List[Boolean], elem_size : Int, acc : ListBuffer[Boolean], row : Int, col : Int, matrix : Array[Array[Polynomial]], index_coef : Int, poly : Polynomial) : Array[Array[Polynomial]]  = {
    xs match {
      case Nil => {
        // If we have the perfect length we need to add the last polynomials else it is just padding
        if(acc.length == elem_size) {
          poly.coef(index_coef) = Integer.parseInt(BitString.booleanToString(acc.toList), 2).toChar
          matrix(row)(col) = poly
        }
        matrix
      }
      case el :: els =>
        if (acc.length == elem_size){
          poly.coef(index_coef) =  Integer.parseInt(BitString.booleanToString(acc.toList),2).toChar
          poly.coef.length match {
            case _ if index_coef  == params.n -1 => {
              matrix(row)(col) = poly
              col match {
                case _ if matrix(0).length -1 == col => loop_matrix(els,elem_size, ListBuffer[Boolean](el),row+1,0, matrix,0,new Polynomial(Array.ofDim[Char](n), false, p))
                case _ => loop_matrix(els, elem_size,ListBuffer[Boolean](el),row,col+1, matrix,0,new Polynomial(Array.ofDim[Char](n), false, p))
              }
            }
            case _ =>
              loop_matrix(els,elem_size, ListBuffer[Boolean](el),row,col, matrix, index_coef + 1 ,poly)
          }
        }else{
          acc.prepend(el)
          loop_matrix(els, elem_size, acc,row,col, matrix,index_coef,poly)
        }

    }
  }



}
