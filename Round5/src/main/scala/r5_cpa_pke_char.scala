
import core_char._
import params._
import pack_char._
import xef._


object r5_cpa_pke_char {
  def keygen() = {
    val sigma = NIST_RNG.randombytes(kappa/8)
     //Util.printHex(sigma)
    val A = create_A(sigma)

  //  A.foreach(b => b.foreach(c => Util.printHex(c.coef)))
    val sk = NIST_RNG.randombytes(kappa/8)
   // Util.printHex(sk)
    val S_T = create_S_T(sk)
    val S = S_T.transpose
    //S.foreach(b => b.foreach(c => Util.printHex(c.coef)))
    var B = mult_matrix(A, S, n.toInt, q.toInt)
 //   B.foreach(c => c.foreach(a => a.coef.foreach(q => print(q.toInt + " "))))
    B = round_matrix(B, q_bits, p_bits,h_1)
    val pk = pack_pk(sigma, B)

    (pk, sk)
  }


  def encrypt(pk: BitString, m: Array[Byte], rho: Array[Byte]) = {
    val (sigma,b) = unpack_pk(pk, kappa / 8, d / n * n_bar, p_bits)
    val A = create_A(sigma.toByteArray)
    val R_T = create_R_T(rho)
    val A_T = A.transpose
    val R = R_T.transpose
    val U = mult_matrix(A_T, R, n, q)
    var U_T = U.transpose
    U_T = round_matrix(U_T, q_bits, p_bits,h_2)
    val B_T = b.transpose
    val X = mult_matrix(B_T, R, n, p)
    val poly = new Polynomial_char(sample_mu(X),false,p)
    val x = round_matrix(Array[Array[Polynomial_char]](Array[Polynomial_char](poly)), p_bits, t_bits,h_2)
    if(xe == 0 ) m :+ 0.toByte

    var m1 = new BitString("")
    val m_bits = BitString.byteArrayToBitString(m,8)
    m1.bits  =  m1.bits ::: m_bits.bits
    (0 until xe).foreach(i => m1.:+("0"))
    if (xe != 0) m1 = compute(m1) else (0 until 8) foreach(_ => m1.:+("0")) //Need to add a byte to the end if xe = 0 in order to have enough byte to add_msg
    val y = m1.toByteArray
    val v = add_msg(x,m1,b_bits,t_bits)
    val z= v map (a => a.toInt)
    val ct = pack_ct(U_T, v)
    ct
  }

  def decrypt(sk: Array[Byte], ct: BitString) = {
    val S_T = create_S_T(sk)
    var (u_t,v) = unpack_ct(ct, d/n * m_bar, p_bits, mu, t_bits)

    val U = u_t.transpose
    val v_dec = decompress_vector(v, p_bits, t_bits)
    val v_int = v_dec map (x => x.toInt.toChar)
    val X_prime = mult_matrix(S_T, U, n, p)

    val m2 = diff_msg(v_dec, charArrayToBitStringArray(sample_mu(X_prime),p_bits), p)
    val x = m2 map (a => a.toInt.toChar)
    val m3 = round_matrix(Array[Array[BitString]](m2), p_bits, b_bits,h_3)
    val x2 = m3 map (a => a map (b => b.toInt.toChar))
    var m1 = pack(m3(0),b_bits)
    val x3 = m1.toByteArray
    var m4 = compute(m1)
    val m = fixerr(m4)
    val x5 = m.toByteArray
    m.bits = m.bits.slice(0,kappa)
    m.bitStringToString
  }
}
