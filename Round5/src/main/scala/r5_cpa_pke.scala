
import core._
import params._
import r5_pack._
import xef._


object r5_cpa_pke {
  def keygen() = {
    val sigma = NIST_RNG.randombytes(kappa/8)
    val A = create_A(sigma)
    val sk = NIST_RNG.randombytes(kappa/8)
    val S_T = create_S_T(sk)
    var B = mult_matrix(A, S_T.transpose, n.toInt, q.toInt)
    B = round_matrix(B, q_bits, p_bits,h_1)
    val pk = pack_pk(sigma, B)
    (pk, sk)
  }


  def encrypt(pk: BitString, m: Array[Byte], rho: Array[Byte]) = {
    val (sigma,b) = unpack_pk(pk, kappa / 8, d * n_bar , p_bits)
    val A = create_A(sigma.toByteArray)
    val R = create_R_T(rho).transpose
    val U = mult_matrix( A.transpose, R, n, q)
    val U_T = round_matrix(U.transpose, q_bits, p_bits,h_2)
    val X = mult_matrix( b.transpose, R, n, p)
    val poly = new Polynomial(sample_mu(X),f != 0 && xe != 0 ,p)
    val x = round_matrix(Array[Array[Polynomial]](Array[Polynomial](poly)), p_bits, t_bits,h_2)
    var m1 = new BitString("")
    m1.bits  =  BitString.byteArrayToBitString(m,8).bits ::: List.fill(xe)(false)
    if (xe != 0) m1 = computeC(m1) else  m1.bits  =  m1.bits ::: List.fill(8)(false) //Need to add a byte to the end if xe = 0 in order to have enough byte to add_msg
    val v = add_msg(x,m1,b_bits,t_bits)
    val ct = pack_ct(U_T, v)
    ct
  }

  def decrypt(sk: Array[Byte], ct: BitString) = {
    val S_T = create_S_T(sk)
    var (u_t,v) = unpack_ct(ct, d * m_bar, p_bits, mu, t_bits)
    val v_dec = decompress_vector(v, p_bits, t_bits)
    val v_int = v_dec map (x => x.toChar)
    val X_prime = mult_matrix(S_T, u_t.transpose, n, p)
    val m2 = diff_msg(v_dec, charArrayToBitStringArray(sample_mu(X_prime),p_bits), p)
    val m3 = round_matrix(Array[Array[BitString]](m2), p_bits, b_bits,h_3)
    var m1 = pack(m3(0),b_bits)
    var m4 = computeC(m1)
    val m = fixerr(m4)
    m.bitStringToString
  }
}
