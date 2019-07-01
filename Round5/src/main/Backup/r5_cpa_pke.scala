import java.math.BigInteger
import java.security.SecureRandom

import core._
import params._
import pack._
import xef._

object r5_cpa_pke {
  def keygen = {
    val sigma = Random.random.generateSeed(kappa/8)
    val A = create_A(sigma)
    val sk = NIST_RNG.randombytes(kappa/8)
    val S_T = create_S_T()
    val S = S_T.transpose
    var B = mult_matrix(A, S, n.toInt, q.toInt)
    B = round_matrix(B, q_bits, p_bits)
    val pk = pack_pk((sigma.map(_.toChar)).mkString, B)
    (pk, sk)
  }

  def keygen_test(sigma:String) = {
    val random = new SecureRandom()
    val A = create_A(sigma.getBytes)
    val sk = random.generateSeed(kappa / 8)
    val S_T = create_S_T()
    val S = S_T.transpose
    var B = mult_matrix(A, S, n.toInt, q.toInt)
    B = round_matrix(B, q_bits, p_bits)
    val pk = pack_pk(sigma.map(_.toChar).mkString, B)
    (pk, sk)
  }

  def encrypt(pk: BitString, m: BitString, ct: BitString) = {
    val sigma_B = unpack_pk(pk, kappa / 8, d / n * n_bar, p_bits)
    val A = create_A(sigma_B._1.bitStringToString.getBytes())
    val R_T = create_R_T()
    val A_T = A.transpose
    val R = R_T.transpose
    val U = mult_matrix(A_T, R, n, q)
    val U_T = U.transpose
    val B_T = round_matrix(U, q_bits, p_bits)
    val X = mult_matrix(B_T, R, n, p)
    val x = round_matrix(Array[Array[BitString]](sample_mu(X)), p_bits, t_bits)
    var m1 = new BitString("")
    m1.bits :: m.bits
    (0 until xe).foreach(i => m1.+:("0"))
    m1 = compute(m1)
    val v = add_msg(m1,b_bits,t_bits)
    val ct = pack_ct(U_T, v)
    ct
  }

  def decrypt(sk: BitString, ct: BitString) = {
    val S_T = create_S_T()
    val U_T_v = unpack_ct(ct, d / n * m_bar, 16, mu, t_bits)
    val U = U_T_v._1.transpose
    val v = decompress_matrix(U_T_v._2, p_bits, q_bits)
    val X_prime = mult_matrix(S_T, U, n, p)
    val m2 = diff_msg(v, sample_mu(X_prime), p)
    val m3 = round_matrix(Array[Array[BitString]](m2), p_bits, b_bits)
    var m1 = pack_default(m3(0))
    m1 = compute(m1)
    val m = fixerr(m1)
    m.bitStringToString
  }
}
