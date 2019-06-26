import java.math.BigInteger
import java.security.SecureRandom

import core_char._
import org.bouncycastle.crypto.prng.EntropySource
import params._
import pack_char._
import xef._


object r5_cpa_pke_char {
  def keygen() = {
    val sigma =  Array.ofDim[Byte](kappa/8)
    Random.random.nextBytes(sigma)
    val A = create_A(sigma)
    val sk = Random.random.generateSeed(kappa / 8)
    val S_T = create_S_T(sigma)
    val S = S_T.transpose
    var B = mult_matrix(A, S, n.toInt, q.toInt)
    B = round_matrix(B, q_bits, p_bits)
    val pk = pack_pk(sigma, B)
    (pk, sk)
  }


  def encrypt(pk: BitString, m: BitString, rho: Array[Byte]) = {
    val sigma_B = unpack_pk(pk, kappa / 8, d / n * n_bar, p_bits)
      /*
    println("______________________")
    println(sigma_B._1)
    sigma_B._2 foreach(x => {
      x foreach(a => print(a.coef(0).toInt +", "))
      println
    })
    */

    val A = create_A(sigma_B._1.bitStringToString.getBytes())
    val R_T = create_R_T(rho)
    val A_T = A.transpose
    val R = R_T.transpose
    val U = mult_matrix(A_T, R, n, q)
    val U_T = U.transpose
    val B_T = round_matrix(U, q_bits, p_bits)
    val X = mult_matrix(B_T, R, n, p)
    val poly = new Polynome_char(sample_mu(X),false,Char.MaxValue)
    val x = round_matrix(Array[Array[Polynome_char]](Array[Polynome_char](poly)), p_bits, t_bits)
    var m1 = new BitString("")
    m1.bits  =  m.bits ::: m1.bits
    (0 until xe).foreach(i => m1.+:("0"))
   // m1.bits = t
    if (xe != 0) m1 = compute(m1)
    val v = add_msg(m1,b_bits,t_bits)
    val ct = pack_ct(U_T, v)
    ct
  }

  def decrypt(sk: Array[Byte], ct: BitString) = {
    val S_T = create_S_T(sk)
    val U_T_v = unpack_ct(ct, d/n * m_bar, p_bits, mu, t_bits)
    val U = U_T_v._1.transpose
    val v = decompress_matrix(U_T_v._2, p_bits, q_bits)
    val X_prime = mult_matrix(S_T, U, n, p)

    val m2 = diff_msg(v, charArrayTobitStringArray(sample_mu(X_prime)), p)
    val m3 = round_matrix(Array[Array[BitString]](m2), p_bits, b_bits)
    var m1 = pack_default(m3(0))
    var m4 = compute(m1)
    val m = fixerr(m4)
    m.bitStringToString
  }
}
