import java.math.BigInteger
import java.security.SecureRandom

import core_char._
import params._
import pack_char._
import xef._

//TODO add sigma and other to prng
object r5_cpa_pke_char {
  def keygen = {
    val random = new SecureRandom()
    val sigma = random.generateSeed(kappa / 8)
    val A = create_A(sigma)
    val sk = random.generateSeed(kappa / 8)
    val S_T = create_S_T(sigma)
    val S = S_T.transpose
    val S_poly = charMatrixToPolyMatrix(S)
    var B = mult_matrix(A, S_poly, n.toInt, q.toInt)
    B = round_matrix(B, q_bits, p_bits)
    val pk = pack_pk(sigma, B)
    (pk, sk)
  }

  def keygen_test(sigma:Array[Byte]) = {
    val random = new SecureRandom()
    val A = create_A(sigma)
    val sk = random.generateSeed(kappa / 8)
    val S_T = create_S_T(sigma)
    val S = S_T.transpose
    val S_poly = charMatrixToPolyMatrix(S)
    var B = mult_matrix(A, S_poly, n.toInt, q.toInt)
    println(testIfAllinMod(B))
    B = round_matrix(B, q_bits, p_bits)
    B foreach(x => {
      x foreach(a => print(a.coef(0).toInt + ", "))
      println
    })
    val pk = pack_pk(sigma, B)
    println()
    println("LEN OF PK : " + pk.bits.length)
    (pk, sk)
  }

  def encrypt(pk: BitString, m: BitString, rho: Array[Byte]) = {
    val sigma_B = unpack_pk(pk, kappa / 8, d / n * n_bar, p_bits)
    println("______________________")
    println(sigma_B._1)
    sigma_B._2 foreach(x => {
      x foreach(a => print(a.coef(0).toInt +", "))
      println
    })
    val A = create_A(sigma_B._1.bitStringToString.getBytes())
    val R_T = create_R_T(rho)
    val A_T = A.transpose
    val R = R_T.transpose
    val R_poly = charMatrixToPolyMatrix(R)
    val U = mult_matrix(A_T, R_poly, n, q)
    val U_T = U.transpose
    val B_T = round_matrix(U, q_bits, p_bits)
    val X = mult_matrix(B_T, R_poly, n, p)
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
    val S_T_poly = charMatrixToPolyMatrix(S_T)
    val U_T_v = unpack_ct(ct, d/n * m_bar, p_bits, mu, t_bits)
    val U = U_T_v._1.transpose
    val v = decompress_matrix(U_T_v._2, p_bits, q_bits)
    val X_prime = mult_matrix(S_T_poly, U, n, p)

    val m2 = diff_msg(v, charArrayTobitStringArray(sample_mu(X_prime)), p)
    val m3 = round_matrix(Array[Array[BitString]](m2), p_bits, b_bits)
    var m1 = pack_default(m3(0))
    m1 = compute(m1)
    val m = fixerr(m1)
    m.bitStringToString
  }
}
