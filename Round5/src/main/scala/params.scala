object   params {
    var tau = 0
    var security_level = ""

    lazy val d = security_level match{
        case "R5N1_1KEM_0d" => 594
        case "R5N1_3KEM_0d" => 881
        case "R5N1_5KEM_0d" => 1186
        case "R5ND_1KEM_0d" => 618
        case "R5ND_3KEM_0d" => 768
        case "R5ND_5KEM_0d" => 1018
        case "R5N1_1PKE_0d" => 636
        case "R5N1_3PKE_0d" => 876
        case "R5N1_5PKE_0d" => 1217
        case _ => 0
    } //number of coeffs in a Polynomial
    lazy val n = security_level match{
        case "R5N1_1KEM_0d" => 1
        case "R5N1_3KEM_0d" => 1
        case "R5N1_5KEM_0d" => 1
        case "R5ND_1KEM_0d" => 618
        case "R5ND_3KEM_0d" => 768
        case "R5ND_5KEM_0d" => 1018
        case "R5N1_1PKE_0d" => 1
        case "R5N1_3PKE_0d" => 1
        case "R5N1_5PKE_0d" => 1
        case _ => 0
    } //ring based if > 1
    lazy val h = security_level match{
        case "R5N1_1KEM_0d" => 238
        case "R5N1_3KEM_0d" => 238
        case "R5N1_5KEM_0d" => 712
        case "R5ND_1KEM_0d" => 104
        case "R5ND_3KEM_0d" => 384
        case "R5ND_5KEM_0d" => 428
        case "R5N1_1PKE_0d" => 114
        case "R5N1_3PKE_0d" => 446
        case "R5N1_5PKE_0d" => 462
        case _ => 0
    } //Number of non-zero values per column in secret matrix

    lazy val q_bits = security_level match{
        case "R5N1_1KEM_0d" => 13
        case "R5N1_3KEM_0d" => 13
        case "R5N1_5KEM_0d" => 15
        case "R5ND_1KEM_0d" => 11
        case "R5ND_3KEM_0d" => 13
        case "R5ND_5KEM_0d" => 14
        case "R5N1_1PKE_0d" => 12
        case "R5N1_3PKE_0d" => 15
        case "R5N1_5PKE_0d" => 15
        case _ => 0
    }
    lazy val q = math.pow(2,q_bits).toInt

    lazy val p_bits = security_level match{
        case "R5N1_1KEM_0d" => 10
        case "R5N1_3KEM_0d" => 10
        case "R5N1_5KEM_0d" => 12
        case "R5ND_1KEM_0d" => 8
        case "R5ND_3KEM_0d" => 9
        case "R5ND_5KEM_0d" => 9
        case "R5N1_1PKE_0d" => 9
        case "R5N1_3PKE_0d" => 11
        case "R5N1_5PKE_0d" => 12
        case _ => 0
    }
    lazy val p = math.pow(2,p_bits).toInt

    lazy val t_bits = security_level match{
        case "R5N1_1KEM_0d" => 7
        case "R5N1_3KEM_0d" => 7
        case "R5N1_5KEM_0d" => 7
        case "R5ND_1KEM_0d" => 4
        case "R5ND_3KEM_0d" => 4
        case "R5ND_5KEM_0d" => 4
        case "R5N1_1PKE_0d" => 6
        case "R5N1_3PKE_0d" => 7
        case "R5N1_5PKE_0d" => 9
        case _ => 0
    }
    lazy val t = math.pow(2,t_bits).toInt

    lazy val b_bits: Int  = security_level match{
        case "R5N1_1KEM_0d" => 3
        case "R5N1_3KEM_0d" => 3
        case "R5N1_5KEM_0d" => 4
        case "R5ND_1KEM_0d" => 1
        case "R5ND_3KEM_0d" => 1
        case "R5ND_5KEM_0d" => 1
        case "R5N1_1PKE_0d" => 2
        case "R5N1_3PKE_0d" => 3
        case "R5N1_5PKE_0d" => 4
        case _ => 0
    }
    lazy val b = math.pow(2,b_bits).toInt
    lazy val n_bar = security_level match{
        case "R5N1_1KEM_0d" => 7
        case "R5N1_3KEM_0d" => 8
        case "R5N1_5KEM_0d" => 8
        case "R5ND_1KEM_0d" => 1
        case "R5ND_3KEM_0d" => 1
        case "R5ND_5KEM_0d" => 1
        case "R5N1_1PKE_0d" => 8
        case "R5N1_3PKE_0d" => 8
        case "R5N1_5PKE_0d" => 8

        case _ => 0
    }
    lazy val m_bar = security_level match{
        case "R5N1_1KEM_0d" => 7
        case "R5N1_3KEM_0d" => 8
        case "R5N1_5KEM_0d" => 8
        case "R5ND_1KEM_0d" => 1
        case "R5ND_3KEM_0d" => 1
        case "R5ND_5KEM_0d" => 1
        case "R5N1_1PKE_0d" => 8
        case "R5N1_3PKE_0d" => 8
        case "R5N1_5PKE_0d" => 8
        case _ => 0
    }


    lazy val f = security_level match{
        case "R5N1_1KEM_0d" => 0
        case "R5N1_3KEM_0d" => 0
        case "R5N1_5KEM_0d" => 0
        case "R5ND_1KEM_0d" => 0
        case "R5ND_3KEM_0d" => 0
        case "R5ND_5KEM_0d" => 0
        case "R5N1_1PKE_0d" => 0
        case "R5N1_3PKE_0d" => 0
        case "R5N1_5PKE_0d" => 0
        case _ => 0
    }


    lazy val xe = security_level match{
        case "R5N1_1KEM_0d" => 0
        case "R5N1_3KEM_0d" => 0
        case "R5N1_5KEM_0d" => 0
        case "R5ND_1KEM_0d" => 0
        case "R5ND_3KEM_0d" => 0
        case "R5ND_5KEM_0d" => 0
        case "R5N1_1PKE_0d" => 0
        case "R5N1_3PKE_0d" => 0
        case "R5N1_5PKE_0d" => 0
        case _ => 0
    }
    lazy val kappa = security_level match{
        case "R5N1_1KEM_0d" => 128
        case "R5N1_3KEM_0d" => 192
        case "R5N1_5KEM_0d" => 256
        case "R5ND_1KEM_0d" => 128
        case "R5ND_3KEM_0d" => 192
        case "R5ND_5KEM_0d" => 256
        case "R5N1_1PKE_0d" => 128
        case "R5N1_3PKE_0d" => 192
        case "R5N1_5PKE_0d" => 264
        case _ => 0
    }

    lazy val mu = security_level match{
        case "R5N1_1KEM_0d" => 43
        case "R5N1_3KEM_0d" => 64
        case "R5N1_5KEM_0d" => 64
        case "R5ND_1KEM_0d" => 128
        case "R5ND_3KEM_0d" => 192
        case "R5ND_5KEM_0d" => 156
        case "R5N1_1PKE_0d" => 64
        case "R5N1_3PKE_0d" => 64
        case "R5N1_5PKE_0d" => 64
        case _ => 0
    }


    lazy val z = math.max(p,t*q/p)
    lazy val h_1 = q/(2*p) //rounding constant
    lazy val h_2 = q/(2*z)
    lazy val h_3 = p/(2*t) + p/(2*b) - q/(2*z)
    lazy val h_4 = q/(2*p) + q/(2*z)
    lazy val len_tau_2 = math.pow(2,11).toInt


}
