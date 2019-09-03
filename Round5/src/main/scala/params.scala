/**
  * @author Florent Piller
  */

object  params {
    var tau = 0

    var security_level = ""

    def d = security_level match{
        case "R5N1_1KEM_0d" => 594
        case "R5N1_3KEM_0d" => 881
        case "R5N1_5KEM_0d" => 1186
        case "R5ND_1KEM_0d" => 618
        case "R5ND_3KEM_0d" => 786
        case "R5ND_5KEM_0d" => 1018
        case "R5ND_1KEM_5d" => 490
        case "R5ND_3KEM_5d" => 756
        case "R5ND_5KEM_5d" => 940
        case "R5N1_1PKE_0d" => 636
        case "R5N1_3PKE_0d" => 876
        case "R5N1_5PKE_0d" => 1217
        case "R5ND_1PKE_0d" => 586
        case "R5ND_3PKE_0d" => 852
        case "R5ND_5PKE_0d" => 1170
        case "R5ND_1PKE_5d" => 508
        case "R5ND_3PKE_5d" => 756
        case "R5ND_5PKE_5d" => 946
        case "R5N1_3PKE_0smallCT" => 757
        case "R5ND_0KEM_2iot" => 372
        case "R5ND_1KEM_4longkey" => 490
        case _ => throw new Exception("Wrong security level or security level not initialized")
    } //number of coeffs in a Polynomial

    lazy val n = security_level match{
        case "R5N1_1KEM_0d" => 1
        case "R5N1_3KEM_0d" => 1
        case "R5N1_5KEM_0d" => 1
        case "R5ND_1KEM_0d" => 618
        case "R5ND_3KEM_0d" => 786
        case "R5ND_5KEM_0d" => 1018
        case "R5ND_1KEM_5d" => 490
        case "R5ND_3KEM_5d" => 756
        case "R5ND_5KEM_5d" => 940
        case "R5N1_1PKE_0d" => 1
        case "R5N1_3PKE_0d" => 1
        case "R5N1_5PKE_0d" => 1
        case "R5ND_1PKE_0d" => 586
        case "R5ND_3PKE_0d" => 852
        case "R5ND_5PKE_0d" => 1170
        case "R5ND_1PKE_5d" => 508
        case "R5ND_3PKE_5d" => 756
        case "R5ND_5PKE_5d" => 946
        case "R5N1_3PKE_0smallCT" => 1
        case "R5ND_0KEM_2iot" => 372
        case "R5ND_1KEM_4longkey" => 490
        case _ => throw new Exception("Wrong security level or security level not initialized")
    } //ring based if > 1

    lazy val h = security_level match{
        case "R5N1_1KEM_0d" => 238
        case "R5N1_3KEM_0d" => 238
        case "R5N1_5KEM_0d" => 712
        case "R5ND_1KEM_0d" => 104
        case "R5ND_3KEM_0d" => 384
        case "R5ND_5KEM_0d" => 428
        case "R5ND_1KEM_5d" => 162
        case "R5ND_3KEM_5d" => 242
        case "R5ND_5KEM_5d" => 414
        case "R5N1_1PKE_0d" => 114
        case "R5N1_3PKE_0d" => 446
        case "R5N1_5PKE_0d" => 462
        case "R5ND_1PKE_0d" => 182
        case "R5ND_3PKE_0d" => 212
        case "R5ND_5PKE_0d" => 222
        case "R5ND_1PKE_5d" => 136
        case "R5ND_3PKE_5d" => 242
        case "R5ND_5PKE_5d" => 388
        case "R5N1_3PKE_0smallCT" => 378
        case "R5ND_0KEM_2iot" => 178
        case "R5ND_1KEM_4longkey" => 162
        case _ => throw new Exception("Wrong security level or security level not initialized")
    } //Number of non-zero values per column in secret matrix

    lazy val q_bits = security_level match{
        case "R5N1_1KEM_0d" => 13
        case "R5N1_3KEM_0d" => 13
        case "R5N1_5KEM_0d" => 15
        case "R5ND_1KEM_0d" => 11
        case "R5ND_3KEM_0d" => 13
        case "R5ND_5KEM_0d" => 14
        case "R5ND_1KEM_5d" => 10
        case "R5ND_3KEM_5d" => 12
        case "R5ND_5KEM_5d" => 12
        case "R5N1_1PKE_0d" => 12
        case "R5N1_3PKE_0d" => 15
        case "R5N1_5PKE_0d" => 15
        case "R5ND_1PKE_0d" => 13
        case "R5ND_3PKE_0d" => 12
        case "R5ND_5PKE_0d" => 13
        case "R5ND_1PKE_5d" => 10
        case "R5ND_3PKE_5d" => 12
        case "R5ND_5PKE_5d" => 11
        case "R5N1_3PKE_0smallCT" => 14
        case "R5ND_0KEM_2iot" => 11
        case "R5ND_1KEM_4longkey" => 10
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }
    lazy val q = math.pow(2,q_bits).toInt

    lazy val p_bits = security_level match{
        case "R5N1_1KEM_0d" => 10
        case "R5N1_3KEM_0d" => 10
        case "R5N1_5KEM_0d" => 12
        case "R5ND_1KEM_0d" => 8
        case "R5ND_3KEM_0d" => 9
        case "R5ND_5KEM_0d" => 9
        case "R5ND_1KEM_5d" => 7
        case "R5ND_3KEM_5d" => 8
        case "R5ND_5KEM_5d" => 8
        case "R5N1_1PKE_0d" => 9
        case "R5N1_3PKE_0d" => 11
        case "R5N1_5PKE_0d" => 12
        case "R5ND_1PKE_0d" => 9
        case "R5ND_3PKE_0d" => 9
        case "R5ND_5PKE_0d" => 9
        case "R5ND_1PKE_5d" => 7
        case "R5ND_3PKE_5d" => 8
        case "R5ND_5PKE_5d" => 8
        case "R5N1_3PKE_0smallCT" => 9
        case "R5ND_0KEM_2iot" => 7
        case "R5ND_1KEM_4longkey" => 7
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }
    lazy val p = math.pow(2,p_bits).toInt

    lazy val t_bits = security_level match{
        case "R5N1_1KEM_0d" => 7
        case "R5N1_3KEM_0d" => 7
        case "R5N1_5KEM_0d" => 7
        case "R5ND_1KEM_0d" => 4
        case "R5ND_3KEM_0d" => 4
        case "R5ND_5KEM_0d" => 4
        case "R5ND_1KEM_5d" => 3
        case "R5ND_3KEM_5d" => 2
        case "R5ND_5KEM_5d" => 2
        case "R5N1_1PKE_0d" => 6
        case "R5N1_3PKE_0d" => 7
        case "R5N1_5PKE_0d" => 9
        case "R5ND_1PKE_0d" => 4
        case "R5ND_3PKE_0d" => 5
        case "R5ND_5PKE_0d" => 5
        case "R5ND_1PKE_5d" => 4
        case "R5ND_3PKE_5d" => 3
        case "R5ND_5PKE_5d" => 5
        case "R5N1_3PKE_0smallCT" => 4
        case "R5ND_0KEM_2iot" => 3
        case "R5ND_1KEM_4longkey" => 3
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }
    lazy val t = math.pow(2,t_bits).toInt

    lazy val b_bits: Int  = security_level match{
        case "R5N1_1KEM_0d" => 3
        case "R5N1_3KEM_0d" => 3
        case "R5N1_5KEM_0d" => 4
        case "R5ND_1KEM_0d" => 1
        case "R5ND_3KEM_0d" => 1
        case "R5ND_5KEM_0d" => 1
        case "R5ND_1KEM_5d" => 1
        case "R5ND_3KEM_5d" => 1
        case "R5ND_5KEM_5d" => 1
        case "R5N1_1PKE_0d" => 2
        case "R5N1_3PKE_0d" => 3
        case "R5N1_5PKE_0d" => 4
        case "R5ND_1PKE_0d" => 1
        case "R5ND_3PKE_0d" => 1
        case "R5ND_5PKE_0d" => 1
        case "R5ND_1PKE_5d" => 1
        case "R5ND_3PKE_5d" => 1
        case "R5ND_5PKE_5d" => 1
        case "R5N1_3PKE_0smallCT" => 1
        case "R5ND_0KEM_2iot" => 1
        case "R5ND_1KEM_4longkey" => 1
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }
    lazy val b = math.pow(2,b_bits).toInt
    lazy val n_bar = security_level match{
        case "R5N1_1KEM_0d" => 7
        case "R5N1_3KEM_0d" => 8
        case "R5N1_5KEM_0d" => 8
        case "R5ND_1KEM_0d" => 1
        case "R5ND_3KEM_0d" => 1
        case "R5ND_5KEM_0d" => 1
        case "R5ND_1KEM_5d" => 1
        case "R5ND_3KEM_5d" => 1
        case "R5ND_5KEM_5d" => 1
        case "R5N1_1PKE_0d" => 8
        case "R5N1_3PKE_0d" => 8
        case "R5N1_5PKE_0d" => 8
        case "R5ND_1PKE_0d" => 1
        case "R5ND_3PKE_0d" => 1
        case "R5ND_5PKE_0d" => 1
        case "R5ND_1PKE_5d" => 1
        case "R5ND_3PKE_5d" => 1
        case "R5ND_5PKE_5d" => 1
        case "R5N1_3PKE_0smallCT" => 192
        case "R5ND_0KEM_2iot" => 1
        case "R5ND_1KEM_4longkey" => 1
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }
    lazy val m_bar = security_level match{
        case "R5N1_1KEM_0d" => 7
        case "R5N1_3KEM_0d" => 8
        case "R5N1_5KEM_0d" => 8
        case "R5ND_1KEM_0d" => 1
        case "R5ND_3KEM_0d" => 1
        case "R5ND_5KEM_0d" => 1
        case "R5ND_1KEM_5d" => 1
        case "R5ND_3KEM_5d" => 1
        case "R5ND_5KEM_5d" => 1
        case "R5N1_1PKE_0d" => 8
        case "R5N1_3PKE_0d" => 8
        case "R5N1_5PKE_0d" => 8
        case "R5ND_1PKE_0d" => 1
        case "R5ND_3PKE_0d" => 1
        case "R5ND_5PKE_0d" => 1
        case "R5ND_1PKE_5d" => 1
        case "R5ND_3PKE_5d" => 1
        case "R5ND_5PKE_5d" => 1
        case "R5N1_3PKE_0smallCT" => 1
        case "R5ND_0KEM_2iot" => 1
        case "R5ND_1KEM_4longkey" => 1
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }


    lazy val f = security_level match{
        case "R5N1_1KEM_0d" => 0
        case "R5N1_3KEM_0d" => 0
        case "R5N1_5KEM_0d" => 0
        case "R5ND_1KEM_0d" => 0
        case "R5ND_3KEM_0d" => 0
        case "R5ND_5KEM_0d" => 0
        case "R5ND_1KEM_5d" => 5
        case "R5ND_3KEM_5d" => 5
        case "R5ND_5KEM_5d" => 5
        case "R5N1_1PKE_0d" => 0
        case "R5N1_3PKE_0d" => 0
        case "R5N1_5PKE_0d" => 0
        case "R5ND_1PKE_0d" => 0
        case "R5ND_3PKE_0d" => 0
        case "R5ND_5PKE_0d" => 0
        case "R5ND_1PKE_5d" => 5
        case "R5ND_3PKE_5d" => 5
        case "R5ND_5PKE_5d" => 5
        case "R5N1_3PKE_0smallCT" => 0
        case "R5ND_0KEM_2iot" => 2
        case "R5ND_1KEM_4longkey" => 4
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }


    lazy val xe = security_level match{
        case "R5N1_1KEM_0d" => 0
        case "R5N1_3KEM_0d" => 0
        case "R5N1_5KEM_0d" => 0
        case "R5ND_1KEM_0d" => 0
        case "R5ND_3KEM_0d" => 0
        case "R5ND_5KEM_0d" => 0
        case "R5ND_1KEM_5d" => 190
        case "R5ND_3KEM_5d" => 218
        case "R5ND_5KEM_5d" => 234
        case "R5N1_1PKE_0d" => 0
        case "R5N1_3PKE_0d" => 0
        case "R5N1_5PKE_0d" => 0
        case "R5ND_1PKE_0d" => 0
        case "R5ND_3PKE_0d" => 0
        case "R5ND_5PKE_0d" => 0
        case "R5ND_1PKE_5d" => 190
        case "R5ND_3PKE_5d" => 218
        case "R5ND_5PKE_5d" => 234
        case "R5N1_3PKE_0smallCT" => 0
        case "R5ND_0KEM_2iot" => 53
        case "R5ND_1KEM_4longkey" => 163
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }
    lazy val kappa = security_level match{
        case "R5N1_1KEM_0d" => 128
        case "R5N1_3KEM_0d" => 192
        case "R5N1_5KEM_0d" => 256
        case "R5ND_1KEM_0d" => 128
        case "R5ND_3KEM_0d" => 192
        case "R5ND_5KEM_0d" => 256
        case "R5ND_1KEM_5d" => 128
        case "R5ND_3KEM_5d" => 192
        case "R5ND_5KEM_5d" => 256
        case "R5N1_1PKE_0d" => 128
        case "R5N1_3PKE_0d" => 192
        case "R5N1_5PKE_0d" => 256
        case "R5ND_1PKE_0d" => 128
        case "R5ND_3PKE_0d" => 192
        case "R5ND_5PKE_0d" => 256
        case "R5ND_1PKE_5d" => 128
        case "R5ND_3PKE_5d" => 192
        case "R5ND_5PKE_5d" => 256
        case "R5N1_3PKE_0smallCT" => 192
        case "R5ND_0KEM_2iot" => 128
        case "R5ND_1KEM_4longkey" => 192
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }

    lazy val mu = security_level match{
        case "R5N1_1KEM_0d" => 43
        case "R5N1_3KEM_0d" => 64
        case "R5N1_5KEM_0d" => 64
        case "R5ND_1KEM_0d" => 128
        case "R5ND_3KEM_0d" => 192
        case "R5ND_5KEM_0d" => 256
        case "R5ND_1KEM_5d" => 318
        case "R5ND_3KEM_5d" => 410
        case "R5ND_5KEM_5d" => 490
        case "R5N1_1PKE_0d" => 64
        case "R5N1_3PKE_0d" => 64
        case "R5N1_5PKE_0d" => 64
        case "R5ND_1PKE_0d" => 128
        case "R5ND_3PKE_0d" => 192
        case "R5ND_5PKE_0d" => 256
        case "R5ND_1PKE_5d" => 318
        case "R5ND_3PKE_5d" => 410
        case "R5ND_5PKE_5d" => 490
        case "R5N1_3PKE_0smallCT" => 192
        case "R5ND_0KEM_2iot" => 181
        case "R5ND_1KEM_4longkey" => 355
        case _ => throw new Exception("Wrong security level or security level not initialized")
    }


    lazy val z = math.max(p,t*q/p)
    lazy val h_1 = q/(2*p) //rounding constant
    lazy val h_2 = q/(2*z)
    lazy val h_3 = p/(2*t) + p/(2*b) - q/(2*z)
    lazy val h_4 = q/(2*p) + q/(2*z)
    lazy val len_tau_2 = math.pow(2,11).toInt


}
