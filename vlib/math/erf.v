module math

/*
*                           x
 *                    2      |\
 *     erf(x)  =  ---------  | exp(-t*t)dt
 *                 sqrt(pi) \|
 *                           0
 *
 *     erfc(x) =  1-erf(x)
 *  Note that
 *              erf(-x) = -erf(x)
 *              erfc(-x) = 2 - erfc(x)
 *
 * Method:
 *      1. For |x| in [0, 0.84375]
 *          erf(x)  = x + x*R(x**2)
 *          erfc(x) = 1 - erf(x)           if x in [-.84375,0.25]
 *                  = 0.5 + ((0.5-x)-x*R)  if x in [0.25,0.84375]
 *         where R = P/Q where P is an odd poly of degree 8 and
 *         Q is an odd poly of degree 10.
 *                                               -57.90
 *                      | R - (erf(x)-x)/x | <= 2
 *
 *
 *         Remark. The formula is derived by noting
 *          erf(x) = (2/sqrt(pi))*(x - x**3/3 + x**5/10 - x**7/42 + ....)
 *         and that
 *          2/sqrt(pi) = 1.128379167095512573896158903121545171688
 *         is close to one. The interval is chosen because the fix
 *         point of erf(x) is near 0.6174 (i.e., erf(x)=x when x is
 *         near 0.6174), and by some experiment, 0.84375 is chosen to
 *         guarantee the error is less than one ulp for erf.
 *
 *      2. For |x| in [0.84375,1.25], let s_ = |x| - 1, and
 *         c = 0.84506291151 rounded to single (24 bits)
 *              erf(x)  = sign(x) * (c  + P1(s_)/Q1(s_))
 *              erfc(x) = (1-c)  - P1(s_)/Q1(s_) if x > 0
 *                        1+(c+P1(s_)/Q1(s_))    if x < 0
 *              |P1/Q1 - (erf(|x|)-c)| <= 2**-59.06
 *         Remark: here we use the taylor series expansion at x=1.
 *              erf(1+s_) = erf(1) + s_*Poly(s_)
 *                       = 0.845.. + P1(s_)/Q1(s_)
 *         That is, we use rational approximation to approximate
 *                      erf(1+s_) - (c = (single)0.84506291151)
 *         Note that |P1/Q1|< 0.078 for x in [0.84375,1.25]
 *         where
 *              P1(s_) = degree 6 poly in s_
 *              Q1(s_) = degree 6 poly in s_
 *
 *      3. For x in [1.25,1/0.35(~2.857143)],
 *              erfc(x) = (1/x)*exp(-x*x-0.5625+R1/s1)
 *              erf(x)  = 1 - erfc(x)
 *         where
 *              R1(z) = degree 7 poly in z, (z=1/x**2)
 *              s1(z) = degree 8 poly in z
 *
 *      4. For x in [1/0.35,28]
 *              erfc(x) = (1/x)*exp(-x*x-0.5625+R2/s2) if x > 0
 *                      = 2.0 - (1/x)*exp(-x*x-0.5625+R2/s2) if -6<x<0
 *                      = 2.0 - tiny            (if x <= -6)
 *              erf(x)  = sign(x)*(1.0 - erfc(x)) if x < 6, else
 *              erf(x)  = sign(x)*(1.0 - tiny)
 *         where
 *              R2(z) = degree 6 poly in z, (z=1/x**2)
 *              s2(z) = degree 7 poly in z
 *
 *      Note1:
 *         To compute exp(-x*x-0.5625+R/s), let s_ be a single
 *         precision number and s_ := x; then
 *              -x*x = -s_*s_ + (s_-x)*(s_+x)
 *              exp(-x*x-0.5626+R/s) =
 *                      exp(-s_*s_-0.5625)*exp((s_-x)*(s_+x)+R/s);
 *      Note2:
 *         Here 4 and 5 make use of the asymptotic series
 *                        exp(-x*x)
 *              erfc(x) ~ ---------- * ( 1 + Poly(1/x**2) )
 *                        x*sqrt(pi)
 *         We use rational approximation to approximate
 *              g(s_)=f(1/x**2) = log(erfc(x)*x) - x*x + 0.5625
 *         Here is the error bound for R1/s1 and R2/s2
 *              |R1/s1 - f(x)|  < 2**(-62.57)
 *              |R2/s2 - f(x)|  < 2**(-61.52)
 *
 *      5. For inf > x >= 28
 *              erf(x)  = sign(x) *(1 - tiny)  (raise inexact)
 *              erfc(x) = tiny*tiny (raise underflow) if x > 0
 *                      = 2 - tiny if x<0
 *
 *      7. special case:
 *              erf(0)  = 0, erf(inf)  = 1, erf(-inf) = -1,
 *              erfc(0) = 1, erfc(inf) = 0, erfc(-inf) = 2,
 *              erfc/erf(nan) is nan
*/
const (
	erx  = 8.45062911510467529297e-01 // 0x3FEB0AC160000000
	// Coefficients for approximation to  erf in [0, 0.84375]
	efx  = 1.28379167095512586316e-01 // 0x3FC06EBA8214DB69
	efx8 = 1.02703333676410069053e+00 // 0x3FF06EBA8214DB69
	pp0  = 1.28379167095512558561e-01 // 0x3FC06EBA8214DB68
	pp1  = -3.25042107247001499370e-01 // 0xBFD4CD7D691CB913
	pp2  = -2.84817495755985104766e-02 // 0xBF9D2A51DBD7194F
	pp3  = -5.77027029648944159157e-03 // 0xBF77A291236668E4
	pp4  = -2.37630166566501626084e-05 // 0xBEF8EAD6120016AC
	qq1  = 3.97917223959155352819e-01 // 0x3FD97779CDDADC09
	qq2  = 6.50222499887672944485e-02 // 0x3FB0A54C5536CEBA
	qq3  = 5.08130628187576562776e-03 // 0x3F74D022C4D36B0F
	qq4  = 1.32494738004321644526e-04 // 0x3F215DC9221C1A10
	qq5  = -3.96022827877536812320e-06 // 0xBED09C4342A26120
	// Coefficients for approximation to  erf  in [0.84375, 1.25]
	pa0  = -2.36211856075265944077e-03 // 0xBF6359B8BEF77538
	pa1  = 4.14856118683748331666e-01 // 0x3FDA8D00AD92B34D
	pa2  = -3.72207876035701323847e-01 // 0xBFD7D240FBB8C3F1
	pa3  = 3.18346619901161753674e-01 // 0x3FD45FCA805120E4
	pa4  = -1.10894694282396677476e-01 // 0xBFBC63983D3E28EC
	pa5  = 3.54783043256182359371e-02 // 0x3FA22A36599795EB
	pa6  = -2.16637559486879084300e-03 // 0xBF61BF380A96073F
	qa1  = 1.06420880400844228286e-01 // 0x3FBB3E6618EEE323
	qa2  = 5.40397917702171048937e-01 // 0x3FE14AF092EB6F33
	qa3  = 7.18286544141962662868e-02 // 0x3FB2635CD99FE9A7
	qa4  = 1.26171219808761642112e-01 // 0x3FC02660E763351F
	qa5  = 1.36370839120290507362e-02 // 0x3F8BEDC26B51DD1C
	qa6  = 1.19844998467991074170e-02 // 0x3F888B545735151D
	// Coefficients for approximation to  erfc in [1.25, 1/0.35]
	ra0  = -9.86494403484714822705e-03 // 0xBF843412600D6435
	ra1  = -6.93858572707181764372e-01 // 0xBFE63416E4BA7360
	ra2  = -1.05586262253232909814e+01 // 0xC0251E0441B0E726
	ra3  = -6.23753324503260060396e+01 // 0xC04F300AE4CBA38D
	ra4  = -1.62396669462573470355e+02 // 0xC0644CB184282266
	ra5  = -1.84605092906711035994e+02 // 0xC067135CEBCCABB2
	ra6  = -8.12874355063065934246e+01 // 0xC054526557E4D2F2
	ra7  = -9.81432934416914548592e+00 // 0xC023A0EFC69AC25C
	sa1  = 1.96512716674392571292e+01 // 0x4033A6B9BD707687
	sa2  = 1.37657754143519042600e+02 // 0x4061350C526AE721
	sa3  = 4.34565877475229228821e+02 // 0x407B290DD58A1A71
	sa4  = 6.45387271733267880336e+02 // 0x40842B1921EC2868
	sa5  = 4.29008140027567833386e+02 // 0x407AD02157700314
	sa6  = 1.08635005541779435134e+02 // 0x405B28A3EE48AE2C
	sa7  = 6.57024977031928170135e+00 // 0x401A47EF8E484A93
	sa8  = -6.04244152148580987438e-02 // 0xBFAEEFF2EE749A62
	// Coefficients for approximation to  erfc in [1/.35, 28]
	rb0  = -9.86494292470009928597e-03 // 0xBF84341239E86F4A
	rb1  = -7.99283237680523006574e-01 // 0xBFE993BA70C285DE
	rb2  = -1.77579549177547519889e+01 // 0xC031C209555F995A
	rb3  = -1.60636384855821916062e+02 // 0xC064145D43C5ED98
	rb4  = -6.37566443368389627722e+02 // 0xC083EC881375F228
	rb5  = -1.02509513161107724954e+03 // 0xC09004616A2E5992
	rb6  = -4.83519191608651397019e+02 // 0xC07E384E9BDC383F
	sb1  = 3.03380607434824582924e+01 // 0x403E568B261D5190
	sb2  = 3.25792512996573918826e+02 // 0x40745CAE221B9F0A
	sb3  = 1.53672958608443695994e+03 // 0x409802EB189D5118
	sb4  = 3.19985821950859553908e+03 // 0x40A8FFB7688C246A
	sb5  = 2.55305040643316442583e+03 // 0x40A3F219CEDF3BE6
	sb6  = 4.74528541206955367215e+02 // 0x407DA874E79FE763
	sb7  = -2.24409524465858183362e+01 // 0xC03670E242712D62
)

// erf returns the error function of x.
//
// special cases are:
// erf(+inf) = 1
// erf(-inf) = -1
// erf(nan) = nan
pub fn erf(a f64) f64 {
	mut x := a
	very_tiny := 2.848094538889218e-306 // 0x0080000000000000
	small_ := 1.0 / f64(u64(1) << 28) // 2**-28
	if is_nan(x) {
		return nan()
	}
	if is_inf(x, 1) {
		return 1.0
	}
	if is_inf(x, -1) {
		return f64(-1)
	}
	mut sign := false
	if x < 0 {
		x = -x
		sign = true
	}
	if x < 0.84375 { // |x| < 0.84375
		mut temp := 0.0
		if x < small_ { // |x| < 2**-28
			if x < very_tiny {
				temp = 0.125 * (8.0 * x + math.efx8 * x) // avoid underflow
			} else {
				temp = x + math.efx * x
			}
		} else {
			z := x * x
			r := math.pp0 + z * (math.pp1 + z * (math.pp2 + z * (math.pp3 + z * math.pp4)))
			s_ := 1.0 + z * (math.qq1 + z * (math.qq2 + z * (math.qq3 + z * (math.qq4 +
				z * math.qq5))))
			y := r / s_
			temp = x + x * y
		}
		if sign {
			return -temp
		}
		return temp
	}
	if x < 1.25 { // 0.84375 <= |x| < 1.25
		s_ := x - 1
		p := math.pa0 + s_ * (math.pa1 + s_ * (math.pa2 + s_ * (math.pa3 + s_ * (math.pa4 +
			s_ * (math.pa5 + s_ * math.pa6)))))
		q := 1.0 + s_ * (math.qa1 + s_ * (math.qa2 + s_ * (math.qa3 + s_ * (math.qa4 +
			s_ * (math.qa5 + s_ * math.qa6)))))
		if sign {
			return -math.erx - p / q
		}
		return math.erx + p / q
	}
	if x >= 6 { // inf > |x| >= 6
		if sign {
			return -1
		}
		return 1.0
	}
	s_ := 1.0 / (x * x)
	mut r := 0.0
	mut s := 0.0
	if x < 1.0 / 0.35 { // |x| < 1 / 0.35  ~ 2.857143
		r = math.ra0 + s_ * (math.ra1 + s_ * (math.ra2 + s_ * (math.ra3 + s_ * (math.ra4 +
			s_ * (math.ra5 + s_ * (math.ra6 + s_ * math.ra7))))))
		s = 1.0 + s_ * (math.sa1 + s_ * (math.sa2 + s_ * (math.sa3 + s_ * (math.sa4 +
			s_ * (math.sa5 + s_ * (math.sa6 + s_ * (math.sa7 + s_ * math.sa8)))))))
	} else { // |x| >= 1 / 0.35  ~ 2.857143
		r = math.rb0 + s_ * (math.rb1 + s_ * (math.rb2 + s_ * (math.rb3 + s_ * (math.rb4 +
			s_ * (math.rb5 + s_ * math.rb6)))))
		s = 1.0 + s_ * (math.sb1 + s_ * (math.sb2 + s_ * (math.sb3 + s_ * (math.sb4 +
			s_ * (math.sb5 + s_ * (math.sb6 + s_ * math.sb7))))))
	}
	z := f64_from_bits(f64_bits(x) & 0xffffffff00000000) // pseudo-single (20-bit) precision x
	r_ := exp(-z * z - 0.5625) * exp((z - x) * (z + x) + r / s)
	if sign {
		return r_ / x - 1.0
	}
	return 1.0 - r_ / x
}

// erfc returns the complementary error function of x.
//
// special cases are:
// erfc(+inf) = 0
// erfc(-inf) = 2
// erfc(nan) = nan
pub fn erfc(a f64) f64 {
	mut x := a
	tiny := 1.0 / f64(u64(1) << 56) // 2**-56
	// special cases
	if is_nan(x) {
		return nan()
	}
	if is_inf(x, 1) {
		return 0.0
	}
	if is_inf(x, -1) {
		return 2.0
	}
	mut sign := false
	if x < 0 {
		x = -x
		sign = true
	}
	if x < 0.84375 { // |x| < 0.84375
		mut temp := 0.0
		if x < tiny { // |x| < 2**-56
			temp = x
		} else {
			z := x * x
			r := math.pp0 + z * (math.pp1 + z * (math.pp2 + z * (math.pp3 + z * math.pp4)))
			s_ := 1.0 + z * (math.qq1 + z * (math.qq2 + z * (math.qq3 + z * (math.qq4 +
				z * math.qq5))))
			y := r / s_
			if x < 0.25 { // |x| < 1.0/4
				temp = x + x * y
			} else {
				temp = 0.5 + (x * y + (x - 0.5))
			}
		}
		if sign {
			return 1.0 + temp
		}
		return 1.0 - temp
	}
	if x < 1.25 { // 0.84375 <= |x| < 1.25
		s_ := x - 1
		p := math.pa0 + s_ * (math.pa1 + s_ * (math.pa2 + s_ * (math.pa3 + s_ * (math.pa4 +
			s_ * (math.pa5 + s_ * math.pa6)))))
		q := 1.0 + s_ * (math.qa1 + s_ * (math.qa2 + s_ * (math.qa3 + s_ * (math.qa4 +
			s_ * (math.qa5 + s_ * math.qa6)))))
		if sign {
			return 1.0 + math.erx + p / q
		}
		return 1.0 - math.erx - p / q
	}
	if x < 28 { // |x| < 28
		s_ := 1.0 / (x * x)
		mut r := 0.0
		mut s := 0.0
		if x < 1.0 / 0.35 { // |x| < 1 / 0.35 ~ 2.857143
			r = math.ra0 + s_ * (math.ra1 + s_ * (math.ra2 + s_ * (math.ra3 + s_ * (math.ra4 +
				s_ * (math.ra5 + s_ * (math.ra6 + s_ * math.ra7))))))
			s = 1.0 + s_ * (math.sa1 + s_ * (math.sa2 + s_ * (math.sa3 + s_ * (math.sa4 +
				s_ * (math.sa5 + s_ * (math.sa6 + s_ * (math.sa7 + s_ * math.sa8)))))))
		} else { // |x| >= 1 / 0.35 ~ 2.857143
			if sign && x > 6 {
				return 2.0 // x < -6
			}
			r = math.rb0 + s_ * (math.rb1 + s_ * (math.rb2 + s_ * (math.rb3 + s_ * (math.rb4 +
				s_ * (math.rb5 + s_ * math.rb6)))))
			s = 1.0 + s_ * (math.sb1 + s_ * (math.sb2 + s_ * (math.sb3 + s_ * (math.sb4 +
				s_ * (math.sb5 + s_ * (math.sb6 + s_ * math.sb7))))))
		}
		z := f64_from_bits(f64_bits(x) & 0xffffffff00000000) // pseudo-single (20-bit) precision x
		r_ := exp(-z * z - 0.5625) * exp((z - x) * (z + x) + r / s)
		if sign {
			return 2.0 - r_ / x
		}
		return r_ / x
	}
	if sign {
		return 2.0
	}
	return 0.0
}
