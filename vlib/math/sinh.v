module math

// sinh calculates hyperbolic sine.
pub fn sinh(x_ f64) f64 {
	mut x := x_
	// The coefficients are #2029 from Hart & Cheney. (20.36D)
	p0 := -0.6307673640497716991184787251e+6
	p1 := -0.8991272022039509355398013511e+5
	p2 := -0.2894211355989563807284660366e+4
	p3 := -0.2630563213397497062819489e+2
	q0 := -0.6307673640497716991212077277e+6
	q1 := 0.1521517378790019070696485176e+5
	q2 := -0.173678953558233699533450911e+3
	mut sign := false
	if x < 0 {
		x = -x
		sign = true
	}
	mut temp := 0.0
	if x > 21 {
		temp = exp(x) * 0.5
	} else if x > 0.5 {
		ex := exp(x)
		temp = (ex - 1.0 / ex) * 0.5
	} else {
		sq := x * x
		temp = (((p3 * sq + p2) * sq + p1) * sq + p0) * x
		temp = temp / (((sq + q2) * sq + q1) * sq + q0)
	}
	if sign {
		temp = -temp
	}
	return temp
}

// cosh returns the hyperbolic cosine of x in radians
//
// special cases are:
// cosh(±0) = 1
// cosh(±inf) = +inf
// cosh(nan) = nan
pub fn cosh(x f64) f64 {
	abs_x := abs(x)
	if abs_x > 21 {
		return exp(abs_x) * 0.5
	}
	ex := exp(abs_x)
	return (ex + 1.0 / ex) * 0.5
}
