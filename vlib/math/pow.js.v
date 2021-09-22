module math

fn JS.Math.pow(x f64, y f64) f64

pub fn pow(x f64, y f64) f64 {
	return JS.Math.pow(x, y)
}
