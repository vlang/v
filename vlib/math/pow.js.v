module math

fn JS.Math.pow(x f64, y f64) f64

// pow returns base raised to the provided power.
pub fn pow(x f64, y f64) f64 {
	return f64(JS.Math.pow(x, y))
}
