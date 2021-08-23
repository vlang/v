module math

fn C.log(x f64) f64

fn C.log2(x f64) f64

fn C.log10(x f64) f64

// log calculates natural (base-e) logarithm of the provided value.
[inline]
pub fn log(x f64) f64 {
	return C.log(x)
}

// log2 calculates base-2 logarithm of the provided value.
[inline]
pub fn log2(x f64) f64 {
	return C.log2(x)
}

// log10 calculates the common (base-10) logarithm of the provided value.
[inline]
pub fn log10(x f64) f64 {
	return C.log10(x)
}
