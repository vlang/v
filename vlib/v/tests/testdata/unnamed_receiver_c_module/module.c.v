module unnamed_receiver_c_module

// Keep a native link dependency on every host, without requiring Unix libm on Windows.
$if windows {
	#flag -lkernel32
} $else {
	#flag -lm
}

pub fn identity(value int) int {
	return value
}
