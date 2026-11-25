module unnamed_receiver_c_module

#flag -DUNNAMED_RECEIVER_C_MODULE

// Keep a native link dependency on every host, without requiring Unix libm on Windows.
$if windows {
	#flag -lkernel32
} $else {
	#flag -lm
}

pub fn identity(value int) int {
	return value
}
