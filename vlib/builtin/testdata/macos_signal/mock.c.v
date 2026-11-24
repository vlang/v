module main

#flag @DIR/policy.o

fn C.v_macos_signal_test_call(u32, u64, u64, u64, u64, u64, u64, &u64) i32
fn C.v_macos_signal_policy_probe() i32

@[no_profile; no_trace]
fn mac_kernel_bsd_call6(number u32, a1 u64, a2 u64, a3 u64, a4 u64, a5 u64, a6 u64, result &u64) i32 {
	return C.v_macos_signal_test_call(number, a1, a2, a3, a4, a5, a6, result)
}

fn main() {
	code := C.v_macos_signal_policy_probe()
	if code != 0 {
		eprintln('signal policy: ${code}')
		exit(93)
	}
	println('V Darwin signal policy and C ABI ok')
}
