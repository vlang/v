module sync

@[noreturn]
fn cpanic(res int) {
	panic(unsafe { tos_clone(&u8(C.strerror(res))) })
}

@[noreturn]
fn cpanic_errno() {
	cpanic(C.errno)
}

fn should_be_zero(res int) {
	if res != 0 {
		cpanic(res)
	}
}
