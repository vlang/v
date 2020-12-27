module forkedtest

pub fn run (op fn(), label string, code Wi_si_code, status int) int {
	child := sys_fork()
	if child == 0 {
		op()
		sys_exit(0)
	}

	siginfo := []int{len:int(Sig_index.si_size)}

	e := sys_waitid(.p_pid, child, intptr(&siginfo[0]), .wexited, 0)

	assert e == .enoerror
	assert siginfo[int(Sig_index.si_pid)] == child
	assert siginfo[int(Sig_index.si_signo)] == int(Signo.sigchld)
	assert siginfo[int(Sig_index.si_uid)] == sys_getuid()

	r_code := siginfo[Sig_index.si_code]
	r_status := siginfo[Sig_index.si_status]

	print("+++ ")
	print(label)
	if (int(code) == r_code) && (status == r_status) {
		println(" PASSED")
		return 0
	}
	println(" FAILED")

	if int(code) != r_code {
		print(">> Expecting si_code 0x")
		println(i64_str(int(code),16))
		print(">> Got 0x")
		println(i64_str(r_code,16))
	}

	if status != r_status {
		print(">> Expecting status 0x")
		println(i64_str(status,16))
		print(">> Got 0x")
		println(i64_str(r_status,16))
	}

	return 1
}

pub fn normal_run (op fn(), label string) int {
	return run (op, label, .cld_exited, 0)
}
