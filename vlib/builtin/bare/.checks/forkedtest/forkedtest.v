module forkedtest

__global buffer [128]byte

pub fn run (op fn(), label string, code wi_si_code, status int) int {
	child := sys_fork()
	if child == 0 {
		op()
		sys_exit(0)
	}
	siginfo := [
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0]

	e := sys_waitid(.p_pid, child, intptr(siginfo.data) , .wexited, 0)

	assert e == .enoerror
	assert siginfo[sig_index.si_pid] == child
	assert siginfo[sig_index.si_signo] == int(signo.sigchld)
	assert siginfo[sig_index.si_uid] == sys_getuid()

	r_code := siginfo[sig_index.si_code]
	r_status := siginfo[sig_index.si_status]

	print(label)
	if (int(code) == r_code) && (status == r_status) {
		println(" PASSED")
		return 0
	}
	println(" FAILED")

	if int(code) != r_code {
		print(">> Expecting si_code 0x")
		println(i64_tos(buffer,80,int(code),16))
		print(">> Got 0x")
		println(i64_tos(buffer,80,r_code,16))
	}

	if status != r_status {
		print(">> Expecting status 0x")
		println(i64_tos(buffer,80,status,16))
		print(">> Got 0x")
		println(i64_tos(buffer,80,r_status,16))
	}

	return 1
}

pub fn normal_run (op fn(), label string) int {
	return run (op, label, .cld_exited, 0)
}

