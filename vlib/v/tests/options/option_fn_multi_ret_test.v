module main

type Cmd = fn () string

fn cplx() (bool, ?Cmd) {
	return true, stringer
}

fn stringer() string {
	return 'a string'
}

fn test_main() {
	mut cmd := ?Cmd(none)
	mut truth := false
	truth, cmd = cplx()
	if cmd != none {
		println(cmd())
		assert true
	} else {
		assert false
	}
}
