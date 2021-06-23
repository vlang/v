module main

// This program verifies that `v test` propagates errors
// and that it exits with code 1, when at least 1 FAIL happen.
import os
import rand

const (
	vexe  = get_vexe_path()
	vroot = os.dir(vexe)
	tdir  = new_tdir()
)

fn get_vexe_path() string {
	env_vexe := os.getenv('VEXE')
	if env_vexe != '' {
		return env_vexe
	}
	me := os.executable()
	eprintln('me: $me')
	mut vexe_ := os.join_path(os.dir(os.dir(os.dir(me))), 'v')
	if os.user_os() == 'windows' {
		vexe_ += '.exe'
	}
	return vexe_
}

fn new_tdir() string {
	tdir_ := os.join_path(os.temp_dir(), rand.ulid())
	if os.exists(tdir_) {
		os.rmdir(tdir_) or { panic(err) }
	}
	os.mkdir(tdir_) or { panic(err) }
	C.atexit(cleanup_tdir)
	return tdir_
}

fn cleanup_tdir() {
	println('... removing tdir: $tdir')
	os.rmdir_all(tdir) or { panic(err) }
}

fn main() {
	println('> vroot: $vroot | vexe: $vexe | tdir: $tdir')
	ok_fpath := os.join_path(tdir, 'single_test.v')
	os.write_file(ok_fpath, 'fn test_ok(){ assert true }') ?
	check_ok('"$vexe" $ok_fpath')
	check_ok('"$vexe" test $ok_fpath')
	fail_fpath := os.join_path(tdir, 'failing_test.v')
	os.write_file(fail_fpath, 'fn test_fail(){ assert 1 == 2 }') ?
	check_fail('"$vexe" $fail_fpath')
	check_fail('"$vexe" test $fail_fpath')
	check_fail('"$vexe" test $tdir')
}

fn check_ok(cmd string) string {
	println('>   check_ok cmd: $cmd')
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('>   check_ok failed.\n$res.output')
		exit(1)
	}
	return res.output
}

fn check_fail(cmd string) string {
	println('> check_fail cmd: $cmd')
	res := os.execute(cmd)
	if res.exit_code == 0 {
		eprintln('> check_fail succeeded, but it should have failed.\n$res.output')
		exit(1)
	}
	return res.output
}
