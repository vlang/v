module main

// This program verifies that `v test` propagates errors
// and that it exits with code 1, when at least 1 FAIL happen.
import os
import rand

const vexe = os.quoted_path(get_vexe_path())
const vroot = os.dir(vexe)
const tdir = new_tdir()

fn get_vexe_path() string {
	env_vexe := os.getenv('VEXE')
	if env_vexe != '' {
		return env_vexe
	}
	me := os.executable()
	eprintln('me: ${me}')
	mut vexe_ := os.join_path(os.dir(os.dir(os.dir(me))), 'v')
	if os.user_os() == 'windows' {
		vexe_ += '.exe'
	}
	return vexe_
}

fn new_tdir() string {
	dir := os.join_path(os.vtmp_dir(), rand.ulid())
	os.rmdir_all(dir) or {}
	eprintln('... creating tdir: ${tdir}')
	os.mkdir_all(dir) or { panic(err) }
	C.atexit(cleanup_tdir)
	return dir
}

fn cleanup_tdir() {
	eprintln('... removing tdir: ${tdir}')
	os.rmdir_all(tdir) or { eprintln(err) }
}

type MyResult = string

@[noreturn]
fn (result MyResult) fail(reason string) {
	eprintln('> ${reason}, but it does not. Result:\n${result}')
	exit(1)
}

fn (result MyResult) has(sub string) MyResult {
	if !result.contains(sub) {
		result.fail(' result should have the substring `${sub}`')
	}
	return result
}

fn (result MyResult) matches(gpattern string) MyResult {
	if !result.match_glob(gpattern) {
		result.fail('result should match the glob pattern `${gpattern}`')
	}
	return result
}

fn create_test(tname string, tcontent string) !string {
	tpath := os.join_path(tdir, tname)
	os.write_file(tpath, tcontent)!
	$if trace_create_test ? {
		eprintln('>>>>>>>> tpath: ${tpath} | tcontent:')
		eprintln(tcontent)
		eprintln('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>')
	}
	return os.quoted_path(tpath)
}

fn check_assert_continues_works() ! {
	os.chdir(tdir)!
	create_test('assert_continues_option_works_test.v', 'fn test_fail1() {\nassert 2==4\nassert 2==1\nassert 2==0 }\nfn test_ok(){ assert true }\nfn test_fail2() {\n assert false\n}\n')!
	result := check_fail('${vexe} -g -assert continues assert_continues_option_works_test.v')
	result.has('assert_continues_option_works_test.v:2: fn test_fail1')
	result.has('assert_continues_option_works_test.v:3: fn test_fail1')
	result.has('assert_continues_option_works_test.v:4: fn test_fail1')
	result.has('assert_continues_option_works_test.v:7: fn test_fail2')
	result.has('> assert 2 == 4').has('> assert 2 == 1').has('> assert 2 == 0')
	// Check if a test function, tagged with @[assert_continues], has the same behaviour, without needing additional options
	create_test('assert_continues_tag_works_test.v', '@[assert_continues]fn test_fail1() {\nassert 2==4\nassert 2==1\nassert 2==0 }\nfn test_ok(){ assert true }\nfn test_fail2() {\n assert false\n assert false\n}\n')!
	tag_res := check_fail('${vexe} -g assert_continues_tag_works_test.v')
	tag_res.has('assert_continues_tag_works_test.v:2: fn test_fail1')
	tag_res.has('assert_continues_tag_works_test.v:3: fn test_fail1')
	tag_res.has('assert_continues_tag_works_test.v:4: fn test_fail1')
	tag_res.has('assert_continues_tag_works_test.v:7: fn test_fail2')
	if tag_res.contains('assert_continues_tag_works_test.v:8: fn test_fail2') {
		exit(1)
	}
}

fn check_ok(cmd string) MyResult {
	println('>   check_ok cmd: ${cmd}')
	res := os.execute(cmd)
	if res.exit_code != 0 {
		eprintln('>   check_ok failed.\n${res.output}')
		exit(1)
	}
	return res.output
}

fn check_fail(cmd string) MyResult {
	println('> check_fail cmd: ${cmd}')
	res := os.execute(cmd)
	if res.exit_code == 0 {
		eprintln('> check_fail succeeded, but it should have failed.\n${res.output}')
		exit(1)
	}
	return res.output
}

fn main() {
	defer {
		os.chdir(os.wd_at_startup) or {}
	}
	println('> vroot: ${vroot} | vexe: ${vexe} | tdir: ${tdir}')
	ok_fpath := create_test('a_single_ok_test.v', 'fn test_ok(){\n assert true \n}\n')!
	if check_ok('${vexe} -g ${ok_fpath}') != '' {
		exit(1)
	}
	check_ok('${vexe} -g test ${ok_fpath}').matches('*OK*a_single_ok_test.v*')
	check_ok('${vexe} -g test "${tdir}"').matches('*OK*a_single_ok_test.v*')
	//
	fail_fpath := create_test('a_single_failing_test.v', 'fn test_fail(){\n assert 1 == 2 \n}\n')!
	check_fail('${vexe} -g ${fail_fpath}').has('> assert 1 == 2').has('a_single_failing_test.v:2: fn test_fail')
	check_fail('${vexe} -g test ${fail_fpath}').has('> assert 1 == 2').has('a_single_failing_test.v:2: fn test_fail')
	check_fail('${vexe} -g test "${tdir}"').has('> assert 1 == 2')
	rel_dir := os.join_path(tdir, rand.ulid())
	os.mkdir(rel_dir)!
	os.chdir(rel_dir)!
	relative_path := '..' + os.path_separator + 'a_single_ok_test.v'
	check_ok('${vexe} -g test ${os.quoted_path(relative_path)}').has('OK').has('a_single_ok_test.v')
	//
	check_assert_continues_works()!
	println('> all done')
}
