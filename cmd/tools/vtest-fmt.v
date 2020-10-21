module main

import os
import testing

// os.v - // embeded comments, mib := [1/* CTL_KERN */, 14/* KERN_PROC */, 12/* KERN_PROC_PATHNAME */, -1] => comment the rest of the line
const (
	known_failing_exceptions = [
		'vlib/v/tests/generics_test.v', // struct Repo<T, U> { => struct Repo {
		'vlib/crypto/aes/aes.v', // pub fn (c &AesCipher) encrypt(mut dst, mut src []byte) {
		'vlib/crypto/aes/block_generic.v', // fn expand_key_generic(key []byte, mut enc, mut dec []u32) {
		'vlib/crypto/aes/const.v', // multiple narrow columns of []string turned to 1 long single column, otherwise works
		'vlib/crypto/rc4/rc4.v', // pub fn (mut c Cipher) xor_key_stream(mut dst, mut src []byte) {
		'vlib/vweb/vweb.v', // $for method in T.methods { => $for method in T(methods) { , `return // xx` => parse expr error
		'vlib/v/gen/js/tests/life.v', // error: unexpected `,`, expecting ), on JS.setInterval(fn () { show(game) game = step(game) }, 500)
		'vlib/builtin/js/builtin.v', // JS.console.error(s) => JS.error(s), JS.process.exit(c) => JS.exit(c)
		'vlib/builtin/js/jsfns_node.js.v',
		'vlib/builtin/js/jsfns.js.v',
		'vlib/builtin/js/jsfns_browser.js.v',
		'vlib/builtin/bare/linuxsys_bare.v', // error: expr(): bad token `asm`, on `asm {}`
		'vlib/os/os.v',
	]
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	v_test_formatting(args_string.all_before('test-fmt'))
}

fn v_test_formatting(vargs string) {
	all_v_files := v_files()
    prepare_vfmt_when_needed()
	testing.eheader('Run "v fmt" over all .v files')
	mut vfmt_test_session := testing.new_test_session('$vargs fmt -worker')
	vfmt_test_session.files << all_v_files
	vfmt_test_session.skip_files << known_failing_exceptions
	vfmt_test_session.test()
	eprintln(vfmt_test_session.benchmark.total_message('running vfmt over V files'))
	if vfmt_test_session.benchmark.nfail > 0 {
		eprintln('\nWARNING: v fmt failed $vfmt_test_session.benchmark.nfail times.\n')
		exit(1)
	}
}

fn v_files() []string {
	mut files_that_can_be_formatted := []string{}
	all_test_files := os.walk_ext('.', '.v')
	for tfile in all_test_files {
		if tfile.starts_with('./vlib/v/cgen/tests') {
			continue
		}
		files_that_can_be_formatted << tfile
	}
	return files_that_can_be_formatted
}

fn prepare_vfmt_when_needed() {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	vfmtv := os.join_path(vroot, 'cmd', 'tools', 'vfmt.v')
	if os.file_last_mod_unix(vexe) <= os.file_last_mod_unix(vfmtv) {
		recompile_result := os.system('$vexe ' + os.join_path(vroot, 'cmd', 'tools', 'vfmt.v'))
		if recompile_result != 0 {
			eprintln('could not recompile cmd/tools/vfmt.v')
			exit(2)
		}
	}
}
