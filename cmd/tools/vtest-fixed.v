module main

import os
import testing
import v.pref

const (
	skip_test_files = [
		'vlib/arrays/arrays_test.v',
		'vlib/json/json_test.v',
		'vlib/v/tests/enum_bitfield_test.v',
		'vlib/v/tests/num_lit_call_method_test.v',
		'vlib/v/tests/pointers_test.v',
		'vlib/v/tests/type_test.v',
		'vlib/v/tests/valgrind/valgrind_test.v', // ubuntu-musl only
		'vlib/v/tests/pointers_str_test.v',
		'vlib/net/http/http_httpbin_test.v', // fails on ubuntu-musl, because of missing openssl
		'vlib/net/http/http_test.v', // fails on ubuntu-musl, because of missing openssl
		'vlib/net/http/cookie_test.v', // ok, but should be skipped on ubuntu-musl, since there is no openssl there
		'vlib/clipboard/clipboard_test.v',
		'vlib/sqlite/sqlite_test.v',

		'vlib/v/tests/asm_test.v', // skip everywhere for now, works on linux with cc != tcc
	]
	skip_on_linux = []string{}
	skip_on_non_linux = []string{}
)

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)
	args := os.args
	args_string := args[1..].join(' ')
	cmd_prefix := args_string.all_before('test-fixed')
	title := 'testing all fixed tests'
	all_test_files := os.walk_ext( os.join_path(vroot,'vlib'), '_test.v')
	testing.eheader(title)
	mut tsession := testing.new_test_session(cmd_prefix)
	tsession.files << all_test_files
	tsession.skip_files << skip_test_files
	$if !linux {
	   tsession.skip_files << skip_on_non_linux
	}
	$if linux {
	   tsession.skip_files << skip_on_linux
	}
	tsession.test()
	eprintln(tsession.benchmark.total_message(title))
	if tsession.benchmark.nfail > 0 {
		panic('\nWARNING: failed ${tsession.benchmark.nfail} times.\n')
	}
}
