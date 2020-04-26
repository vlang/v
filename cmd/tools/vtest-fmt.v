module main

import os
import testing

const (
	known_failing_exceptions = ['./examples/vweb/vweb_example.v',
	'./cmd/tools/gen_vc.v',
	'./cmd/tools/modules/vgit/vgit.v', // generics
	'./cmd/tools/preludes/live_main.v',
	'./cmd/tools/preludes/live_shared.v',
	'./cmd/tools/preludes/tests_assertions.v',
	'./cmd/tools/preludes/tests_with_stats.v',
	'./cmd/tools/performance_compare.v', // generics
	'./cmd/tools/oldv.v', // generics
	'./tutorials/code/blog/article.v',
	'./tutorials/code/blog/blog.v',
	'./vlib/arrays/arrays.v',
	'./vlib/arrays/arrays_test.v',
	'./vlib/builtin/js/hashmap.v',
	'./vlib/v/tests/fn_variadic_test.v',
	'./vlib/v/tests/generic_test.v',
	'./vlib/crypto/aes/aes.v',
	'./vlib/crypto/aes/aes_cbc.v',
	'./vlib/crypto/aes/block_generic.v',
	'./vlib/crypto/aes/const.v',
	'./vlib/crypto/aes/cypher_generic.v',
	'./vlib/crypto/rc4/rc4.v',
	'./vlib/eventbus/eventbus_test.v',
	'./vlib/os/bare/bare_example_linux.v',
	'./vlib/szip/szip.v',
	'./vlib/uiold/examples/users_gui/users.v',
	'./vlib/vweb/assets/assets.v',
	'./vlib/vweb/vweb.v',
	]
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	v_test_formatting(args_string.all_before('test-fmt'))
}

fn v_test_formatting(vargs string) {
	all_v_files := v_files()
	testing.eheader('Run "v fmt" over all .v files')
	mut vfmt_test_session := testing.new_test_session('$vargs fmt -worker')
	vfmt_test_session.files << all_v_files
	vfmt_test_session.test()
	eprintln(vfmt_test_session.benchmark.total_message('running vfmt over V files'))
	if vfmt_test_session.benchmark.nfail > 0 {
		panic('\nWARNING: v fmt failed ${vfmt_test_session.benchmark.nfail} times.\n')
	}
}

fn v_files() []string {
	mut files_that_can_be_formatted := []string{}
	all_test_files := os.walk_ext('.', '.v')
	for tfile in all_test_files {
		if tfile in known_failing_exceptions {
			continue
		}
		if tfile.starts_with('./vlib/v/cgen/tests') {
			continue
		}
		files_that_can_be_formatted << tfile
	}
	return files_that_can_be_formatted
}
