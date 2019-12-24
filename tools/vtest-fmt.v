module main

import (
	os
	testing
	benchmark
)

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	v_test_formatting(args_string.all_before('test-fmt'))
}

fn v_test_formatting(vargs string) {
	// NB: vfmt have to be build with '-d vfmt' . V itself knows about this,
	// and v will rebuild tools/vfmt, if it is missing.
	// Removing the binaries below is needed, since the building tools step
	// rebuilds all the tools without the special option needed by vfmt
	// by simply compiling each of them with `v tools/{toolname}.v`
	//	os.rm('tools/vfmt')
	//	os.rm('tools/vfmt.exe')	

	mut files_able_to_be_formatted := []string
	all_test_files := os.walk_ext('.', '.v')
	known_failing_exceptions := [
    './examples/vweb/vweb_example.v',
    './tools/gen_vc.v',
    './tutorials/code/blog/article.v',
    './tutorials/code/blog/blog.v',
    './vlib/arrays/arrays.v',
		'./vlib/arrays/arrays_test.v',
    './vlib/builtin/js/hashmap.v',
		'./vlib/compiler/tests/fn_variadic_test.v',
		'./vlib/compiler/tests/generic_test.v',
    './vlib/compiler/preludes/live_main.v',
    './vlib/compiler/preludes/live_shared.v',
    './vlib/compiler/preludes/tests_assertions.v',
    './vlib/compiler/preludes/tests_with_stats.v',
    './vlib/crypto/aes/aes.v',
    './vlib/crypto/aes/aes_cbc.v',
    './vlib/crypto/aes/block_generic.v',
    './vlib/crypto/aes/const.v',
    './vlib/crypto/aes/cypher_generic.v',
    './vlib/crypto/rc4/rc4.v',
		'./vlib/eventbus/eventbus_test.v',
    './vlib/flag/flag.v',
    './vlib/os/bare/bare_example_linux.v',
    './vlib/sdl/examples/tvintris/tvintris.v',
    './vlib/sdl/ttf/ttf.v',
    './vlib/szip/szip.v',
    './vlib/ui/examples/users_gui/users.v',
    './vlib/vweb/assets/assets.v',
    './vlib/vweb/vweb.v',
	]
	for tfile in all_test_files {
		if tfile in known_failing_exceptions { continue }
		files_able_to_be_formatted << tfile
	}
	
	eprintln('Run "v fmt" over all .v files')
	mut vfmt_test_session := testing.new_test_session('$vargs fmt')
	vfmt_test_session.files << files_able_to_be_formatted
	vfmt_test_session.test()
	eprintln(vfmt_test_session.benchmark.total_message('running vfmt over V test files'))
	if vfmt_test_session.benchmark.nfail > 0 {
		panic('\nWARNING: v fmt failed ${vfmt_test_session.benchmark.nfail} times.\n')
	}
}
