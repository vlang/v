// vtest build: !sanitize-memory-gcc && !sanitize-address-gcc && !sanitize-address-clang
// vtest vflags: -autofree
import flag
import os

fn test_main() {
	mut fp := flag.new_flag_parser(os.args)
	test := fp.string('test', 0, '', 'this is a test')
	fp.finalize() or { return }
	println(test)
}
