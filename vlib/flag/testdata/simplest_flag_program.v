import os
import flag

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.application('abc')
	fp.version('0.0.1')
	fp.skip_executable()
	rest_of_args := fp.finalize() or {
		eprintln(err)
		exit(1)
	}
	dump(rest_of_args)
}
