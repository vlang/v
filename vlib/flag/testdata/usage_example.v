import os
import flag

fn main() {
	mut fp := flag.new_flag_parser(os.args)
	fp.skip_executable()
	fp.application('xyz')
	fp.version('0.0.2')
	fp.usage_example('[NUMBER]...')
	fp.usage_example('OPTION')
	fp.description('description line 1')
	fp.description('description line 2')
	fp.footer('footer 1')
	fp.footer('footer 2')
	rest_of_args := fp.remaining_parameters()
	dump(rest_of_args)
}
