import os
import arrays.diff
import flag

fn main() {
	mut fp := flag.new_flag_parser(os.args[1..])
	fp.application('v diff')
	fp.version('0.0.1')
	fp.description('Compare files line by line. Example: `v diff examples/hello_world.v examples/log.v`')
	fp.arguments_description('file1 file2')
	fp.skip_executable()
	fp.limit_free_args_to_at_least(2)!

	if fp.bool('help', `h`, false, 'Show this help screen.') {
		println(fp.usage())
		exit(0)
	}

	args := fp.finalize() or {
		eprintln('Argument error: ${err}')
		exit(1)
	}

	src := os.read_lines(args[0])!
	dst := os.read_lines(args[1])!
	mut ctx := diff.diff(src, dst)
	println(ctx.generate_patch(colorful: false, block_header: true, unified: 3))
	exit(0)
}
