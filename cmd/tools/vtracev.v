module main

import os
import v.pref

fn main() {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	os.chdir(vroot)!
	os.setenv('VCOLORS', 'always', true)
	self_idx := os.args.index('tracev')
	args := os.args[1..self_idx]
	args_str := args.join(' ')
	options := if args.len > 0 { '($args_str)' } else { '' }
	println('Compiling a `tracev` executable ${options}...')
	os.system('${os.quoted_path(vexe)} -cg -d trace_parser -d trace_checker -d trace_gen -o tracev $args_str cmd/v')
}
