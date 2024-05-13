module main

import os

fn main() {
	args := os.args[2..]
	if args.len == 0 || args == ['help'] || '-help' in args || '--help' in args {
		os.system('${os.quoted_path(vexe)} help where')
		exit(0)
	}
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	fdr.show_results()
}
