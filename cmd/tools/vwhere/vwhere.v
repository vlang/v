module main

import os
import os.cmdline

fn main() {
	args := os.args[2..]
	verbose := '-v' in cmdline.only_options(args)
	header := '-h' in cmdline.only_options(args)
	format := '-f' in cmdline.only_options(args)

	valid_args_quantity_or_show_help(args)
	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	fdr.show_results(verbose, header, format)
}
