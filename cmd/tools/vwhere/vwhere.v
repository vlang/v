module main

import os

fn main() {
	args := os.args[2..]

	valid_args_quantity_or_show_help(args)

	mut fdr := Finder{}
	fdr.configure_from_arguments(args)
	fdr.search_for_matches()
	fdr.show_results()
}
