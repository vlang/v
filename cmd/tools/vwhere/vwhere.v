module main

fn main() {
	valid_args_quantity_or_show_help()
	mut fdr := Finder{}
	fdr.configure_from_arguments()
	fdr.search_for_matches()
	fdr.show_results()
}
