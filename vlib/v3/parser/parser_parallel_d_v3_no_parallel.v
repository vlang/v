module parser

// parse_files_dispatch falls back to the serial parse when v3 is built with
// the internal `v3_no_parallel` define. Returns each file's first node id in
// p.a and whether worker threads were used.
pub fn (mut p Parser) parse_files_dispatch(paths []string, _ bool) ([]int, bool) {
	return p.parse_files_with_starts(paths), false
}
