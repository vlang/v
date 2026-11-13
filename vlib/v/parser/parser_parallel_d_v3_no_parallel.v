module parser

// parse_files_dispatch falls back to the serial parse when v3 is built with
// the internal `v3_no_parallel` define. Returns each file's first node id in
// p.a and whether worker threads were used.
pub fn (mut p Parser) parse_files_dispatch(paths []string, _ bool) ([]int, bool) {
	p.drop_batch_preloads()
	return p.parse_files_with_starts(paths), false
}

// speculate_next_batch does nothing in a build without the parallel parse.
pub fn (mut p Parser) speculate_next_batch(_ []string) {}

// discard_speculated_batch does nothing in a build without the parallel parse.
pub fn (mut p Parser) discard_speculated_batch() {}
