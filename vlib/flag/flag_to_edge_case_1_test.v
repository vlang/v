import flag

const edge_case = ['appimage', '-v', '3', '-o', '/tmp/lol.appimage', '/home/user/Projects/game/']

pub struct AppImageOptions {
pub:
	verbosity    int  @[short: v; xdoc: 'Verbosity level 1-3']
	dump_usage   bool @[long: help; short: h; xdoc: 'Show this help message and exit']
	show_version bool @[long: version; xdoc: 'Output version information and exit']
pub mut:
	input  string   @[tail]
	output string   @[short: o; xdoc: 'Path to output (dir/file)']
	assets []string @[short: a; xdoc: 'Asset dir(s) to include in build']
}

fn test_edge_case() {
	aio, no_matches := flag.to_struct[AppImageOptions](edge_case, skip: 1)!
	assert aio.verbosity == 3
	assert aio.output == '/tmp/lol.appimage'
	assert aio.input == '/home/user/Projects/game/'
	assert aio.dump_usage == false
	assert aio.show_version == false
	assert aio.assets == []
	assert no_matches == []
}
