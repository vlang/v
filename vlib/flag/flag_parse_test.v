import flag

pub struct Options {
pub:
	verbosity  int
	dump_usage bool
pub mut:
	additional_args []string
	archs           []string
	v_flags         []string
	api_level       string
}

fn test_flag_parse() {
	args := ['/my/app', '-v', '3', '-f', '-d sdl_memory_no_gc', '-f', '-d shy_use_wren', '--api',
		'21', '--archs', 'arm64-v8a', '/path/to/input.v']

	mut fp := flag.new_flag_parser(args)
	fp.application('bug')
	fp.version('0.2.0')
	fp.description('bugged')
	fp.arguments_description('not important')

	fp.skip_executable()

	mut opt := Options{
		v_flags:    fp.string_multi('flag', `f`, 'Additional flags for the V compiler')
		archs:      fp.string('archs', 0, 'arm64-v8a,armeabi-v7a,x86,x86_64', 'Comma separated string with any of archs').split(',')
		dump_usage: fp.bool('help', `h`, false, 'Show this help message and exit')
		verbosity:  fp.int_opt('verbosity', `v`, 'Verbosity level 1-3') or { 0 }
		api_level:  fp.string('api', 0, '21', 'Android API level to use (--list-apis)')
	}

	opt.additional_args = fp.finalize() or { panic(err) }

	assert opt.v_flags[0] == '-d sdl_memory_no_gc'
	assert opt.v_flags[1] == '-d shy_use_wren' // looks like the builtin support for `-h` eats the "h" in this flag
	assert opt.dump_usage == false
}
