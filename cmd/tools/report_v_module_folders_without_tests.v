module main

import os

const vexe = @VEXE

const known_skip_patterns_env = os.getenv('VKNOWN_SKIP_PATTERNS')

const known_folder_patterns_that_are_not_module_ones = [
	'vlib/sync/bench',
	'/tests',
	'/testdata',
	'/preludes_js',
	'vlib/builtin/js', // TODO: fix compiler panic
	'vlib/fontstash', // used by `gg`
	'vlib/sokol/sfons', // used by `gg`, and by examples/sokol/fonts.v
	'vlib/sokol/sapp', // used by `gg`, and many examples/
	'vlib/sokol/gfx', // used by `gg`, `x.ttf`
	'vlib/sokol/sgl', // used by `gg`
	'vlib/toml', // toml is well tested, even if the top level folder does not have _test.v files, the ones below do
	'vlib/v/', // the compiler itself is well tested
]

fn main() {
	mut places := if os.args.len > 1 {
		os.args#[1..]
	} else {
		eprintln('> check the current folder only by default ...')
		['.']
	}
	mut known_skip_patterns := known_folder_patterns_that_are_not_module_ones.clone()
	if known_skip_patterns_env != '' {
		known_skip_patterns = known_skip_patterns_env.split(',').filter(it != '')
	}
	for path in places {
		eprintln('> Checking folder: `${path}` ...')
		mut found := 0
		files := os.walk_ext(path, '.v')
		mut v_files := map[string]int{}
		mut v_test_files := map[string]int{}
		for f in files {
			folder := os.to_slash(os.dir(f))
			if known_skip_patterns.any(f.contains(it)) {
				continue
			}
			if f.ends_with('.v') {
				v_files[folder]++
			}
			if f.ends_with('_test.v') {
				v_test_files[folder]++
			}
		}
		eprintln('> Found ${v_files.len:5} potential V module folders (containing .v files).')
		for folder, n_v_files in v_files {
			n_test_v_files := v_test_files[folder]
			if n_v_files > 1 && n_test_v_files == 0 {
				println('> ${n_test_v_files:5} _test.v files, with ${n_v_files:5} .v files, in folder: ${folder}')
				compilation := os.execute('${os.quoted_path(vexe)} -shared -W -Wfatal-errors -check ${os.quoted_path(folder)}')
				if compilation.exit_code != 0 {
					eprintln('> ${folder} has parser/checker errors!')
					eprintln(compilation.output)
				}
				found++
			}
		}
		eprintln('> Found ${found} module folders without _test.v files in `${path}` .')
	}
}
