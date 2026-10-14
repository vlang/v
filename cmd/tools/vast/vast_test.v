module main

import os

// vast_test covers the argument handling of `v ast`. Everything else in this
// tool either shells out (`write_file_or_print` runs `VEXE -check` and the
// watch loop runs `VEXE -o ... .c`), or writes JSON next to the input file.

fn test_absolute_path_keeps_absolute_input() {
	assert absolute_path(os.join_path(os.vtmp_dir(), 'demo.v')) == os.join_path(os.vtmp_dir(),
		'demo.v')
}

fn test_absolute_path_resolves_a_relative_input_against_the_cwd() {
	assert absolute_path('vast.v').starts_with(os.getwd())
	assert absolute_path('vast.v') == os.join_path(os.getwd(), 'vast.v')
}

fn test_absolute_path_handles_nested_relative_input() {
	assert absolute_path('vast/test/demo.v') == os.join_path(os.getwd(), 'vast', 'test', 'demo.v')
}

// NOTE: the `./` branch strips the prefix and rejoins with `os.getwd()`, which
// makes `./x` and `x` indistinguishable. Both resolve identically.
fn test_absolute_path_treats_dot_slash_as_a_plain_relative_path() {
	assert absolute_path('./vast.v') == absolute_path('vast.v')
	assert absolute_path('./vast.v') == os.join_path(os.getwd(), 'vast.v')
}

fn test_absolute_path_does_not_require_the_file_to_exist() {
	missing := absolute_path('vast_missing_${os.getpid()}.v')
	assert missing == os.join_path(os.getwd(), 'vast_missing_${os.getpid()}.v')
	assert !os.exists(missing)
}
