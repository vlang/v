module driver

import os

fn test_source_imports_fast_tracks_nested_interpolation_strings() {
	path := os.join_path(os.temp_dir(), 'v3_eager_import_scan_${os.getpid()}.v')
	defer {
		os.rm(path) or {}
	}
	dollar := '$'
	source := 'module scan_fixture

import real_module

fn interpolate(value string) string {
	return value
}

const text = \'before ${dollar}{interpolate(\'it\\\'s\')}
import fake_module
after\'
'
	expected_interpolation := dollar + "{interpolate('it\\'s')}"
	assert source.contains(expected_interpolation)
	os.write_file(path, source) or { panic(err) }

	assert source_imports_fast(path) == ['real_module']
}
