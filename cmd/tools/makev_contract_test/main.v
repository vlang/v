module main

import os

fn routine(source string, name string) string {
	mut start := -1
	for i, line in source.split_into_lines() {
		if line == ':' + name {
			start = i
			break
		}
	}
	if start < 0 {
		return ''
	}
	mut end := source.split_into_lines().len
	for j := start + 1; j < source.split_into_lines().len; j++ {
		if source.split_into_lines()[j].starts_with(':') && source.split_into_lines()[j].len > 1 {
			end = j
			break
		}
	}
	return source.split_into_lines()[start..end].join('\n')
}

fn main() {
	source := os.read_file(os.join_path(@DIR, '..', '..', '..', 'makev.bat')) or {
		eprintln('cannot read makev.bat: ${err}')
		exit(1)
	}
	assert source.contains('set VC_BOOTSTRAP_DEFINE=-DCUSTOM_DEFINE_v1_fallback\n') ||
		source.contains('set VC_BOOTSTRAP_DEFINE=-DCUSTOM_DEFINE_v1_fallback\r\n')

	for compiler in ['tcc', 'clang', 'gcc'] {
		r := routine(source, 'build_bootstrap_with_' + compiler)
		assert r.len > 0, 'missing routine build_bootstrap_with_${compiler}'
		mut found := 0
		for line in r.split_into_lines() {
			if line.starts_with('"!' + compiler + '_exe!" ') && line.contains('"%V_C_FILE%"') {
				assert line.contains('%VC_BOOTSTRAP_DEFINE%')
				found++
			}
		}
		assert found == 1, 'expected 1 bootstrap command for ${compiler}, got ${found}'
	}

	// MSVC
	r := routine(source, 'build_bootstrap_with_msvc')
	mut msvc_found := 0
	for line in r.split_into_lines() {
		if line.starts_with('cl ') && line.contains('"%V_C_FILE%"') {
			assert line.contains('%VC_BOOTSTRAP_DEFINE%')
			msvc_found++
		}
	}
	assert msvc_found == 1, 'expected 1 MSVC bootstrap command, got ${msvc_found}'

	// native helpers
	mut native_line := ''
	for line in source.split_into_lines() {
		if line.starts_with('set V_BOOTSTRAP_NATIVE=') {
			native_line = line
			break
		}
	}
	assert native_line.len > 0, 'missing V_BOOTSTRAP_NATIVE'
	assert native_line.contains('.c'), 'V_BOOTSTRAP_NATIVE should reference C helpers'

	println('makev bootstrap configuration tests: OK')
}
