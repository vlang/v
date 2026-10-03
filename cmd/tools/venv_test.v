import os
import json2

const vexe = os.quoted_path(@VEXE)

// reported_settings is every name `v env` documents. The count matters as much
// as the names: a name added without a test line here would pass unnoticed.
const reported_settings = ['VEXE', 'VROOT', 'VOS', 'VARCH', 'VVERSION', 'VMODULES', 'VTMP', 'V3CACHE',
	'VTOOLS_CACHE_DIR', 'VCACHE', 'VFLAGS', 'VOSARGS', 'CC', 'CFLAGS', 'LDFLAGS', 'VJOBS',
	'VERROR_PATHS', 'VCOLORS', 'VCOVDIR', 'VSTARTUP', 'VQUIET']

// name_and_quoted_value splits one `NAME="value"` line into its two halves.
fn name_and_quoted_value(line string) ![]string {
	at := line.index('=') or { return error('no `=` in `${line}`') }
	return [line[..at], line[at + 1..]]
}

fn test_env_lists_every_reported_setting() {
	res := os.exec([@VEXE, 'env'])
	assert res.exit_code == 0, res.output
	for name in reported_settings {
		assert res.output.contains('${name}="'), '${name} is missing from:\n${res.output}'
	}
}

fn test_env_output_is_one_quoted_setting_per_line() {
	res := os.exec([@VEXE, 'env'])
	assert res.exit_code == 0, res.output
	lines := res.output.trim_space().split_into_lines()
	assert lines.len == reported_settings.len, res.output
	for line in lines {
		parts := name_and_quoted_value(line)!
		assert parts[0] != ''
		assert parts[1].starts_with('"') && parts[1].ends_with('"'), line
	}
}

fn test_env_reports_the_vroot_of_the_running_compiler() {
	res := os.exec([@VEXE, 'env', 'VROOT'])
	assert res.exit_code == 0, res.output
	vroot := res.output.trim_space()
	assert vroot != ''
	assert os.is_dir(os.join_path(vroot, 'vlib', 'v')), vroot
}

fn test_env_prints_one_setting_without_quotes_or_other_lines() {
	res := os.exec([@VEXE, 'env', 'VMODULES'])
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == os.vmodules_dir()
	assert res.output.count('\n') == 1, res.output
	assert !res.output.contains('"')
}

fn test_env_rejects_an_unknown_setting() {
	res := os.exec([@VEXE, 'env', 'VNOT_A_REAL_SETTING'])
	assert res.exit_code == 1
	assert res.output.contains('unknown setting `VNOT_A_REAL_SETTING`.'), res.output
	assert res.output.contains('Known settings:'), res.output
}

fn test_env_json_reports_the_same_settings_as_the_text_output() {
	text := os.exec([@VEXE, 'env'])
	assert text.exit_code == 0, text.output
	json_res := os.exec([@VEXE, 'env', '-json'])
	assert json_res.exit_code == 0, json_res.output
	reported := json2.decode[map[string]string](json_res.output) or {
		panic('cannot decode ${json_res.output}: ${err}')
	}
	assert reported.len == reported_settings.len, json_res.output
	for line in text.output.trim_space().split_into_lines() {
		parts := name_and_quoted_value(line)!
		name := parts[0]
		assert name in reported, '${name} is missing from the json output'
		assert reported[name] == json2.decode[string](parts[1])!, '${name} differs between the two outputs'
	}
}

fn test_env_json_values_match_the_single_setting_output() {
	all := os.exec([@VEXE, 'env', '--json'])
	assert all.exit_code == 0, all.output
	reported := json2.decode[map[string]string](all.output) or { panic(err) }
	for name in ['VROOT', 'VMODULES', 'VTMP', 'VOS', 'VARCH'] {
		one := os.exec([@VEXE, 'env', '${name}'])
		assert one.exit_code == 0, one.output
		assert reported[name] == one.output.trim_space(), name
	}
}

fn test_env_help_describes_the_output() {
	res := os.exec([@VEXE, 'env', '--help'])
	assert res.exit_code == 0, res.output
	assert res.output.contains('Usage: v env [options] [NAME]'), res.output
	assert res.output.contains('json'), res.output
}

fn test_env_quoted_values_round_trip_quotes_and_control_characters() {
	tool := os.join_path(os.vtmp_dir(), 'venv_escape_test_${os.getpid()}')
	built := os.exec([@VEXE, '-o', '${tool}', os.join_path(@VEXEROOT, 'cmd/tools/venv.v')])
	assert built.exit_code == 0, built.output
	defer { os.rm(tool) or {} }
	previous := os.getenv_opt('CFLAGS')
	defer {
		if old := previous { os.setenv('CFLAGS', old, true) } else { os.unsetenv('CFLAGS') }
	}
	flags := '-DNAME="hello world"\n-DPATH=C:\\build\tsecond\rline' + u8(1).ascii_str()
	os.setenv('CFLAGS', flags, true)
	text := os.exec(['${tool}'])
	assert text.exit_code == 0, text.output
	lines := text.output.trim_space().split_into_lines()
	assert lines.len == reported_settings.len, text.output
	mut encoded_flags := ''
	for line in lines {
		parts := name_and_quoted_value(line)!
		if parts[0] == 'CFLAGS' { encoded_flags = parts[1] }
	}
	assert json2.decode[string](encoded_flags)! == flags
	all := os.exec(['${tool}', '-json'])
	assert all.exit_code == 0, all.output
	reported := json2.decode[map[string]string](all.output)!
	assert reported['CFLAGS'] == flags
	one := os.exec(['${tool}', 'CFLAGS'])
	assert one.exit_code == 0, one.output
	assert one.output == flags + '\n'
}
