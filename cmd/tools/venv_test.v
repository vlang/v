import os
import json2

const vexe = os.quoted_path(@VEXE)

// reported_settings is every name `v env` documents. The count matters as much
// as the names: a name added without a test line here would pass unnoticed.
const reported_settings = ['VEXE', 'VROOT', 'VOS', 'VARCH', 'VVERSION', 'VMODULES', 'VTMP',
	'V3CACHE', 'VTOOLS_CACHE_DIR', 'VCACHE', 'VFLAGS', 'VOSARGS', 'CC', 'CFLAGS', 'LDFLAGS',
	'VJOBS', 'VERROR_PATHS', 'VCOLORS', 'VCOVDIR', 'VSTARTUP', 'VQUIET']

// name_and_quoted_value splits one `NAME="value"` line into its two halves.
fn name_and_quoted_value(line string) ![]string {
	at := line.index('=') or { return error('no `=` in `${line}`') }
	return [line[..at], line[at + 1..]]
}

fn test_env_lists_every_reported_setting() {
	res := os.execute('${vexe} env')
	assert res.exit_code == 0, res.output
	for name in reported_settings {
		assert res.output.contains('${name}="'), '${name} is missing from:\n${res.output}'
	}
}

fn test_env_output_is_one_quoted_setting_per_line() {
	res := os.execute('${vexe} env')
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
	res := os.execute('${vexe} env VROOT')
	assert res.exit_code == 0, res.output
	vroot := res.output.trim_space()
	assert vroot != ''
	assert os.is_dir(os.join_path(vroot, 'vlib', 'v')), vroot
}

fn test_env_prints_one_setting_without_quotes_or_other_lines() {
	res := os.execute('${vexe} env VMODULES')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == os.vmodules_dir()
	assert res.output.count('\n') == 1, res.output
	assert !res.output.contains('"')
}

fn test_env_rejects_an_unknown_setting() {
	res := os.execute('${vexe} env VNOT_A_REAL_SETTING')
	assert res.exit_code == 1
	assert res.output.contains('unknown setting `VNOT_A_REAL_SETTING`.'), res.output
	assert res.output.contains('Known settings:'), res.output
}

fn test_env_json_reports_the_same_settings_as_the_text_output() {
	text := os.execute('${vexe} env')
	assert text.exit_code == 0, text.output
	json_res := os.execute('${vexe} env -json')
	assert json_res.exit_code == 0, json_res.output
	reported := json2.decode[map[string]string](json_res.output) or {
		panic('cannot decode ${json_res.output}: ${err}')
	}
	assert reported.len == reported_settings.len, json_res.output
	for line in text.output.trim_space().split_into_lines() {
		parts := name_and_quoted_value(line)!
		name := parts[0]
		assert name in reported, '${name} is missing from the json output'
		assert reported[name] == parts[1].trim('"'), '${name} differs between the two outputs'
	}
}

fn test_env_json_values_match_the_single_setting_output() {
	all := os.execute('${vexe} env --json')
	assert all.exit_code == 0, all.output
	reported := json2.decode[map[string]string](all.output) or { panic(err) }
	for name in ['VROOT', 'VMODULES', 'VTMP', 'VOS', 'VARCH'] {
		one := os.execute('${vexe} env ${name}')
		assert one.exit_code == 0, one.output
		assert reported[name] == one.output.trim_space(), name
	}
}

fn test_env_help_describes_the_output() {
	res := os.execute('${vexe} env --help')
	assert res.exit_code == 0, res.output
	assert res.output.contains('Usage: v env [options] [NAME]'), res.output
	assert res.output.contains('json'), res.output
}