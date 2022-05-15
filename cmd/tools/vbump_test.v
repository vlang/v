import os

struct BumpTestCase {
	file_name      string
	contents       string
	line           int
	expected_patch string
	expected_minor string
	expected_major string
}

const test_cases = [
	BumpTestCase{
		file_name: 'v.mod'
		contents: "Module {
	name: 'Sample'
	description: 'Sample project'
	version: '1.2.6'
	license: 'MIT'
	dependencies: []
}

"
		line: 3
		expected_patch: "	version: '1.2.7'"
		expected_minor: "	version: '1.3.0'"
		expected_major: "	version: '2.0.0'"
	},
	BumpTestCase{
		file_name: 'random_versions.vv'
		contents: "
1.1.2
1.2.5
3.21.73
version = '1.5.1'

"
		line: 4
		expected_patch: "version = '1.5.2'"
		expected_minor: "version = '1.6.0'"
		expected_major: "version = '2.0.0'"
	},
	BumpTestCase{
		file_name: 'sample_tool.v'
		contents: "// Module comment and copyright information
import os
import flag

const (
	tool_name        = os.file_name(os.executable())
	tool_version     = '0.1.33'
)
fn main() {
	// stuff
}
	"
		line: 6
		expected_patch: "	tool_version     = '0.1.34'"
		expected_minor: "	tool_version     = '0.2.0'"
		expected_major: "	tool_version     = '1.0.0'"
	},
]

fn run_individual_test(case BumpTestCase) ? {
	vexe := @VEXE

	temp_dir := os.temp_dir()
	test_file := os.join_path_single(temp_dir, case.file_name)

	os.rm(test_file) or {}
	os.write_file(test_file, case.contents)?
	//
	os.execute_or_exit('${os.quoted_path(vexe)} bump --patch ${os.quoted_path(test_file)}')
	patch_lines := os.read_lines(test_file)?
	assert patch_lines[case.line] == case.expected_patch

	os.execute_or_exit('${os.quoted_path(vexe)} bump --minor ${os.quoted_path(test_file)}')
	minor_lines := os.read_lines(test_file)?
	assert minor_lines[case.line] == case.expected_minor

	os.execute_or_exit('${os.quoted_path(vexe)} bump --major ${os.quoted_path(test_file)}')
	major_lines := os.read_lines(test_file)?
	assert major_lines[case.line] == case.expected_major
	//
	os.rm(test_file)?
}

fn test_all_bump_cases() {
	for case in test_cases {
		run_individual_test(case) or { panic(err) }
	}
}
