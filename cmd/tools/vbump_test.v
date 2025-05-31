import os

const vexe = @VEXE
const tfolder = os.join_path(os.vtmp_dir(), 'vbump')

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

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
		file_name:      'v.mod'
		contents:       "Module {
	name: 'Sample'
	description: 'Sample project'
	version: '1.2.6'
	license: 'MIT'
	dependencies: []
}

"
		line:           3
		expected_patch: "	version: '1.2.7'"
		expected_minor: "	version: '1.3.0'"
		expected_major: "	version: '2.0.0'"
	},
	BumpTestCase{
		file_name:      'random_versions.vv'
		contents:       "
1.1.2
1.2.5
3.21.73
version = '1.5.1'

"
		line:           4
		expected_patch: "version = '1.5.2'"
		expected_minor: "version = '1.6.0'"
		expected_major: "version = '2.0.0'"
	},
	BumpTestCase{
		file_name:      'sample_tool.v'
		contents:       "// Module comment and copyright information
import os
import flag

const tool_name = os.file_name(os.executable())
const tool_version = '0.1.33'

fn main() {
	// stuff
}
	"
		line:           5
		expected_patch: "const tool_version = '0.1.34'"
		expected_minor: "const tool_version = '0.2.0'"
		expected_major: "const tool_version = '1.0.0'"
	},
]

fn run_individual_test(case BumpTestCase) ! {
	test_file := os.join_path_single(tfolder, case.file_name)

	os.rm(test_file) or {}
	os.write_file(test_file, case.contents)!

	os.execute_or_exit('${os.quoted_path(vexe)} bump --patch ${os.quoted_path(test_file)}')
	patch_lines := os.read_lines(test_file)!
	assert patch_lines[case.line] == case.expected_patch

	os.execute_or_exit('${os.quoted_path(vexe)} bump --minor ${os.quoted_path(test_file)}')
	minor_lines := os.read_lines(test_file)!
	assert minor_lines[case.line] == case.expected_minor

	os.execute_or_exit('${os.quoted_path(vexe)} bump --major ${os.quoted_path(test_file)}')
	major_lines := os.read_lines(test_file)!
	assert major_lines[case.line] == case.expected_major

	os.rm(test_file)!
}

fn test_all_bump_cases() {
	for case in test_cases {
		run_individual_test(case) or { panic(err) }
	}
}

struct SkipTestCase {
	file_name      string
	contents       string
	skip           string
	line           int
	expected_patch string
	expected_minor string
	expected_major string
}

const skip_test_cases = [
	SkipTestCase{
		file_name:      'CITATION.cff'
		contents:       'abstract: A sample CLI tool made in V that prints geometric shapes to the screen.
authors:
  - alias: hungrybluedev
    family-names: Haldar
    given-names: Subhomoy
cff-version: 1.2.0
date-released: 2023-04-20
license: MIT
message: Please cite this software using these information.
repository-code: https://github.com/hungrybluedev/geo
title: geo
url: https://github.com/hungrybluedev/geo
version: 0.2.4
'
		line:           12
		skip:           'cff-version'
		expected_patch: 'version: 0.2.5'
		expected_minor: 'version: 0.3.0'
		expected_major: 'version: 1.0.0'
	},
]

fn run_skip_test(case SkipTestCase) ! {
	test_file := os.join_path_single(tfolder, case.file_name)

	os.rm(test_file) or {}
	os.write_file(test_file, case.contents)!

	os.execute_or_exit('${os.quoted_path(vexe)} bump --patch --skip="${case.skip}" ${os.quoted_path(test_file)}')
	patch_lines := os.read_lines(test_file)!
	assert patch_lines[case.line] == case.expected_patch

	os.execute_or_exit('${os.quoted_path(vexe)} bump --minor --skip="${case.skip}" ${os.quoted_path(test_file)}')
	minor_lines := os.read_lines(test_file)!
	assert minor_lines[case.line] == case.expected_minor

	os.execute_or_exit('${os.quoted_path(vexe)} bump --major --skip="${case.skip}" ${os.quoted_path(test_file)}')
	major_lines := os.read_lines(test_file)!
	assert major_lines[case.line] == case.expected_major

	os.rm(test_file)!
}

fn test_all_skip_bump_cases() ! {
	for case in skip_test_cases {
		run_skip_test(case) or { panic(err) }
	}
}
