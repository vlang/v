import os

const vexe = @VEXE
const tfolder = os.join_path(os.vtmp_dir(), 'vbump_${os.getpid()}')

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

fn run_bump_cases(cases []BumpTestCase, options []string) ! {
	assert cases.len > 0
	mut paths := []string{cap: cases.len}
	for case in cases {
		path := os.join_path_single(tfolder, case.file_name)
		os.write_file(path, case.contents)!
		paths << path
	}
	for increment_index, increment in ['--patch', '--minor', '--major'] {
		os.exec_or_exit([vexe, 'bump', increment, ...options, ...paths])
		for i, case in cases {
			lines := os.read_lines(paths[i])!
			expected := [case.expected_patch, case.expected_minor, case.expected_major]
			assert lines[case.line] == expected[increment_index], '${increment} ${case.file_name}'
		}
	}
}

fn test_all_bump_cases() ! {
	run_bump_cases(test_cases, [])!
}

const skip_test_cases = [
	BumpTestCase{
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
		expected_patch: 'version: 0.2.5'
		expected_minor: 'version: 0.3.0'
		expected_major: 'version: 1.0.0'
	},
]

fn test_all_skip_bump_cases() ! {
	run_bump_cases(skip_test_cases, ['--skip=cff-version'])!
}
