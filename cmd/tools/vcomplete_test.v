import os

const vexe = @VEXE
const tfolder = os.join_path(os.vtmp_dir(), 'vcomplete_test')

enum Shell {
	bash
	fish
	zsh
	powershell
}

fn string_to_shell(shell string) !Shell {
	return match shell {
		'bash' {
			.bash
		}
		'fish' {
			.fish
		}
		'zsh' {
			.zsh
		}
		'powershell' {
			.powershell
		}
		else {
			error('${shell} is not supported')
		}
	}
}

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn detect_shell() Shell {
	forced_shell := os.getenv('VTEST_VCOMPLETE_SHELL')
	if forced_shell != '' {
		return string_to_shell(forced_shell) or { panic(err) }
	}
	$if linux || macos {
		return .bash
	}
	$if windows {
		return .powershell
	}
	return .bash
}

struct CompleteTestCase {
	shell     Shell = detect_shell()
	structure []string
	completes map[string][]string
}

const test_cases = [
	CompleteTestCase{
		// Tests the completion of "v complet<tab>" *completeception*
		structure: [] // no test folders
		completes: {
			'v complet': ['complete']
		}
	},
	CompleteTestCase{
		// Tests the completion of "v ru<tab>"
		structure: [] // no test folders
		completes: {
			'v ru': ['run']
		}
	},
	CompleteTestCase{
		// Tests with one sub-folder in test root
		// vfmt off
		structure: [
			os.join_path('examples', 'ex1'), // Empty dir
			os.join_path('examples', 'ex2'), // Two files
			os.join_path('examples', 'ex2', 'ex2_0.v'),
			os.join_path('examples', 'ex2', 'ex2_1.v'),
			os.join_path('examples', 'ex3'), // One file
			os.join_path('examples', 'ex3', 'ex3.v')
		]
		// vfmt on
		completes: {
			'v run e':                  ['examples/']
			'v run':                    ['examples/']
			'v run ':                   ['examples/']
			'v run examples/':          ['ex1', 'ex2', 'ex3']
			'v run examples/ex':        ['examples/ex1/', 'examples/ex2/', 'examples/ex3/']
			'v run examples/ex1':       ['examples/ex1/']
			'v run examples/ex1/':      []
			'v run examples/ex2/':      ['ex2_0.v', 'ex2_1.v']
			'v run examples/ex3/':      ['examples/ex3/ex3.v']
			'v run examples/ex3/ex3.v': ['examples/ex3/ex3.v']
			'v examp':                  ['examples/']
			'v examples/f':             []
			'v examples/':              ['ex1', 'ex2', 'ex3']
			'v examples/ex':            ['examples/ex1/', 'examples/ex2/', 'examples/ex3/']
			'v examples/ex1':           ['examples/ex1/']
			'v examples/ex1/':          []
			'v examples/ex2/':          ['ex2_0.v', 'ex2_1.v']
			'v examples/ex3/':          ['examples/ex3/ex3.v']
			'v examples/ex3/ex3.v':     ['examples/ex3/ex3.v']
		}
	},
	CompleteTestCase{
		// Tests with two sub-folder in test root
		// vfmt off
		structure: [
			os.join_path('sub0', 'ex1'), // Empty dir
			os.join_path('sub0', 'ex2'), // Two files
			os.join_path('sub0', 'ex2', 'ex2_0.v'),
			os.join_path('sub0', 'ex2', 'ex2_1.v'),
			os.join_path('sub0', 'ex3'), // One file
			os.join_path('sub0', 'ex3', 'ex3.v'),
			os.join_path('sub1', 'vex1'),
			os.join_path('sub1', 'vex2'),
			os.join_path('sub1', 'vex3'),
			os.join_path('sub1', 'vex3', 'vex3.v')
		]
		// vfmt on
		completes: {
			// sub0
			'v run ./':             ['sub0/', 'sub1/']
			'v run ./s':            ['./sub0/', './sub1/']
			'v run':                ['sub0/', 'sub1/']
			'v run ':               ['sub0/', 'sub1/']
			'v run sub0/':          ['ex1', 'ex2', 'ex3']
			'v run sub0/ex':        ['sub0/ex1/', 'sub0/ex2/', 'sub0/ex3/']
			'v run sub0/ex1':       ['sub0/ex1/']
			'v run sub0/ex1/':      []
			'v run sub0/ex2/':      ['ex2_0.v', 'ex2_1.v']
			'v run sub0/ex3/':      ['sub0/ex3/ex3.v']
			'v run sub0/ex3/ex3.v': ['sub0/ex3/ex3.v']
			'v su':                 ['sub0/', 'sub1/']
			'v sub':                ['sub0/', 'sub1/']
			'v sub0/f':             []
			'v sub0/':              ['ex1', 'ex2', 'ex3']
			'v sub0/ex':            ['sub0/ex1/', 'sub0/ex2/', 'sub0/ex3/']
			'v sub0/ex1':           ['sub0/ex1/']
			'v sub0/ex1/':          []
			'v sub0/ex2/':          ['ex2_0.v', 'ex2_1.v']
			'v sub0/ex3/':          ['sub0/ex3/ex3.v']
			'v sub0/ex3/ex3.v':     ['sub0/ex3/ex3.v']
			// sub1
			'v run sub1/':          ['vex1', 'vex2', 'vex3']
			'v sub1/vex1/':         []
			'v sub1/vex2/':         []
			'v sub1/vex3/':         ['sub1/vex3/vex3.v']
		}
	},
]

fn run_individual_test(case CompleteTestCase) ! {
	// Clean plate
	os.rmdir_all(tfolder) or {}
	os.mkdir_all(tfolder) or {}
	// Work relative to the test dir
	os.chdir(tfolder) or { panic(err) }
	for relative_path in case.structure {
		path := os.join_path(tfolder, relative_path)
		rel_parts := relative_path.split(os.path_separator)
		if rel_parts.last().contains('.') {
			// Make the containing dir
			os.mkdir_all(os.dir(path)) or { panic(err) }
			// Create empty file
			mut f := os.create(path) or { panic(err) }
			f.close()
		} else {
			os.mkdir_all(path) or { panic(err) }
		}
	}
	for complete, expected in case.completes {
		mut pre_strip := ''
		mut post_strip := ''
		mut complete_command := '${vexe} complete'
		match case.shell {
			.bash {
				pre_strip = "COMPREPLY+=('"
				post_strip = "')"
				complete_command += ' bash'
			}
			.fish {
				complete_command += ' fish'
			}
			.zsh {
				pre_strip = 'compadd -U -S"" -- \''
				post_strip = "';"
				complete_command += ' zsh'
			}
			.powershell {
				complete_command += ' powershell'
			}
		}

		mut normalized_complete := complete
		if case.shell == .powershell {
			normalized_complete = normalized_complete.replace('/', '\\')
		}
		complete_command += ' ${normalized_complete}'
		res := os.execute('${complete_command}')
		mut lines := res.output.split('\n')
		for mut line in lines {
			if case.shell == .powershell {
				line = line.replace('\r', '')
				line = line.all_after('.\\')
			}
			line = line.trim_string_left(pre_strip).trim_string_right(post_strip)
		}
		lines = lines.filter(it != '')
		lines.sort()
		mut sorted_expected := expected.clone()
		if case.shell == .powershell {
			sorted_expected = sorted_expected.map(fn (path string) string {
				mut normalized_path := path.replace('/', '\\')
				normalized_path = normalized_path.all_after('.\\')
				return normalized_path
			})
		}
		sorted_expected.sort()
		// eprintln('${complete} ${lines} vs ${sorted_expected}') // kept for easier debugging

		assert lines.len == sorted_expected.len
		for i, line in lines {
			assert line == sorted_expected[i]
		}
	}
}

fn test_all_complete_cases() {
	for case in test_cases {
		run_individual_test(case)!
	}
}
