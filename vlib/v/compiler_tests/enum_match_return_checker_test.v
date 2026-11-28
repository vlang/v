import os
import v.tempname

fn enum_match_run_to_c(cases map[string]string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_enum_match_return_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	mut imports := []string{cap: cases.len}
	mut calls := []string{cap: cases.len}
	for name, source in cases {
		module_dir := os.join_path(dir, name)
		os.mkdir(module_dir) or { panic(err) }
		imports << 'import ${name}'
		calls << '\t${name}.exercise()'
		fixture := source.replace_once('module main\n', 'module ${name}\n').replace_once('\nfn main()',
			'\npub fn exercise()')
		os.write_file(os.join_path(module_dir, '${name}.v'), fixture) or { panic(err) }
	}
	os.write_file(os.join_path(dir, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {\n${calls.join('\n')}\n}\n') or {
		panic(err)
	}
	return os.exec([@VEXE, '-b', 'c', '-nocolor', '-o', os.join_path(dir, 'program.c'), dir])
}

fn enum_match_check_good(cases map[string]string) {
	result := enum_match_run_to_c(cases)
	assert result.exit_code == 0, result.output
	assert !result.output.contains('missing return'), result.output
}

fn enum_match_check_bad(cases map[string]string) {
	result := enum_match_run_to_c(cases)
	assert result.exit_code != 0, result.output
	mut missing_returns := map[string]bool{}
	for line in result.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		if message.starts_with('missing return at end of function') {
			// Drop the line and column from the right to preserve any colon in the path.
			file := location.all_before_last(':').all_before_last(':')
			missing_returns[os.file_name(file)] = true
		}
	}
	for name, _ in cases {
		assert '${name}.v' in missing_returns, '${name}: expected a missing-return error in\n${result.output}'
	}
}

fn test_exhaustive_enum_match_without_else_definitely_returns() {
	enum_match_check_good({
		'exhaustive': 'module main

enum Color {
	red
	green
	blue
}

fn label(c Color) string {
	match c {
		.red {
			return "red"
		}
		.green {
			return "green"
		}
		.blue {
			return "blue"
		}
	}
}

fn main() {
	println(label(.red))
}
'
		'grouped':    'module main

enum Cell {
	empty
	blocked
	live
}

fn weight(c Cell) int {
	match c {
		.empty, .blocked {
			return 0
		}
		.live {
			return 1
		}
	}
}

fn main() {
	println(weight(.live))
}
'
	})
}

fn test_non_exhaustive_or_unsupported_match_without_else_does_not_return() {
	enum_match_check_bad({
		'partial_enum':         'module main

enum Color {
	red
	green
	blue
}

fn label(c Color) string {
	match c {
		.red {
			return "red"
		}
		.green {
			return "green"
		}
	}
}

fn main() {}
'
		'non_returning_branch': 'module main

enum Color {
	red
	green
	blue
}

fn label(c Color) string {
	match c {
		.red {
			return "red"
		}
		.green {
			x := 1
			_ := x
		}
		.blue {
			return "blue"
		}
	}
}

fn main() {}
'
		'non_enum_match':       'module main

fn label(i int) string {
	match i {
		0 {
			return "zero"
		}
		1 {
			return "one"
		}
	}
}

fn main() {}
'
		'flag_enum':            'module main

@[flag]
enum Permission {
	read
	write
}

fn label(p Permission) string {
	match p {
		.read {
			return "read"
		}
		.write {
			return "write"
		}
	}
}

fn main() {}
'
	})
}
