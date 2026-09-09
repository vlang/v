import os

const enum_match_vexe = @VEXE
const enum_match_tests_dir = os.dir(@FILE)
const enum_match_v3_dir = os.dir(enum_match_tests_dir)
const enum_match_vlib_dir = os.dir(enum_match_v3_dir)
const enum_match_v3_src = os.join_path(enum_match_v3_dir, 'v3.v')

fn enum_match_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_enum_match_return_checker_test')
	build :=
		os.execute('${enum_match_vexe} -gc none -path "${enum_match_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${enum_match_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn enum_match_run_to_c(v3_bin string, name string, src string) os.Result {
	src_path := os.join_path(os.temp_dir(), 'v3_enum_match_return_${name}.v')
	os.write_file(src_path, src) or { panic(err) }
	c_path := os.join_path(os.temp_dir(), 'v3_enum_match_return_${name}.c')
	os.rm(c_path) or {}
	return os.execute('${v3_bin} ${src_path} -b c -o ${c_path}')
}

fn enum_match_check_good(v3_bin string, name string, src string) {
	result := enum_match_run_to_c(v3_bin, name, src)
	assert result.exit_code == 0, result.output
	assert !result.output.contains('missing return'), result.output
}

fn enum_match_check_bad(v3_bin string, name string, src string) {
	result := enum_match_run_to_c(v3_bin, name, src)
	assert result.exit_code != 0, result.output
	assert result.output.contains('missing return at end of function'), result.output
}

fn test_exhaustive_enum_match_without_else_definitely_returns() {
	v3_bin := enum_match_build_v3()
	enum_match_check_good(v3_bin, 'exhaustive', 'module main

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
')
	enum_match_check_good(v3_bin, 'grouped', 'module main

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
')
}

fn test_non_exhaustive_or_unsupported_match_without_else_does_not_return() {
	v3_bin := enum_match_build_v3()
	enum_match_check_bad(v3_bin, 'partial_enum', 'module main

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
')
	enum_match_check_bad(v3_bin, 'non_returning_branch', 'module main

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
')
	enum_match_check_bad(v3_bin, 'non_enum_match', 'module main

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
')
	enum_match_check_bad(v3_bin, 'flag_enum', 'module main

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
')
}
