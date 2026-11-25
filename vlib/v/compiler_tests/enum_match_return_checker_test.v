import os
import v.tempname

fn enum_match_run_to_c(name string, src string) os.Result {
	dir := os.join_path(os.vtmp_dir(), 'v3_enum_match_return_${tempname.unique_token()}')
	os.mkdir(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	base := os.join_path(dir, 'v3_enum_match_return_${name}')
	src_path := base + '.v'
	os.write_file(src_path, src) or { panic(err) }
	return os.exec([@VEXE, src_path, '-b', 'c', '-o', base + '.c'])
}

fn enum_match_check_good(name string, src string) {
	result := enum_match_run_to_c(name, src)
	assert result.exit_code == 0, result.output
	assert !result.output.contains('missing return'), result.output
}

fn enum_match_check_bad(name string, src string) {
	result := enum_match_run_to_c(name, src)
	assert result.exit_code != 0, result.output
	assert result.output.contains('missing return at end of function'), result.output
}

fn test_exhaustive_enum_match_without_else_definitely_returns() {
	enum_match_check_good('exhaustive', 'module main

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
	enum_match_check_good('grouped', 'module main

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
	enum_match_check_bad('partial_enum', 'module main

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
	enum_match_check_bad('non_returning_branch', 'module main

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
	enum_match_check_bad('non_enum_match', 'module main

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
	enum_match_check_bad('flag_enum', 'module main

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
