module x64

import os

fn test_native_stdlib_core_semantics() {
	$if linux && amd64 {
		path := os.join_path(os.vtmp_dir(), 'x64_stdlib_semantics_${os.getpid()}.v')
		output := path.all_before_last('.')
		defer {
			os.rm(path) or {}
			os.rm(output) or {}
		}
		os.write_file(path, r'module main
import strconv
import strings

fn C.exit(int)

struct Box[T] {
    values []T
}

fn make_box[T](values []T) Box[T] {
    return Box[T]{
        values: values
    }
}

fn main() {
    if strconv.atoi("-121")! != -121 { C.exit(1) }
    if strconv.atoi16("-32768")! != -32768 { C.exit(2) }
    if strconv.atoi64("-9223372036854775807")! != -9223372036854775807 { C.exit(3) }
    if strconv.atof_quick("1.5") != 1.5 { C.exit(4) }
    if strconv.atof_quick("42abc") != 42.0 { C.exit(5) }
    mut value := 1.5
    value *= 2.0
    value += 0.25
    if value != 3.25 { C.exit(6) }
    mut error_message_ok := false
    _ := strconv.parse_uint("1_000", 10, 0) or {
        error_message_ok = err.msg().contains("syntax error")
        0
    }
    if !error_message_ok {
        C.exit(8)
    }
    mut builder := strings.new_builder(8)
    builder.write_string("hello")
    if builder.last_n(99) != "" { C.exit(9) }
    if builder.str() != "hello" { C.exit(10) }
    builder.write_string("again")
    if builder.str() != "again" { C.exit(11) }
    box := make_box([1, 2, 3])
    if box.values.len != 3 || box.values[2] != 3 { C.exit(12) }
    mut reversed := [1, 2, 3]
    reversed = reversed.reverse()
    if reversed != [3, 2, 1] { C.exit(13) }
}
')!
		compiled := os.exec([@VEXE, '-nocache', '-b', 'x64', '-o', output, path])
		assert compiled.exit_code == 0, compiled.output
		result := os.exec([output])
		assert result.exit_code == 0, 'exit ${result.exit_code}: ${result.output}'
	}
}
