import os

const embedded_method_source = 'struct Left {}

fn (l Left) label() string {
	return "left"
}

struct Right {}

fn (r Right) label() string {
	return "right"
}

struct Middle {
	Left
	Right
}

struct Outer {
	Middle
}
'

fn check_embedded_method_program(name string, body string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'v_embedded_method_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), embedded_method_source + body) or { panic(err) }
	return os.exec([@VEXE, '-check', root])
}

// A method that two embedded structs both have is ambiguous, also when the
// struct embedding them is itself embedded.
fn test_method_of_two_embedded_structs_is_ambiguous() {
	direct := check_embedded_method_program('direct', 'fn main() {\n\tprintln(Middle{}.label())\n}\n')
	assert direct.exit_code != 0, direct.output
	assert direct.output.contains('ambiguous method `label`'), direct.output
	nested := check_embedded_method_program('nested', 'fn main() {\n\tprintln(Outer{}.label())\n}\n')
	assert nested.exit_code != 0, nested.output
	assert nested.output.contains('ambiguous method `label`'), nested.output
}

// A method of the struct itself, or of the nearest embedded struct, is the one called.
fn test_nearest_method_is_not_ambiguous() {
	own := check_embedded_method_program('own', 'fn (m Middle) label() string {\n\treturn "middle"\n}\n\nfn main() {\n\tprintln(Middle{}.label())\n\tprintln(Outer{}.label())\n}\n')
	assert own.exit_code == 0, own.output
}
