import os

const vexe = @VEXE

// Diagnostics produced by the whole-program checker passes must not depend on the
// scheduling mode: `-no-parallel` selects a different check_semantics entry point,
// which used to skip those passes entirely (#28792).
struct PassCase {
	name     string
	source   string
	expected string
}

const cases = [
	PassCase{
		name:     'deprecated_byte.v'
		source:   'fn main() {\n\tx := byte(1)\n\tprintln(x)\n}\n'
		expected: 'byte is deprecated, use u8 instead'
	},
	PassCase{
		// Several hits of one pass in one file share an anchor node; each must survive.
		name:     'deprecated_byte_multiple.v'
		source:   'fn foo(_ byte) {}\n\nfn main() {\n\tfoo(byte(0))\n\t_ := fn (_ byte) {}\n}\n'
		expected: 'byte is deprecated, use u8 instead'
	},
	PassCase{
		name:     'goto_label.v'
		source:   'fn main() {\n\tunsafe {\n\t\tgoto nowhere\n\t}\n}\n'
		expected: 'unknown label `nowhere`'
	},
	PassCase{
		name:     'loop_label.v'
		source:   'fn main() {\n\tfor i in 0 .. 3 {\n\t\tif i == 1 {\n\t\t\tbreak nolabel\n\t\t}\n\t}\n}\n'
		expected: 'invalid label name `nolabel`'
	},
	PassCase{
		name:     'c_generic.v'
		source:   'struct C.Foo[T] {\n\tx T\n}\n\nfn main() {\n\tprintln(1)\n}\n'
		expected: 'C structs cannot be declared as generic'
	},
	PassCase{
		name:     'builtin_import.v'
		source:   'import builtin { string }\n\nfn main() {\n\tprintln(1)\n}\n'
		expected: 'cannot import or override builtin type'
	},
	// Per-declaration checks of the top-level walk.
	PassCase{
		name:     'enum_backing.v'
		source:   'enum Color as f64 {\n\tred\n}\n\nfn main() {\n\tprintln(Color.red)\n}\n'
		expected: '`f64` is not one of `i8`,`i16`,`i32`,`int`,`i64`,`u8`,`u16`,`u32`,`u64`'
	},
	PassCase{
		name:     'struct_name.v'
		source:   'struct my_struct {\n\tx int\n}\n\nfn main() {\n\tprintln(my_struct{}.x)\n}\n'
		expected: 'struct name `my_struct` must begin with capital letter'
	},
	PassCase{
		name:     'struct_implements.v'
		source:   'interface Shape {\n\tarea() int\n}\n\nstruct Square implements Shape {\n\tside int\n}\n\nfn main() {\n\tprintln(Square{}.side)\n}\n'
		expected: "`Square` doesn't implement method `area` of interface `Shape`"
	},
]

fn diagnostic_lines(output string) []string {
	return output.split_into_lines().filter(it.contains(' error: ') || it.contains(' warning: '))
}

fn test_no_parallel_reports_the_same_whole_program_diagnostics() {
	dir := os.join_path(os.vtmp_dir(), 'no_parallel_passes_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	for c in cases {
		path := os.join_path(dir, c.name)
		os.write_file(path, c.source)!
		default_res := os.execute('${os.quoted_path(vexe)} -check ${os.quoted_path(path)}')
		serial_res := os.execute('${os.quoted_path(vexe)} -no-parallel -check ${os.quoted_path(path)}')
		assert default_res.output.contains(c.expected), '${c.name} (default): ${default_res.output}'
		assert serial_res.output.contains(c.expected), '${c.name} (-no-parallel): ${serial_res.output}'
		// The passes must run exactly once in both modes.
		assert diagnostic_lines(serial_res.output).len == diagnostic_lines(default_res.output).len, '${c.name}: default:\n${default_res.output}\n-no-parallel:\n${serial_res.output}'
	}
}
