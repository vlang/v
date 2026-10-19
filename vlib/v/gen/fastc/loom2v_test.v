module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_directory_entry_with_toml_and_function_literals() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_loom2v_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_loom2v' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'native.c'), 'int native_answer(void) { return 42; }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'native.h'), 'int native_answer(void);\n') or {
		panic(err)
	}
	os.mkdir_all(os.join_path(root, 'typesonly')) or { panic(err) }
	os.write_file(os.join_path(root, 'unused.c'), '#error unused native input was compiled\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'typesonly', 'types.v'), r'module typesonly
#flag @VMODROOT/unused.c
fn C.unused_native_answer() i32
pub struct Marker {
	value int
}
pub interface NativeValue {
	native_answer() i32
}
pub fn (m Marker) native_answer() i32 {
	return C.unused_native_answer()
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'helpers.v'), r'module main

import toml

#flag @VMODROOT/native.c
#include "@VMODROOT/native.h"
fn C.native_answer() i32

type Words = [2]u16

const properties = [
	1
	2
	3
]!

struct Packet {
	words Words
}

struct Owner {
	publish fn (string) string
}

struct Entry {
	value int
}

fn pair() (Entry, string) {
	return Entry{value: 42}, "ok"
}

fn choose(flag bool) Entry {
	entry, _ := if flag { pair() } else { pair() }
	return entry
}

fn result_entry(ok bool) !Entry {
	if !ok {
		return error("entry missing")
	}
	return Entry{value: 42}
}

fn result_field(ok bool) !int {
	return (result_entry(ok)!).value
}

fn pick[T](first T, second T, unused voidptr) T {
	return if first < second { first } else { second }
}

fn make_owner(index map[string]int) Owner {
	return Owner{
		publish: fn [index] (name string) string {
			if value := index[name] {
				return "${value}"
			}
			return "missing"
		}
	}
}

fn table(value toml.Any) map[string]toml.Any {
	if value !is map[string]toml.Any {
		return map[string]toml.Any{}
	}
	return value.as_map()
}

fn array_length(value toml.Any) int {
	if value is []toml.Any {
		return value.len
	}
	return -1
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main

import toml
import typesonly
import math
import os
import v.pref

@[if fastc_loom_debug ?]
fn debug(value string) {
	println(value)
}

fn main() {
	assert os.environ().len > 0
	assert pref.module_search_stop_marker == ".v.mod.stop"
	_ := typesonly.Marker{}
	doc := toml.parse_text("[item]\nvalue = 42\nitems = [1, 2]\n") or { panic(err) }
	println(C.native_answer())
	assert properties.len == 3
	index := 1
	minimum := [u32(0x80), 0x800, 0x10000][index]
	assert minimum == u32(0x800)
	address := usize(0x12345)
	unsafe {
		assert usize(voidptr(address)) == address
	}
	assert "${u8(0):02x}" == "00"
	assert "${u8(255):02x}" == "ff"
	assert choose(true).value == 42
	assert choose(false).value == 42
	field_value := result_field(true)!
	assert field_value == 42
	failed_field := result_field(false) or {
		assert err.msg() == "entry missing"
		-1
	}
	assert failed_field == -1
	assert pick(18, 2, unsafe { nil }) == 2
	assert math.min(18, 2) == 2
	base := ["a", "b"]
	assert [...base, "c", ...base].join(",") == "a,b,c,a,b"
	values := table(doc.value("item"))
	mut placement := map[string]bool{}
	placement["remote"] = (values["absent"] or { toml.Any(false) }).bool() || "m4" != ""
	assert placement["remote"]
	nested := (doc.value("item").as_map()["missing"] or { toml.Any(map[string]toml.Any{}) }).as_map()["fd"] or { toml.Any(false) }
	assert !nested.bool()
	aot := toml.parse_text("[[signal]]\nname = \"A\"\n[[signal]]\nname = \"B\"\n") or { panic(err) }
	signals := aot.value("signal").array()
	assert signals.len == 2
	assert signals[0].as_map()["name"].string() == "A"
	assert signals[1].as_map()["name"].string() == "B"
	debug("disabled map interpolation: ${values}")
	println(values.len)
	assert int((values["value"] or { toml.Any(0) }).int()) == 42
	println(array_length(values["items"]))
	println(array_length(values["value"]))
	first := make_owner({"answer": 42})
	second := make_owner({"answer": 7})
	println(first.publish("answer"))
	println(second.publish("answer"))
	println(first.publish("absent"))
	hexes := fn (bytes []u8) string {
		return bytes.map("0x${it.hex()}").join(", ")
	}
	println(hexes([u8(16), 2]))
}
') or { panic(err) }
	// Directory inputs must keep target/backend filtering and exclude tests.
	os.write_file(os.join_path(root, 'bad.native.v'), 'invalid native source') or { panic(err) }
	os.write_file(os.join_path(root, 'bad_test.v'), 'invalid test source') or { panic(err) }
	program := os.join_path(root, 'program')
	compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-nocache', '-o', program,
		root])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output == '42\n2\n2\n-1\n42\n7\nmissing\n0x10, 0x02\n', run.output
}

// A program file named on its own is compiled alone, as by the C backend, so
// independent programs can share a directory (as in `examples/`).
fn test_fastc_real_builtin_file_entry_excludes_sibling_programs() {
	root := os.join_path(os.vtmp_dir(), 'fastc_lone_entry_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'first.v'), "import os\n\nfn main() {\n\tprintln('first \${os.args.len}')\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'second.v'), "fn main() {\n\tprintln('second')\n}\n") or {
		panic(err)
	}
	for name, expected in {
		'first':  'first 1\n'
		'second': 'second\n'
	} {
		program := os.join_path(root, name)
		compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-d', 'fastc_real_builtin', '-nocache',
			'-o', program, os.join_path(root, '${name}.v')])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output == expected, run.output
	}
}

fn test_fastc_directory_entry_filters_sources_and_reports_empty_directory() {
	root := os.join_path(os.vtmp_dir(), 'fastc_entry_dir_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	prefs := pref.new_preferences()
	if _ := fastc_expand_entry_paths([root], prefs) {
		assert false, 'empty directory unexpectedly accepted'
	} else {
		assert err.msg().contains('has no V source files for this target')
	}
	os.write_file(os.join_path(root, 'main.v'), 'fn main() { println(answer()) }') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'helper.v'), 'fn answer() int { return 42 }') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'bad.wasm.v'), 'invalid wasm source') or { panic(err) }
	os.write_file(os.join_path(root, 'bad_test.v'), 'invalid test source') or { panic(err) }
	result := generate_files_with_source_paths([root], prefs) or { panic(err) }
	assert result.source_paths.len == 2, result.source_paths.str()
	assert result.c_source().contains('answer(void)'), result.c_source()
}

fn test_fastc_shared_library_source_flags_keep_relative_paths() {
	for library in ['libexample.dylib', 'libexample.so', 'libexample.so.1', 'native.c'] {
		assert fastc_flag_is_skippable('./lib/${library}')
		args := fastc_c_flag_args('./lib/${library}', @VEXEROOT, '/tmp/project/source.v') or {
			panic(err)
		}
		assert args == ['/tmp/project/lib/${library}'], args.str()
	}
}
