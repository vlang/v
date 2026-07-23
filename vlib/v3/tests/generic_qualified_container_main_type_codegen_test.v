import os

const gqc_vexe = @VEXE
const gqc_tests_dir = os.dir(@FILE)
const gqc_v3_dir = os.dir(gqc_tests_dir)
const gqc_vlib_dir = os.dir(gqc_v3_dir)
const gqc_v3_src = os.join_path(gqc_v3_dir, 'v3.v')

fn gqc_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_generic_qualified_container_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${gqc_vexe} -gc none -path "${gqc_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${gqc_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn gqc_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

fn gqc_run_project(v3_bin string, name string, files map[string]string) string {
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_${name}_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: '${name}' }\n") or { panic(err) }
	for rel, source in files {
		gqc_write_file(root, rel, source)
	}
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A generic function in an imported module builds a value whose type nests a container with BOTH a
// qualified component and a bare generic parameter — `map[other.Key]T` inside `other.Box[...]`.
// When specialized from `main` with a program type (`Context`) that collides by short name with a
// same-named type in the callee module, the container's bare value must be pinned to the program
// type (`main.Context`). The `.` in the qualified key (`other.Key`) must not stop the collision
// decomposition of the bare value: otherwise the map is built with the callee module's homonym
// `Context` (wrong element size/fields) and the C compiler rejects storing the program value.
fn test_qualified_map_container_locks_nested_main_value() {
	v3_bin := gqc_build_v3()
	out := gqc_run_project(v3_bin, 'generic_qualified_map_main_type', {
		'other/other.v': 'module other\n\npub type Key = string\n\npub struct Box[T] {\npub mut:\n\tv T\n}\n'
		'mid/mid.v':     'module mid\n\nimport other\n\npub struct Context {\npub:\n\tid int\n}\n\npub fn make_box[T](k other.Key, val T) T {\n\tmut b := other.Box[map[other.Key]T]{}\n\tb.v[k] = val\n\treturn b.v[k]\n}\n'
		'main.v':        "module main\n\nimport mid\nimport other\n\nstruct Context {\npub:\n\tname string\n}\n\nfn main() {\n\tk := other.Key('a')\n\tgot := mid.make_box[Context](k, Context{\n\t\tname: 'hello'\n\t})\n\tprintln('got: ' + got.name)\n}\n"
	})
	// The map value is the program `Context` (its `.name` field), not the callee module's homonym.
	assert out == 'got: hello'
}
