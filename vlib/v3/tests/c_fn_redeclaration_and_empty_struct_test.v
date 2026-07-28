import os
import rand

const c_fn_redecl_v3_vexe = @VEXE
const c_fn_redecl_v3_tests_dir = os.dir(@FILE)
const c_fn_redecl_v3_dir = os.dir(c_fn_redecl_v3_tests_dir)
const c_fn_redecl_v3_vlib_dir = os.dir(c_fn_redecl_v3_dir)
const c_fn_redecl_v3_src = os.join_path(c_fn_redecl_v3_dir, 'v3.v')

fn build_c_fn_redecl_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_fn_redecl_${os.getpid()}_${rand.ulid()}')
	result :=
		os.execute('${os.quoted_path(c_fn_redecl_v3_vexe)} -gc none -path "${c_fn_redecl_v3_vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(c_fn_redecl_v3_src)}')
	assert result.exit_code == 0, result.output
	return v3_bin
}

fn write_c_fn_redecl_v3_project(name string, files map[string]string) !string {
	root := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}_${rand.ulid()}')
	os.mkdir_all(root)!
	for relative_path, source in files {
		path := os.join_path(root, relative_path)
		os.mkdir_all(os.dir(path))!
		os.write_file(path, source)!
	}
	return root
}

fn test_v3_c_fn_redeclarations_and_empty_struct_defaults() {
	v3_bin := build_c_fn_redecl_v3()
	root := write_c_fn_redecl_v3_project('c_fn_conflict', {
		'v.mod':       "Module { name: 'c_fn_conflict' }\n"
		'moda/moda.v': 'module moda\n\nfn C.getpid() int\nfn C.variadic_probe(value int, weight f32) int\n\npub fn pid() int {\n\treturn C.getpid()\n}\n'
		'modb/modb.v': 'module modb\n\nfn C.getpid() u64\nfn C.variadic_probe(value int, ...) int\n\npub fn pid() u64 {\n\treturn C.getpid()\n}\n'
		'main.v':      'module main\n\nimport moda\nimport modb\n\nfn main() {\n\tprintln(moda.pid())\n\tprintln(modb.pid())\n}\n'
	})!
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	conflict_out := os.join_path(os.temp_dir(),
		'v3_c_fn_conflict_out_${os.getpid()}_${rand.ulid()}')
	defer {
		os.rm(conflict_out) or {}
		os.rm(conflict_out + '.c') or {}
	}
	result :=
		os.execute('${os.quoted_path(v3_bin)} -no-memory-limit ${os.quoted_path(root)} -b c -o ${os.quoted_path(conflict_out)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('C function `C.getpid` was already declared with a different signature'), result.output
	assert result.output.contains('C function `C.variadic_probe` was already declared with a different signature'), result.output

	assert !result.output.contains('C compilation failed'), result.output

	compatible_root := write_c_fn_redecl_v3_project('c_fn_compatible', {
		'v.mod':       "Module { name: 'c_fn_compatible' }\n"
		'moda/moda.v': 'module moda\n\nstruct C.Display {}\nstruct C.Widget {\n\tvalue int\n}\n\nfn C.compat_probe(value int, data byteptr, display voidptr, mut state usize) int\nfn C.local_make() &C.Widget\n\npub fn touch() {\n\tmut state := usize(0)\n\tC.compat_probe(0, unsafe { nil }, unsafe { nil }, mut state)\n}\n\npub fn value() int {\n\treturn C.local_make().value\n}\n'
		'modb/modb.v': 'module modb\n\nstruct C.Display {}\nstruct C.Widget {\n\tvalue int\n}\n\ntype CInt = i32\n\nfn C.compat_probe(value CInt, data &u8, display &C.Display, state &usize) i32\nfn C.local_make() voidptr\n\npub fn touch() {\n\tmut state := usize(0)\n\tC.compat_probe(0, unsafe { nil }, unsafe { nil }, &state)\n}\n\npub fn make() voidptr {\n\treturn C.local_make()\n}\n'
		'main.v':      'module main\n\nimport moda as _\nimport modb as _\n\nfn main() {}\n'
	})!
	defer {
		os.rmdir_all(compatible_root) or {}
	}
	compatible_out := os.join_path(os.temp_dir(),
		'v3_c_fn_compatible_out_${os.getpid()}_${rand.ulid()}')
	defer {
		os.rm(compatible_out) or {}
		os.rm(compatible_out + '.c') or {}
	}
	compatible_result :=
		os.execute('${os.quoted_path(v3_bin)} -no-memory-limit ${os.quoted_path(compatible_root)} -b c -o ${os.quoted_path(compatible_out)}')
	assert compatible_result.exit_code == 0, compatible_result.output

	empty_root := write_c_fn_redecl_v3_project('empty_struct_default', {
		'main.v': 'struct Empty {}\n\n__global global_empty Empty\n\nfn make_empty() Empty {\n\treturn Empty{}\n}\n\nfn main() {\n\t_ := make_empty()\n\t_ = global_empty\n}\n'
	})!
	c_path := os.join_path(empty_root, 'main.c')
	defer {
		os.rmdir_all(empty_root) or {}
	}
	empty_result :=
		os.execute('${os.quoted_path(v3_bin)} -no-memory-limit -enable-globals ${os.quoted_path(empty_root)} -b c -o ${os.quoted_path(c_path)}')
	assert empty_result.exit_code == 0, empty_result.output
	generated := os.read_file(c_path)!
	assert !generated.contains('Empty){}'), generated
	assert generated.contains('Empty){0}'), generated
	global_initializers := generated.split_into_lines().filter(it.contains('global_empty = '))
	assert global_initializers.len == 1, generated
	global_initializer := global_initializers[0]
	assert global_initializer.contains('{0}'), global_initializer
}
