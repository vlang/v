import os
import v.builder
import v.gen.c
import v.pref

const test_vexe = os.quoted_path(@VEXE)

fn test_prealloc_tls_global_has_cplusplus_thread_local_branch() {
	$if windows {
		return
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_cgen_prealloc_cplusplus_${os.getpid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	source_path := os.join_path(workdir, 'main.v')
	generated_c_path := os.join_path(workdir, 'main.c')
	os.write_file(source_path, 'fn main() {}\n')!

	emit_cmd := '${test_vexe} -message-limit 199 -gc none -prealloc -o ${os.quoted_path(generated_c_path)} ${os.quoted_path(source_path)}'
	emit_result := os.execute(emit_cmd)
	assert emit_result.exit_code == 0, '${emit_cmd}\n${emit_result.output}'
	generated := os.read_file(generated_c_path)!
	assert generated.contains('#elif defined(__cplusplus)\nthread_local VMemoryBlock* g_memory_block; // global 6'), generated

	assert generated.contains('#else\n_Thread_local VMemoryBlock* g_memory_block; // global 6'), generated
}

fn test_parallel_cc_prealloc_tls_extern_has_cplusplus_thread_local_branch() {
	workdir := os.join_path(os.vtmp_dir(), 'v_cgen_parallel_prealloc_cplusplus_${os.getpid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	source_path := os.join_path(workdir, 'main.v')
	os.write_file(source_path, 'fn main() {}\n')!
	mut prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-parallel-cc',
		'-gc',
		'none',
		'-prealloc',
		source_path,
	], false)
	mut b := builder.new_builder(prefs)
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	b.front_and_middle_stages(files)!
	result := c.gen(b.parsed_files, mut b.table, b.pref)
	externs := result.extern_str.replace('\r\n', '\n')
	assert externs.contains('#elif defined(__cplusplus)\nextern thread_local VMemoryBlock* g_memory_block;'), externs

	assert externs.contains('#else\nextern _Thread_local VMemoryBlock* g_memory_block;'), externs
}

fn test_is_o_generated_c_files_can_be_linked_together() {
	cc_path := os.find_abs_path_of_executable('cc') or { return }
	cc := os.quoted_path(cc_path)
	workdir := os.join_path(os.vtmp_dir(), 'v_cgen_is_o_link_${os.getpid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	a_v := os.join_path(workdir, 'a.v')
	b_v := os.join_path(workdir, 'b.v')
	host_c := os.join_path(workdir, 'main.c')
	a_c := os.join_path(workdir, 'a.c')
	b_c := os.join_path(workdir, 'b.c')
	prog := os.join_path(workdir, 'prog')
	os.write_file(a_v, 'module main

@[markused]
pub fn a() int {
	return 1
}
')!
	os.write_file(b_v, 'module main

@[markused]
pub fn b() int {
	return 2
}
')!
	os.write_file(host_c, '#include <stdio.h>
int main__a(void);
int main__b(void);

int main(void) {
	printf("%d %d\\n", main__a(), main__b());
	return 0;
}
')!
	// Each preallocated object must keep builtin globals object-local, including
	// the thread-local g_memory_block arena root.
	for cmd in [
		'${test_vexe} -message-limit 199 -cc ${cc} -gc none -prealloc -no-skip-unused -is_o -o ${os.quoted_path(a_c)} ${os.quoted_path(a_v)}',
		'${test_vexe} -message-limit 199 -cc ${cc} -gc none -prealloc -no-skip-unused -is_o -o ${os.quoted_path(b_c)} ${os.quoted_path(b_v)}',
		'${cc} -o ${os.quoted_path(prog)} ${os.quoted_path(a_c)} ${os.quoted_path(b_c)} ${os.quoted_path(host_c)} -lm',
	] {
		res := os.execute(cmd)
		assert res.exit_code == 0, '${cmd}\n${res.output}'
	}
	res := os.execute(os.quoted_path(prog))
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '1 2'
}

fn test_parallel_cc_windows_header_keeps_vv_loc_external() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'parallel_cc_windows_linkage_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(source_path, 'module main
fn helper() string {
	return "ok"
}

fn main() {
	println(helper())
}
')!
	mut prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-parallel-cc',
		'-os',
		'windows',
		source_path,
	], false)
	mut b := builder.new_builder(prefs)
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	b.front_and_middle_stages(files)!
	result := c.gen(b.parsed_files, mut b.table, b.pref)
	header := result.header.replace('\r\n', '\n')
	assert header.contains('#define _VPARALLELCC (1)'), header
	assert header.contains('#ifdef _VPARALLELCC\n\t\t#define VV_LOC\n\t#else\n\t\t#define VV_LOC static\n\t#endif'), header
	assert header.contains('VV_LOC string main__helper(void);'), header
}

fn test_parallel_cc_usecache_interface_index_definition_stays_out_of_header() {
	$if windows {
		return
	}
	tmp_dir := os.join_path(os.vtmp_dir(), 'parallel_cc_usecache_interface_index_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(source_path, "fn main() {\n\tprintln('hello world')\n}\n")!
	mut prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-usecache',
		'-parallel-cc',
		source_path,
	], false)
	mut b := builder.new_builder(prefs)
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	b.front_and_middle_stages(files)!
	result := c.gen(b.parsed_files, mut b.table, b.pref)
	header := result.header.replace('\r\n', '\n')
	out0 := result.out0_str.replace('\r\n', '\n')
	assert header.contains('enum { _IError_None___index_enum ='), header
	assert header.contains('extern const u32 _IError_None___index;'), header
	assert !header.contains('const u32 _IError_None___index = _IError_None___index_enum;')
	assert out0.contains('const u32 _IError_None___index = _IError_None___index_enum;'), out0
}

fn test_parallel_cgen_builtin_method_prefix_uses_receiver_type_index() {
	tmp_dir := os.join_path(os.vtmp_dir(), 'parallel_cgen_builtin_receiver_${os.getpid()}')
	os.mkdir_all(tmp_dir)!
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, 'main.v')
	os.write_file(source_path,
		'fn main() {\n\tmut values := []int{}\n\tunsafe { values.free() }\n}\n')!
	mut prefs, _ := pref.parse_args_and_show_errors([], [
		'',
		'-parallel-cc',
		source_path,
	], false)
	mut b := builder.new_builder(prefs)
	mut files := b.get_builtin_files()
	files << b.get_user_files()
	b.set_module_lookup_paths()
	b.front_and_middle_stages(files)!
	result := c.gen(b.parsed_files, mut b.table, b.pref)
	generated := result.res_builder.bytestr()
	assert generated.contains('builtin__array_free(&values);'), generated
	assert !generated.contains('\n\tarray_free(&values);'), generated
}
