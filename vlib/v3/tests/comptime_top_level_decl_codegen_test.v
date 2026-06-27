import os

const comptime_decl_vexe = @VEXE
const comptime_decl_tests_dir = os.dir(@FILE)
const comptime_decl_v3_dir = os.dir(comptime_decl_tests_dir)
const comptime_decl_vlib_dir = os.dir(comptime_decl_v3_dir)
const comptime_decl_v3_src = os.join_path(comptime_decl_v3_dir, 'v3.v')

fn comptime_decl_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_comptime_top_level_decl_test')
	build :=
		os.execute('${comptime_decl_vexe} -gc none -path "${comptime_decl_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${comptime_decl_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn comptime_decl_count(c_code string, needle string) int {
	mut count := 0
	mut rest := c_code
	for {
		idx := rest.index(needle) or { break }
		count++
		rest = rest[idx + needle.len..]
	}
	return count
}

fn comptime_decl_struct_body(c_code string, name string) string {
	start := c_code.index('struct ${name} {') or { return '' }
	rest := c_code[start..]
	end := rest.index('\n};') or { return rest }
	return rest[..end]
}

fn comptime_decl_gen_c(v3_bin string, name string, feature bool) string {
	root := os.join_path(os.temp_dir(), 'v3_comptime_top_level_decl_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, "module main

$if some_feature ? {
	struct Choice {
		x int
	}
} $else {
	struct Choice {}
}

struct Holder {
	choice Choice
}

fn main() {
	$if some_feature ? {
		holder := Holder{
			choice: Choice{
				x: 1
			}
		}
		println(int_str(holder.choice.x))
	} $else {
		holder := Holder{
			choice: Choice{}
		}
		_ = holder
		println('dummy')
	}
}
") or {
		panic(err)
	}
	bin_path := os.join_path(root, 'out')
	feature_arg := if feature { '-d some_feature' } else { '' }
	compile := os.execute('${v3_bin} ${main_path} ${feature_arg} -b c -o ${bin_path}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	c_path := bin_path + '.c'
	assert os.exists(c_path), '${name}: missing generated C ${c_path}'
	return os.read_file(c_path) or { panic(err) }
}

fn comptime_decl_gen_c_source(v3_bin string, name string, source string, flags string) string {
	root := os.join_path(os.temp_dir(), 'v3_comptime_top_level_decl_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, source) or { panic(err) }
	c_path := os.join_path(root, 'out.c')
	compile := os.execute('${v3_bin} ${main_path} ${flags} -b c -o ${c_path}')
	assert compile.exit_code == 0, '${name}: C output failed: ${compile.output}'
	assert os.exists(c_path), '${name}: missing generated C ${c_path}'
	return os.read_file(c_path) or { panic(err) }
}

fn comptime_decl_compile_run_source(v3_bin string, name string, source string, flags string) os.Result {
	root := os.join_path(os.temp_dir(), 'v3_comptime_top_level_decl_run_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, source) or { panic(err) }
	bin_path := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${main_path} ${flags} -o ${bin_path}')
	assert compile.exit_code == 0, '${name}: compile failed: ${compile.output}'
	return os.execute(bin_path)
}

fn test_top_level_decls_inside_active_comptime_branch_are_codegen_visible() {
	v3_bin := comptime_decl_build_v3()
	else_c := comptime_decl_gen_c(v3_bin, 'else_branch', false)
	else_choice := comptime_decl_struct_body(else_c, 'Choice')
	assert comptime_decl_count(else_c, 'struct Choice {') == 1, else_c
	assert else_choice.contains('_dummy'), else_c
	assert !else_choice.contains('int x;'), else_c

	feature_c := comptime_decl_gen_c(v3_bin, 'feature_branch', true)
	feature_choice := comptime_decl_struct_body(feature_c, 'Choice')
	assert comptime_decl_count(feature_c, 'struct Choice {') == 1, feature_c
	assert feature_choice.contains('int x;'), feature_c
	assert !feature_choice.contains('_dummy'), feature_c
}

fn test_declaration_only_top_level_comptime_block_does_not_synthesize_main() {
	v3_bin := comptime_decl_build_v3()
	c_code := comptime_decl_gen_c_source(v3_bin, 'decl_only_no_main', 'module main

$if some_feature ? {
	fn helper() int {
		return 3
	}
} $else {
	struct Helper {}
}
',
		'-d some_feature')
	assert !c_code.contains('int main('), c_code
}

fn test_top_level_comptime_block_with_statement_still_synthesizes_main() {
	v3_bin := comptime_decl_build_v3()
	c_code := comptime_decl_gen_c_source(v3_bin, 'stmt_synth_main', "module main

$if some_feature ? {
	println('active')
} $else {
	fn helper() int {
		return 3
	}
}
",
		'-d some_feature')
	assert c_code.contains('int main('), c_code
	assert c_code.contains('active'), c_code
}

fn test_synthetic_top_level_block_keeps_main_scope_locals() {
	v3_bin := comptime_decl_build_v3()
	run := comptime_decl_compile_run_source(v3_bin, 'synthetic_block_main_scope', 'module main

$if some_feature ? {
	x := 1
	y := 2
}

fn helper() int {
	return 0
}

println(int_str(x + y))
',
		'-d some_feature')
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '3'
}
