import os

const mini_calc_vexe = @VEXE
const mini_calc_tests_dir = os.dir(@FILE)
const mini_calc_v3_dir = os.dir(mini_calc_tests_dir)
const mini_calc_vlib_dir = os.dir(mini_calc_v3_dir)
const mini_calc_v3_src = os.join_path(mini_calc_v3_dir, 'v3.v')
const mini_calc_repo_root = os.dir(mini_calc_vlib_dir)

fn mini_calc_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_mini_calc_markused_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${mini_calc_vexe} -gc none -path "${mini_calc_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${mini_calc_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn mini_calc_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn mini_calc_compile_run(v3_bin string, name string, files map[string]string, main_file string) (string, string) {
	root := os.join_path(os.temp_dir(), 'v3_mini_calc_markused_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	mini_calc_write_file(root, 'v.mod', "Module { name: 'mini_calc_markused' }\n")
	for rel, src in files {
		mini_calc_write_file(root, rel, src)
	}
	main_path := os.join_path(root, main_file)
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	return run.output.trim_space(), generated
}

fn mini_calc_compile_bad(v3_bin string, name string, files map[string]string, main_file string) string {
	root := os.join_path(os.temp_dir(), 'v3_mini_calc_markused_bad_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	mini_calc_write_file(root, 'v.mod', "Module { name: 'mini_calc_markused_bad' }\n")
	for rel, src in files {
		mini_calc_write_file(root, rel, src)
	}
	main_path := os.join_path(root, main_file)
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${bin}')
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	return compile.output
}

fn test_top_level_for_local_receiver_roots_recursive_chain_only() {
	v3_bin := mini_calc_build_v3()
	output, generated := mini_calc_compile_run(v3_bin, 'for_top_level_receiver', {
		'main.v': 'module main

struct Parser {}

fn (mut p Parser) expr() !int {
	return p.term()!
}

fn (mut p Parser) term() !int {
	return 8
}

fn (mut p Parser) unused() !int {
	return 99
}

for i := 0; i < 1; i++ {
	mut parser := Parser{}
	result := parser.expr() or { 0 }
	println(int_str(result))
}
'
	}, 'main.v')
	assert output == '8'
	assert generated.contains('Parser__expr('), generated
	assert generated.contains('Parser__term('), generated
	assert !generated.contains('Parser__unused('), generated
}

fn test_top_level_future_local_receiver_is_not_rooted() {
	v3_bin := mini_calc_build_v3()
	output := mini_calc_compile_bad(v3_bin, 'future_local_receiver', {
		'main.v': 'module main

struct Parser {}

fn (mut p Parser) expr() !int {
	return 7
}

result := parser.expr() or { 0 }
mut parser := Parser{}
println(int_str(result))
'
	}, 'main.v')
	assert output.contains('unknown function `parser.expr`') || output.contains('unknown__expr')
		|| output.contains('C compilation failed'), output
}

fn test_top_level_same_declaration_receiver_is_not_rooted() {
	v3_bin := mini_calc_build_v3()
	output := mini_calc_compile_bad(v3_bin, 'same_decl_receiver', {
		'main.v': 'module main

struct Parser {}

fn (mut p Parser) expr() !int {
	return 7
}

parser := parser.expr() or { 0 }
println(int_str(parser))
'
	}, 'main.v')
	assert output.contains('unknown function `parser.expr`') || output.contains('unknown__expr')
		|| output.contains('C compilation failed'), output
}

fn test_top_level_multi_decl_same_statement_receiver_is_not_rooted() {
	v3_bin := mini_calc_build_v3()
	output := mini_calc_compile_bad(v3_bin, 'multi_decl_same_statement_receiver', {
		'main.v': 'module main

struct Parser {}

fn (mut p Parser) expr() !int {
	return 7
}

parser, result := Parser{}, parser.expr() or { 0 }
_ := parser
println(int_str(result))
'
	}, 'main.v')
	assert output.contains('unknown function `parser.expr`') || output.contains('unknown__expr')
		|| output.contains('C compilation failed'), output
}

fn test_top_level_block_and_if_scope_do_not_leak_receiver_type() {
	v3_bin := mini_calc_build_v3()
	output, generated := mini_calc_compile_run(v3_bin, 'block_scope', {
		'main.v': 'module main

struct Parser {}

fn (mut p Parser) expr() !int {
	return 1
}

{
	mut parser := Parser{}
	_ := parser
}
if true {
	mut parser := Parser{}
	_ := parser
}
println("ok")
'
	}, 'main.v')
	assert output == 'ok'
	assert !generated.contains('Parser__expr('), generated
}

fn test_explicit_main_ignores_top_level_receiver_calls() {
	v3_bin := mini_calc_build_v3()
	output, generated := mini_calc_compile_run(v3_bin, 'explicit_main', {
		'main.v': 'module main

struct Parser {}

fn (mut p Parser) expr() !int {
	return p.term()!
}

fn (mut p Parser) term() !int {
	return 7
}

mut parser := Parser{}
_ := parser.expr() or { 0 }

fn main() {
	println("entry")
}
'
	}, 'main.v')
	assert output == 'entry'
	assert !generated.contains('Parser__expr('), generated
	assert !generated.contains('Parser__term('), generated
}

fn test_top_level_local_receiver_does_not_root_imported_homonym() {
	v3_bin := mini_calc_build_v3()
	output, generated := mini_calc_compile_run(v3_bin, 'imported_homonym', {
		'main.v':         'module main

import othermod

struct Parser {}

fn (mut p Parser) expr() !int {
	return 5
}

mut parser := Parser{}
println(int_str(parser.expr() or { 0 }))
println(int_str(othermod.touch()))
'
		'othermod/mod.v': 'module othermod

pub struct Parser {}

pub fn touch() int {
	return 2
}

pub fn (mut p Parser) expr() !int {
	return 100
}
'
	}, 'main.v')
	assert output == '5\n2'
	assert generated.contains('Parser__expr('), generated
	assert !generated.contains('othermod__Parser__expr('), generated
}

fn test_top_level_local_receiver_shadows_module_alias() {
	v3_bin := mini_calc_build_v3()
	output, generated := mini_calc_compile_run(v3_bin, 'local_receiver_alias_shadow', {
		'main.v':          'module main

import parsermod as parser

struct Parser {}

fn (mut p Parser) expr() !int {
	return 12
}

mut parser := Parser{}
println(int_str(parser.expr() or { 0 }))
'
		'parsermod/mod.v': 'module parsermod

pub fn expr() int {
	return 3
}
'
	}, 'main.v')
	assert output == '12'
	assert generated.contains('Parser__expr('), generated
	assert !generated.contains('parsermod__expr('), generated
}

fn test_top_level_import_alias_then_local_receiver_shadow_direct_calls() {
	v3_bin := mini_calc_build_v3()
	output, generated := mini_calc_compile_run(v3_bin, 'import_alias_then_local_receiver_shadow', {
		'main.v':          'module main

import parsermod as parser

struct Parser {}

fn (mut p Parser) expr() !int {
	return 99
}

println(int_str(parser.expr()))
mut parser := Parser{}
println(int_str(parser.expr() or { 0 }))
'
		'parsermod/mod.v': 'module parsermod

pub fn expr() int {
	return 17
}
'
	}, 'main.v')
	assert output == '17\n99'
	assert generated.contains('parsermod__expr('), generated
	assert generated.contains('Parser__expr('), generated
}

fn test_mini_calculator_recursive_descent_compiles_and_runs() {
	v3_bin := mini_calc_build_v3()
	example := os.join_path(mini_calc_repo_root, 'examples', 'mini_calculator_recursive_descent.v')
	bin := os.join_path(os.temp_dir(), 'v3_mini_calculator_recursive_descent_${os.getpid()}')
	compile := os.execute('${v3_bin} ${example} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute("printf '2 * (5-1)\\nexit\\n' | ${bin}")
	assert run.exit_code == 0, run.output
	assert run.output.contains('8'), run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('Parser__expr('), generated
	assert generated.contains('Parser__term('), generated
	assert generated.contains('Parser__factor('), generated
	assert generated.contains('Parser__number('), generated
}
