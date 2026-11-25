import os
import v.tempname

const mini_calc_repo_root = os.dir(os.dir(os.dir(os.dir(@FILE))))

fn mini_calc_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn mini_calc_compile_run(name string, files map[string]string, main_file string) (string, string) {
	root := os.join_path(os.vtmp_dir(), 'v3_mini_calc_markused_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mini_calc_write_file(root, 'v.mod', "Module { name: 'mini_calc_markused' }\n")
	for rel, src in files {
		mini_calc_write_file(root, rel, src)
	}
	main_path := os.join_path(root, main_file)
	bin := os.join_path(root, 'out')
	compile := os.exec([@VEXE, main_path, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	return run.output.trim_space(), generated
}

fn mini_calc_compile_bad(name string, files map[string]string, main_file string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_mini_calc_markused_bad_${name}_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mini_calc_write_file(root, 'v.mod', "Module { name: 'mini_calc_markused_bad' }\n")
	for rel, src in files {
		mini_calc_write_file(root, rel, src)
	}
	main_path := os.join_path(root, main_file)
	bin := os.join_path(root, 'out')
	compile := os.exec([@VEXE, main_path, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, '${name}: compile unexpectedly succeeded: ${compile.output}'
	return compile.output
}

fn test_top_level_for_local_receiver_roots_recursive_chain_only() {
	output, generated := mini_calc_compile_run('for_top_level_receiver', {
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
	output := mini_calc_compile_bad('future_local_receiver', {
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
	// The receiver is not declared yet, so the call names no `Parser` method.
	assert output.contains('undefined variable: `parser`'), output
}

fn test_top_level_same_declaration_receiver_is_not_rooted() {
	output := mini_calc_compile_bad('same_decl_receiver', {
		'main.v': 'module main

struct Parser {}

fn (mut p Parser) expr() !int {
	return 7
}

parser := parser.expr() or { 0 }
println(int_str(parser))
'
	}, 'main.v')
	// The receiver is not declared yet, so the call names no `Parser` method.
	assert output.contains('undefined variable: `parser`'), output
}

fn test_top_level_multi_decl_same_statement_receiver_is_not_rooted() {
	output := mini_calc_compile_bad('multi_decl_same_statement_receiver', {
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
	// The receiver is not declared yet, so the call names no `Parser` method.
	assert output.contains('undefined variable: `parser`'), output
}

fn test_top_level_block_and_if_scope_do_not_leak_receiver_type() {
	output, generated := mini_calc_compile_run('block_scope', {
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

fn test_top_level_local_receiver_does_not_root_imported_homonym() {
	output, generated := mini_calc_compile_run('imported_homonym', {
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

fn test_main_and_const_receivers_do_not_root_imported_homonyms() {
	output, generated := mini_calc_compile_run('imported_homonym_main', {
		'main.v':         'module main

import othermod

struct Parser {}

fn (p Parser) expr() int {
	return 5
}

fn (p Parser) term() int {
	return 6
}

const term = Parser{}.term()

fn main() {
	parser := Parser{}
	println(int_str(parser.expr()))
	println(int_str(term))
	println(int_str(othermod.touch()))
}
'
		'othermod/mod.v': 'module othermod

pub struct Parser {}

pub fn touch() int {
	return 2
}

pub fn (p Parser) expr() int {
	return 100
}

pub fn (p Parser) term() int {
	return 100
}
'
	}, 'main.v')
	assert output == '5\n6\n2'
	assert generated.contains('Parser__expr('), generated
	assert generated.contains('Parser__term('), generated
	assert !generated.contains('othermod__Parser__expr('), generated
	assert !generated.contains('othermod__Parser__term('), generated
}

fn test_top_level_local_receiver_cannot_shadow_module_alias() {
	output := mini_calc_compile_bad('local_receiver_alias_shadow', {
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
	assert output.contains('duplicate of an import symbol `parser`'), output
}

fn test_top_level_import_alias_cannot_be_shadowed_after_direct_calls() {
	output := mini_calc_compile_bad('import_alias_then_local_receiver_shadow', {
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
	assert output.contains('duplicate of an import symbol `parser`'), output
}

fn test_mini_calculator_recursive_descent_compiles_and_runs() {
	example := os.join_path(mini_calc_repo_root, 'examples', 'mini_calculator_recursive_descent.v')
	root := os.join_path(os.vtmp_dir(), 'v3_mini_calculator_recursive_descent_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, '${example}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec(['sh', '-c', 'printf \'2 * (5-1)\\nexit\\n\' | "\${1}"', 'v', '${bin}'])
	assert run.exit_code == 0, run.output
	assert run.output.contains('8'), run.output
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('Parser__expr('), generated
	assert generated.contains('Parser__term('), generated
	assert generated.contains('Parser__factor('), generated
	assert generated.contains('Parser__number('), generated
}
