module driver

import os
import v.flat
import v.modulecache
import v.parser
import v.pref

// A build takes the functions of cached interfaces that nothing can name out of
// its AST. These tests parse an interface and a program that uses a part of it.

const cached_interface = 'module cachedmod

pub struct Thing {
pub:
	x int
}

pub fn used() int

@[inline]
pub fn unused() int

fn named_by_a_stage() int

@[export: "cachedmod_exported"]
pub fn exported() int

@[markused]
pub fn marked() int

pub fn init()

pub fn (t Thing) method() int

pub fn Thing.make() Thing

pub fn with_body[T](x T) T {
	return reached_from_a_body(x)
}

pub fn reached_from_a_body[T](x T) T

pub fn same_name_as_a_field() int

fn C.puts(s &char) int

fn C.never_called() int
'

const cached_interface_user = 'import cachedmod

struct Local {
	same_name_as_a_field int
}

fn main() {
	println(cachedmod.used())
	println(cachedmod.with_body(1))
	unsafe { C.puts(c"x") }
}
'

fn parse_cached_interface(name string) (&flat.FlatAst, string) {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	header := os.join_path(root, 'cachedmod.vh')
	program := os.join_path(root, 'main.v')
	os.write_file(header, cached_interface) or { panic(err) }
	os.write_file(program, cached_interface_user) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_files([header, program])
	a.cached_header_sources[header] = os.join_path(root, 'cachedmod.v')
	return a, root
}

fn function_kinds(a &flat.FlatAst, header string) map[string]flat.NodeKind {
	mut kinds := map[string]flat.NodeKind{}
	for node in a.nodes {
		if node.kind != .file || node.value != header {
			continue
		}
		for i in 0 .. node.children_count {
			child := a.child_node(&node, i)
			if child.value.len > 0 && child.kind in [.fn_decl, .c_fn_decl, .empty] {
				kinds[child.value] = child.kind
			}
		}
	}
	return kinds
}

fn test_functions_that_nothing_names_leave_the_ast() {
	mut a, root := parse_cached_interface('cached_declarations')
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'cachedmod.vh')
	before := function_kinds(a, header)
	assert before['unused'] == .fn_decl
	assert before['never_called'] == .c_fn_decl
	mut keep := ['named_by_a_stage']
	keep << module_lifecycle_function_names
	pruned := prune_unreferenced_cached_functions(mut a, keep)
	kinds := function_kinds(a, header)
	// What the program calls, what a kept body calls, what a stage names, what
	// the module roots, and every method stay.
	for name in ['used', 'named_by_a_stage', 'exported', 'marked', 'init', 'Thing.method',
		flat.encode_static_type_method_name('Thing', 'make'), 'with_body', 'reached_from_a_body'] {
		assert kinds[name] == .fn_decl, name
	}
	assert kinds['puts'] == .c_fn_decl
	// A function stays when anything spells its name, whatever that is.
	assert kinds['same_name_as_a_field'] == .fn_decl
	assert kinds['unused'] == .empty
	assert kinds['never_called'] == .empty
	assert pruned.count == 2
	assert pruned.names == ['unused']
	assert pruned.modules == ['cachedmod']
	assert pruned.short['unused'] && pruned.short['never_called']
	// The attributes of a function that left went with it.
	for node in a.nodes {
		if node.kind == .directive && node.value.starts_with('@attributes:') {
			declaration := a.nodes[node.value['@attributes:'.len..].int()]
			assert declaration.kind != .empty, declaration.value
		}
	}
}

fn test_functions_of_a_file_that_is_not_a_cached_interface_stay() {
	mut a, root := parse_cached_interface('cached_declarations_sources')
	defer {
		os.rmdir_all(root) or {}
	}
	a.cached_header_sources.clear()
	pruned := prune_unreferenced_cached_functions(mut a, []string{})
	assert pruned.count == 0
	assert function_kinds(a, os.join_path(root, 'cachedmod.vh'))['unused'] == .fn_decl
}

fn test_messages_that_spell_a_function_that_left_are_found() {
	mut pruned := V3PrunedDeclarations{
		count: 2
	}
	pruned.short['helper'] = true
	pruned.short['tos2'] = true
	assert v3_pruned_functions_named_in('unknown function: helper', &pruned) == ['helper']
	assert v3_pruned_functions_named_in("src.c:3: error: implicit declaration of function 'mymod__helper'",
		&pruned) == ['helper']
	assert v3_pruned_functions_named_in('call to undeclared function `tos2`; and helper too', &pruned) == [
		'tos2',
		'helper',
	]
	assert v3_pruned_functions_named_in('helpers and helper2 are other names', &pruned) == []
	assert v3_pruned_functions_named_in('helper', &V3PrunedDeclarations{}) == []
}

fn test_errors_of_a_source_are_told_from_errors_of_the_link() {
	assert v3_c_output_reports_source_error("src.c:3118: error: '{' expected (got ';')")
	assert v3_c_output_reports_source_error("main.c:3353:2: error: call to undeclared function 'tos2'")
	assert v3_c_output_reports_source_error("warning: x\n/tmp/a.c:1: error: implicit declaration of function 'f'")
	assert !v3_c_output_reports_source_error("tcc: error: unresolved reference to '_closure__closure_try_destroy'")
	assert !v3_c_output_reports_source_error('clang: error: linker command failed with exit code 1')
	assert !v3_c_output_reports_source_error('ld: library not found for -lfoo')
	assert !v3_c_output_reports_source_error('')
}

fn test_functions_that_a_build_missed_are_kept_by_the_next_one() {
	root := os.join_path(os.vtmp_dir(), 'v3_driver_kept_cached_functions_${os.getpid()}')
	os.rmdir_all(root) or {}
	saved := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', root, true)
	defer {
		if value := saved {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
		os.rmdir_all(root) or {}
	}
	manager := modulecache.new_manager(root, 'salt', true, '', '')
	assert v3_kept_cached_functions(&manager) == []
	v3_remember_kept_cached_functions(&manager, ['second', 'first'])
	v3_remember_kept_cached_functions(&manager, ['first', 'third', 'not a name'])
	assert v3_kept_cached_functions(&manager) == ['first', 'second', 'third']
}

// stage_literal_words returns the words that the stages of the compiler spell in
// their string literals.
fn stage_literal_words() map[string]bool {
	words, _ := stage_literal_words_and_names()
	return words
}

// stage_literal_words_and_names returns the words that the stages of the compiler
// spell in their string literals, and the names with dots among them, as
// `dl.interface_export_find`.
fn stage_literal_words_and_names() (map[string]bool, map[string]bool) {
	mut words := map[string]bool{}
	mut names := map[string]bool{}
	for dir in ['gen/c', 'transform', 'markused', 'types'] {
		stage_dir := os.join_path(@VMODROOT, 'vlib', 'v', dir)
		for file in os.ls(stage_dir) or { []string{} } {
			if !file.ends_with('.v') || file.ends_with('_test.v') {
				continue
			}
			source := os.read_file(os.join_path_single(stage_dir, file)) or { continue }
			mut i := 0
			for i < source.len {
				quote := source[i]
				i++
				if quote !in [`'`, `"`] {
					continue
				}
				mut word := []u8{}
				mut name := []u8{}
				for i < source.len && source[i] != quote && source[i] != `\n` {
					c := source[i]
					if c == `\\` {
						// An escape ends a word, and is none of its letters.
						i += 2
						if word.len > 0 {
							words[word.bytestr()] = true
							word.clear()
						}
						if name.len > 0 {
							names[name.bytestr()] = true
							name.clear()
						}
						continue
					}
					if c.is_letter() || c.is_digit() || c == `_` {
						word << c
					} else if word.len > 0 {
						words[word.bytestr()] = true
						word.clear()
					}
					if c.is_letter() || c.is_digit() || c in [`_`, `.`] {
						name << c
					} else if name.len > 0 {
						names[name.bytestr()] = true
						name.clear()
					}
					i++
				}
				if word.len > 0 {
					words[word.bytestr()] = true
				}
				if name.len > 0 {
					names[name.bytestr()] = true
				}
				i++
			}
		}
	}
	return words, names
}

const always_loaded_module_dirs = ['builtin', 'builtin/closure', 'strconv', 'strings', 'hash',
	'math/bits']

// plain_functions_of returns the plain functions that the sources in `module_dir`
// declare.
fn plain_functions_of(module_dir string) []string {
	mut names := map[string]bool{}
	for file in os.ls(module_dir) or { []string{} } {
		if !file.ends_with('.v') || file.ends_with('_test.v') {
			continue
		}
		for line in os.read_lines(os.join_path_single(module_dir, file)) or { []string{} } {
			mut rest := line
			if rest.starts_with('pub fn ') {
				rest = rest[7..]
			} else if rest.starts_with('fn ') {
				rest = rest[3..]
			} else {
				continue
			}
			mut end := 0
			for end < rest.len && (rest[end].is_letter() || rest[end].is_digit() || rest[end] == `_`) {
				end++
			}
			if end > 0 && end < rest.len && rest[end] in [`(`, `[`] && !rest[0].is_capital() {
				names[rest[..end]] = true
			}
		}
	}
	mut sorted := names.keys()
	sorted.sort()
	return sorted
}

// always_loaded_plain_functions returns the plain functions of the modules that
// every program links.
fn always_loaded_plain_functions() []string {
	mut names := map[string]bool{}
	for dir in always_loaded_module_dirs {
		for name in plain_functions_of(os.join_path(@VMODROOT, 'vlib', dir)) {
			names[name] = true
		}
	}
	mut sorted := names.keys()
	sorted.sort()
	return sorted
}

// module_functions_that_stages_name returns the plain functions of the other
// modules of vlib that a stage spells with the name of their module, as
// `module.function` or as the C name `module__function`.
fn module_functions_that_stages_name() []string {
	words, names := stage_literal_words_and_names()
	vlib := os.join_path(@VMODROOT, 'vlib')
	mut dirs := map[string]bool{}
	for file in os.walk_ext(vlib, '.v') {
		dirs[os.dir(file)] = true
	}
	mut found := map[string]bool{}
	for dir, _ in dirs {
		relative := dir[vlib.len + 1..]
		if relative in always_loaded_module_dirs || relative.contains('tests')
			|| relative.contains('testdata') {
			continue
		}
		module_name := relative.replace('/', '.')
		short_module := module_name.all_after_last('.')
		for name in plain_functions_of(dir) {
			if names['${short_module}.${name}'] || names['${module_name}.${name}']
				|| words['${short_module}__${name}']
				|| words['${module_name.replace('.', '__')}__${name}'] {
				found['${short_module}.${name}'] = true
			}
		}
	}
	mut sorted := found.keys()
	sorted.sort()
	return sorted
}

fn test_runtime_function_names_cover_what_the_stages_spell() {
	words := stage_literal_words()
	assert words.len > 1000
	assert words['gc_runtime_init'] && words['tos2']
	functions := always_loaded_plain_functions()
	assert functions.len > 300
	mut missing := []string{}
	for name in functions {
		if words[name] && name !in cached_runtime_function_names {
			missing << name
		}
	}
	// A stage that spells the name of a runtime function can generate a call of it
	// in a program that does not name it. Add the names to
	// cached_runtime_function_names in cached_declarations.v.
	assert missing == [], 'runtime functions that a stage names: ${missing}'
}

fn test_module_function_names_cover_what_the_stages_spell() {
	functions := module_functions_that_stages_name()
	assert 'dl.interface_export_find' in functions
	mut missing := []string{}
	for name in functions {
		if name !in cached_module_function_names {
			missing << name
		}
	}
	// A stage that spells the name of a function of a module can look for its
	// declaration, and do something else without a word where there is none. Add
	// the names to cached_module_function_names in cached_declarations.v.
	assert missing == [], 'functions of modules that a stage names: ${missing}'
}

fn test_a_program_that_lists_its_functions_keeps_every_declaration() {
	mut a, root := parse_cached_interface('with_reflection')
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'cachedmod.vh')
	before := function_kinds(a, header)
	assert before.len > 0
	a.nodes << flat.Node{
		kind:  .import_decl
		value: 'v.reflection'
	}
	pruned := prune_unreferenced_cached_functions(mut a, []string{})
	assert pruned.count == 0
	assert function_kinds(a, header) == before
}
