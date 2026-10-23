module driver

import os
import v.flat
import v.modulecache
import v.parser
import v.pref
import v.scanner
import v.token

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
	pruned := prune_unreferenced_cached_functions(mut a, keep, []string{}, []string{})
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

fn test_functions_of_a_module_that_a_stage_names_stay_in_that_module() {
	mut a, root := parse_cached_interface('kept_in_module')
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'cachedmod.vh')
	pruned := prune_unreferenced_cached_functions(mut a, module_lifecycle_function_names,
		['cachedmod.unused', 'othermod.never_called'], []string{})
	kinds := function_kinds(a, header)
	assert kinds['unused'] == .fn_decl
	// A function of that name in another module is another function.
	assert kinds['never_called'] == .empty
	assert kinds['named_by_a_stage'] == .empty
	assert pruned.count == 2
}

fn test_functions_of_a_file_that_is_not_a_cached_interface_stay() {
	mut a, root := parse_cached_interface('cached_declarations_sources')
	defer {
		os.rmdir_all(root) or {}
	}
	a.cached_header_sources.clear()
	pruned := prune_unreferenced_cached_functions(mut a, []string{}, []string{}, []string{})
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

// Keep offsets and literals intact while excluding quotes in comments and runes.
fn stage_source_for_literals(source string) string {
	mut files := token.FileSet.new()
	file := files.add_file('stage.v', source.len)
	mut lex := scanner.new_scanner(pref.new_preferences(), .scan_comments | .skip_interpolation)
	lex.init(file, source)
	mut clean := source.bytes()
	for {
		kind := lex.scan()
		if kind == .eof {
			break
		}
		if kind in [.comment, .char] {
			for i in lex.pos .. lex.offset {
				if clean[i] != `\n` {
					clean[i] = ` `
				}
			}
		}
	}
	return clean.bytestr()
}

fn test_stage_source_for_literals_excludes_comments_and_runes() {
	source := [
		"// It's 'arrays.sum'\n",
		"/* 'outer' /* 'nested' */ 'comment' */\n",
		"const apostrophe = `'`\n",
		'const quote = `"`\n',
		"const kept = 'https://example.com/*literal*/'\n",
		'const after = "kept.name"\n',
	].join('')
	clean := stage_source_for_literals(source)
	assert clean.len == source.len
	assert clean.count('\n') == source.count('\n')
	assert !clean.contains('arrays.sum')
	assert !clean.contains('outer') && !clean.contains('nested') && !clean.contains('comment')
	assert !clean.contains("`'`") && !clean.contains('`"`')
	assert clean.contains("'https://example.com/*literal*/'")
	assert clean.contains('"kept.name"')
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
			source := stage_source_for_literals(os.read_file(os.join_path_single(stage_dir,
				file)) or { continue })
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
// declare. A test file, as `foo_test.v` or `foo_test.c.v`, is no part of its
// module that a program links.
fn plain_functions_of(module_dir string) []string {
	mut names := map[string]bool{}
	for file in os.ls(module_dir) or { []string{} } {
		if !file.ends_with('.v') || file.all_before('.').ends_with('_test') {
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

// runtime_modules_of_the_compiler are the modules below `vlib/v` whose functions
// a program can call when it runs. The others are the compiler: a stage does not
// write a call of one of their functions into a program.
const runtime_modules_of_the_compiler = ['v/reflection', 'v/debug', 'v/embed_file', 'v/live',
	'v/profile', 'v/trace_calls']

// V3LibraryFunctions are the plain functions that one directory of vlib declares.
struct V3LibraryFunctions {
	// The directory below vlib, and the name of its module with dots.
	dir         string
	module_name string
	names       []string
}

// library_plain_functions returns the plain functions of every module of vlib
// that a program can link, a directory at a time.
fn library_plain_functions() []V3LibraryFunctions {
	vlib := os.join_path(@VMODROOT, 'vlib')
	mut dirs := map[string]bool{}
	for file in os.walk_ext(vlib, '.v') {
		dirs[os.dir(file)] = true
	}
	mut modules := []V3LibraryFunctions{}
	for dir, _ in dirs {
		relative := dir[vlib.len + 1..]
		if relative.contains('tests') || relative.contains('testdata')
			|| (relative == 'v' || relative.starts_with('v/'))
				&& !runtime_modules_of_the_compiler.any(relative == it || relative.starts_with(it + '/')) {
			continue
		}
		modules << V3LibraryFunctions{
			dir:         relative
			module_name: relative.replace('/', '.')
			names:       plain_functions_of(dir)
		}
	}
	return modules
}

// module_functions_that_stages_name returns the plain functions of the other
// modules of vlib that a stage spells with the name of their module, as
// `module.function` or as the C name `module__function`.
fn module_functions_that_stages_name() []string {
	words, names := stage_literal_words_and_names()
	mut found := map[string]bool{}
	for library in library_plain_functions() {
		if library.dir in always_loaded_module_dirs {
			continue
		}
		module_name := library.module_name
		short_module := module_name.all_after_last('.')
		for name in library.names {
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
	// A name that no stage spells any more keeps a declaration for nothing.
	stale := cached_module_function_names.filter(it !in functions)
	assert stale == [], 'functions of modules that no stage names: ${stale}'
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
	pruned := prune_unreferenced_cached_functions(mut a, []string{}, []string{}, []string{})
	assert pruned.count == 0
	assert function_kinds(a, header) == before
}

// built_name_is_specific reports whether a name that a stage builds has a part of
// its own to tell it by: after the name of a module, which only says where a name
// that the stage got elsewhere belongs, three letters or digits or more.
fn built_name_is_specific(pattern string) bool {
	mut name := pattern
	for {
		dot := name.index('.') or { -1 }
		underscores := name.index('__') or { -1 }
		mut cut := dot
		mut width := 1
		if underscores >= 0 && (dot < 0 || underscores < dot) {
			cut = underscores
			width = 2
		}
		if cut <= 0 || name[..cut].contains('*') {
			break
		}
		name = name[cut + width..]
	}
	mut weight := 0
	for c in name {
		if c.is_letter() || c.is_digit() {
			weight++
		}
	}
	return weight >= 3
}

// stage_built_name_patterns returns the names that the stages of the compiler put
// together when they run, as far as a string literal shows it: a literal that is a
// name with interpolations in it, or a name that is joined to something with `+`.
// `*` stands for what is filled in.
fn stage_built_name_patterns() []string {
	mut patterns := map[string]bool{}
	for dir in ['gen/c', 'transform', 'markused', 'types'] {
		stage_dir := os.join_path(@VMODROOT, 'vlib', 'v', dir)
		for file in os.ls(stage_dir) or { []string{} } {
			if !file.ends_with('.v') || file.ends_with('_test.v') {
				continue
			}
			source := stage_source_for_literals(os.read_file(os.join_path_single(stage_dir,
				file)) or { continue })
			mut i := 0
			for i < source.len {
				quote := source[i]
				if quote !in [`'`, `"`] {
					i++
					continue
				}
				start := i
				i++
				mut pattern := []u8{}
				mut is_name := true
				mut interpolated := false
				for i < source.len && source[i] != quote && source[i] != `\n` {
					c := source[i]
					if c == `\\` {
						is_name = false
						i += 2
						continue
					}
					if c == `$` && i + 1 < source.len && source[i + 1] == `{` {
						mut depth := 1
						i += 2
						for i < source.len && depth > 0 && source[i] != `\n` {
							if source[i] == `{` {
								depth++
							} else if source[i] == `}` {
								depth--
							}
							i++
						}
						interpolated = true
						if pattern.len == 0 || pattern.last() != `*` {
							pattern << `*`
						}
						continue
					}
					if c.is_letter() || c.is_digit() || c in [`_`, `.`] {
						pattern << c
					} else {
						is_name = false
					}
					i++
				}
				end := i
				i++
				if !is_name || pattern.len == 0 {
					continue
				}
				mut text := pattern.bytestr()
				if !interpolated {
					mut before := start - 1
					for before >= 0 && source[before] in [` `, `\t`, `\n`] {
						before--
					}
					mut after := end + 1
					for after < source.len && source[after] in [` `, `\t`, `\n`] {
						after++
					}
					joined_before := before >= 0 && source[before] == `+`
					joined_after := after + 1 < source.len && source[after] == `+`
						&& source[after + 1] != `=`
					if !joined_before && !joined_after {
						continue
					}
					if joined_before {
						text = '*' + text
					}
					if joined_after {
						text += '*'
					}
				}
				if built_name_is_specific(text) {
					patterns[text] = true
				}
			}
		}
	}
	mut sorted := patterns.keys()
	sorted.sort()
	return sorted
}

// stage_built_function_name_patterns returns the names that the stages put
// together and that a plain function of a library module has: the others are
// names of something else.
fn stage_built_function_name_patterns() []string {
	libraries := library_plain_functions()
	mut fitting := []string{}
	for text in stage_built_name_patterns() {
		pattern := v3_name_pattern(text)
		for library in libraries {
			module_name := library.module_name.all_after_last('.')
			if library.names.any(pattern.fits(module_name, it)) {
				fitting << text
				break
			}
		}
	}
	return fitting
}

fn test_function_name_patterns_cover_what_the_stages_build() {
	built := stage_built_name_patterns()
	assert built.len > 50
	// `'${name}_str'`, `'map_hash_int_${size}'`: names with a part of their own.
	assert '*_str' in built
	assert 'map_hash_int_*' in built
	// `'builtin.${name}'` only says which module a name belongs to.
	assert 'builtin.*' !in built
	assert built_name_is_specific('*_free') && built_name_is_specific('overflow.*_checked')
	assert !built_name_is_specific('*.*') && !built_name_is_specific('builtin__*')
	assert !built_name_is_specific('math.bits.*') && !built_name_is_specific('*_*')
	patterns := stage_built_function_name_patterns()
	assert patterns.len > 10 && patterns.len < built.len
	assert '*_str' in patterns && 'map_hash_int_*' in patterns
	mut missing := []string{}
	for pattern in patterns {
		if pattern !in cached_function_name_patterns {
			missing << pattern
		}
	}
	// A stage that puts the name of a function together can look for its
	// declaration, and do something else without a word where there is none. Add
	// the names to cached_function_name_patterns in cached_declarations.v.
	assert missing == [], 'names of functions that a stage builds: ${missing}'
	// A pattern that fits no function of a library keeps nothing, and is tried
	// against every function all the same.
	stale := cached_function_name_patterns.filter(it !in patterns)
	assert stale == [], 'names that no stage builds of a function of vlib: ${stale}'
}

fn test_name_patterns_fit_the_names_that_they_stand_for() {
	patterns := ['*_str', 'map_hash_int_*', 'new_*_noscan', '*_key_*', 'overflow.*_i8', '*.free',
		'sync__*_st', 'exact_name*', 'builtin__overflow__*_u8', '__new_*', '*__clone'].map(v3_name_pattern(it))
	fits := fn [patterns] (module_name string, name string) bool {
		return v3_name_fits_a_pattern(patterns, module_name, name)
	}
	for function in [
		['builtin', 'ptr_str'],
		['strconv', 'f64_to_str'],
		['hash', 'map_hash_int_8'],
		['arrays', 'new_array_noscan'],
		['maps', 'drop_owned_key_value'],
		['overflow', 'add_i8'],
		['os', 'free'],
		['sync', 'new_channel_st'],
		['mymod', 'exact_name'],
		['overflow', 'sub_u8'],
		['builtin', '__new_array'],
		['anything', 'clone'],
	] {
		assert fits(function[0], function[1]), function.str()
	}
	for function in [
		['builtin', 'str_ptr'],
		['hash', 'map_hash_int'],
		['arrays', 'new_noscan'],
		['maps', 'key_value'],
		['other', 'add_i8'],
		['os', 'freed'],
		['sync', 'channel_st_new'],
		['mymod', 'not_exact_name'],
		['overflow', 'i8'],
		['builtin', 'sub_u8'],
		['builtin', 'new_array'],
		['anything', 'cloned'],
	] {
		assert !fits(function[0], function[1]), function.str()
	}
	// The module of a pattern is the last part of its name, or any.
	assert v3_name_pattern('builtin.overflow.add_*').module_name == 'overflow'
	assert v3_name_pattern('*.len').module_name == ''
	assert v3_name_pattern('*_str').module_name == ''
	assert v3_name_pattern('__new_array*').module_name == ''
	// The name of a function can start with `__` after the name of its module.
	for text in ['builtin.__new_*', 'builtin____new_*', 'builtin__overflow____new_*'] {
		pattern := v3_name_pattern(text)
		assert pattern.module_name in ['builtin', 'overflow'], text
		assert pattern.parts == ['__new_'], text
		assert pattern.fits(pattern.module_name, '__new_array'), text
		assert !pattern.fits(pattern.module_name, 'new_array'), text
	}
	// A pattern of one part with nothing to fill in is the whole name.
	assert v3_name_pattern('abc').fits('m', 'abc')
	assert !v3_name_pattern('abc').fits('m', 'abcd')
	assert v3_name_pattern('abc*').fits('m', 'abcdef')
	assert !v3_name_pattern('abc*').fits('m', 'xabc')
	// Parts in a row do not share letters of the name.
	assert v3_name_pattern('ab*bc').fits('m', 'abbc')
	assert !v3_name_pattern('ab*bc').fits('m', 'abc')
}

fn test_functions_that_a_built_name_stands_for_stay() {
	mut a, root := parse_cached_interface('kept_by_pattern')
	defer {
		os.rmdir_all(root) or {}
	}
	header := os.join_path(root, 'cachedmod.vh')
	pruned := prune_unreferenced_cached_functions(mut a, module_lifecycle_function_names,
		[]string{}, ['un*ed', 'cachedmod.named_by_*', 'other_*', 'othermod.never_*'])
	kinds := function_kinds(a, header)
	assert kinds['unused'] == .fn_decl
	assert kinds['named_by_a_stage'] == .fn_decl
	assert kinds['never_called'] == .empty
	assert pruned.count == 1
}
