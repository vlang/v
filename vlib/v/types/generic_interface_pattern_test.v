module types

import os
import v.flat

fn test_generic_interface_pattern_names_keep_the_declared_base_and_arguments() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'animation'
	tc.cur_file = 'animation.v'
	tc.structs[flat.source_type_id('animation.Animator')] = []StructField{}
	tc.struct_generic_params[flat.source_type_id('animation.Animator')] = generic_param_ids(['T'])
	// An identically spelled program type cannot replace the local declaration.
	tc.structs[flat.source_type_id('Animator')] = []StructField{}
	for arg in ['f32', 'f64', '[]f32', '[2]f32', 'map[string]f32', 'fn (f32) f64', 'fn (...string) int',
		'fn (int, ...string) int', 'fn (mut []int) !', 'fn ()', 'fn () !', 'fn () ?',
		'fn (fn (...string) !) ?', 'fn () (int)', 'fn () (int, string)', 'fn () !(int, string)',
		'fn () ?(int, string)', 'fn (fn () (int, string)) (bool, Animator[f32])',
		'fn () (map[string]int, fn () (int, string))', 'Animator[f32]'] {
		pattern := tc.resolve_interface_match_pattern(flat.source_type_id('Animator[${arg}]')) or {
			panic('unresolved pattern Animator[${arg}]')
		}
		assert flat.source_type_syntax(pattern).application_base() == flat.source_type_id('animation.Animator')
		assert tc.parse_type(pattern).source_type_id() == pattern
	}
}

fn test_generic_interface_void_callbacks_use_canonical_result_types() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'animation'
	tc.cur_file = 'animation.v'
	tc.structs[flat.source_type_id('animation.Animator')] = []StructField{}
	tc.struct_generic_params[flat.source_type_id('animation.Animator')] = generic_param_ids(['T'])
	for marker in ['!', '?'] {
		callback := flat.source_type_id('fn () ${marker}')
		canonical_callback := tc.parse_type(callback).source_type_id()
		pattern := tc.resolve_interface_match_pattern(flat.source_type_id('Animator[fn () ${marker}]')) or {
			panic('unresolved void callback pattern')
		}
		assert flat.source_type_syntax(pattern).application_args() == [canonical_callback]
		assert tc.qualify_type_text(callback) == canonical_callback
	}
}

fn test_generic_interface_callback_patterns_reject_invalid_multi_return_components() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'animation'
	tc.cur_file = 'animation.v'
	tc.structs[flat.source_type_id('animation.Animator')] = []StructField{}
	tc.struct_generic_params[flat.source_type_id('animation.Animator')] = generic_param_ids(['T'])
	for result in ['()', '(int,)', '(,string)', '(int,,string)', '(int, Missing)',
		'(map[string]Missing, string)', '(int, fn () (Missing, string))'] {
		pattern := flat.source_type_id('Animator[fn () ${result}]')
		if resolved := tc.resolve_interface_match_pattern(pattern) {
			assert false, 'invalid callback pattern resolved as ${flat.source_type_name(resolved)}'
		}
	}
}

fn test_generic_interface_callback_patterns_reject_invalid_parameters_and_empty_arguments() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'animation'
	tc.cur_file = 'animation.v'
	tc.structs[flat.source_type_id('animation.Animator')] = []StructField{}
	tc.struct_generic_params[flat.source_type_id('animation.Animator')] = generic_param_ids(['T'])
	for arg in ['', '!', '?', 'fn (...Missing) int', 'fn (int, ...Missing) int',
		'fn (...Animator[Missing]) !', 'fn (fn (...Missing) !) ?', 'fn (mut Missing) !',
		'fn () !voidMissing', 'fn () ?voidMissing'] {
		pattern := flat.source_type_id('Animator[${arg}]')
		if resolved := tc.resolve_interface_match_pattern(pattern) {
			assert false, 'invalid callback pattern resolved as ${flat.source_type_name(resolved)}'
		}
	}
}

fn test_generic_interface_pointer_patterns_preserve_wrappers() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.cur_module = 'animation'
	tc.cur_file = 'animation.v'
	tc.structs[flat.source_type_id('animation.Animator')] = []StructField{}
	tc.struct_generic_params[flat.source_type_id('animation.Animator')] = generic_param_ids(['T'])
	tc.structs[flat.source_type_id('animation.Plain')] = []StructField{}
	for prefix in ['&', '&&'] {
		for name in ['Animator', 'animation.Animator'] {
			pattern := tc.resolve_interface_match_pattern(flat.source_type_id('${prefix}${name}[f32]')) or {
				panic('unresolved pattern ${prefix}${name}[f32]')
			}
			assert pattern == flat.source_type_id('${prefix}animation.Animator[f32]')
		}
		plain := tc.resolve_interface_match_pattern(flat.source_type_id('${prefix}Plain')) or {
			panic('unresolved pattern ${prefix}Plain')
		}
		assert plain == flat.source_type_id('${prefix}animation.Plain')
	}
}

const generic_interface_pattern_source = 'module animation

pub interface Motion {
mut:
	running bool
}

pub struct Animator[T] {
pub mut:
	running bool
	value T
}

pub struct Plain {
pub mut:
	running bool
}

pub struct Wrong[T] {
pub mut:
	value T
}

struct Pool {
mut:
	active []&Motion
	singles []&Animator[f32]
	doubles []&Animator[f64]
}

fn (mut p Pool) recycle() {
	for i := p.active.len - 1; i >= 0; i-- {
		mut animator := p.active[i]
		if mut animator is Animator[f32] {
			animator.value += f32(1)
			p.singles << animator
			p.active.delete(i)
		} else if mut animator is Animator[f64] {
			animator.value += f64(2)
			p.doubles << animator
			p.active.delete(i)
		}
	}
}

pub fn verify_pool() {
	mut first := &Animator[f32]{value: 1.25}
	mut second := &Animator[f64]{value: 2.5}
	mut p := Pool{}
	p.active << first
	p.active << second
	p.recycle()
	assert p.active.len == 0
	assert p.singles.len == 1
	assert p.doubles.len == 1
	assert p.singles[0] == first
	assert p.doubles[0] == second
	assert first.value == f32(2.25)
	assert second.value == f64(4.5)
	p.singles[0].value = 9
	p.doubles[0].value = 10
	assert first.value == f32(9)
	assert second.value == f64(10)
}
'

fn generic_interface_pattern_fixture(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v_generic_interface_pattern_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'animation')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'generic_interface_pattern' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'animation', 'animation.v'), generic_interface_pattern_source) or {
		panic(err)
	}
	return root
}

fn test_imported_generic_interface_patterns_preserve_pointer_identity() {
	verify_imported_generic_interface_pattern_pointer_identity('')
	verify_imported_generic_interface_pattern_pointer_identity('&')
}

fn verify_imported_generic_interface_pattern_pointer_identity(pointer_prefix string) {
	root := generic_interface_pattern_fixture('runtime_${pointer_prefix.len}')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'animation', 'animation.v'), generic_interface_pattern_source.replace('is Animator[',
		'is ${pointer_prefix}Animator[')) or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'import animation
fn main() { animation.verify_pool() }
') or { panic(err) }
	mut compilers := [[]string{}]
	if _ := os.find_abs_path_of_executable('clang') {
		compilers << ['-cc', 'clang']
	}
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	for flags in compilers {
		mut args := [@VEXE, '-new-compiler', '-no-retry-compilation']
		args << flags
		args << ['-o', binary, root]
		built := os.exec(args)
		assert built.exit_code == 0, '${flags}: ${built.output}'
		ran := os.exec([binary])
		assert ran.exit_code == 0, '${flags}: ${ran.output}'
	}
}

fn test_imported_generic_interface_patterns_accept_multi_return_callbacks() {
	root := generic_interface_pattern_fixture('multi_return')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'animation', 'callbacks.v'), 'module animation
fn pair() (int, string) {
	return 7, "seven"
}

fn result_pair() !(int, string) {
	return 11, "eleven"
}

pub fn verify_callbacks() {
	value := Motion(&Animator[fn () (int, string)]{running: true, value: pair})
	if value is Animator[fn () (int, string)] {
		number, word := value.value()
		assert number == 7
		assert word == "seven"
	} else {
		panic("tuple callback specialization did not match")
	}
	pointer_value := Motion(&Animator[fn () (int, string)]{running: true, value: pair})
	if pointer_value is &Animator[fn () (int, string)] {
		number, word := pointer_value.value()
		assert number == 7
		assert word == "seven"
	} else {
		panic("pointer tuple callback specialization did not match")
	}
	result := Motion(Animator[fn () !(int, string)]{running: true, value: result_pair})
	if result is Animator[fn () !(int, string)] {
		number, word := result.value() or { panic(err) }
		assert number == 11
		assert word == "eleven"
	} else {
		panic("result tuple callback specialization did not match")
	}
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'import animation
fn main() { animation.verify_callbacks() }
') or { panic(err) }
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	built := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-o', binary, root])
	assert built.exit_code == 0, built.output
	ran := os.exec([binary])
	assert ran.exit_code == 0, ran.output
}

fn test_imported_generic_interface_patterns_accept_variadic_and_void_callbacks() {
	root := generic_interface_pattern_fixture('variadic_void')
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'animation', 'callbacks.v'), 'module animation
fn total(start int, words ...string) int {
	mut size := start
	for word in words {
		size += word.len
	}
	return size
}

fn result_action() ! {
	return error("result callback")
}

fn option_action() ? {
	return none
}

pub fn verify_callbacks() {
	variadic := Motion(&Animator[fn (int, ...string) int]{running: true, value: total})
	if variadic is Animator[fn (int, ...string) int] {
		assert variadic.value(3, "one", "two") == 9
		assert variadic.value(4) == 4
	} else {
		panic("variadic callback specialization did not match")
	}
	result := Motion(&Animator[fn () !]{running: true, value: result_action})
	if result is &Animator[fn () !] {
		mut called := false
		result.value() or {
			assert err.msg() == "result callback"
			called = true
		}
		assert called
	} else {
		panic("void result callback specialization did not match")
	}
	option := Motion(&Animator[fn () ?]{running: true, value: option_action})
	if option is Animator[fn () ?] {
		mut called := false
		option.value() or { called = true }
		assert called
	} else {
		panic("void option callback specialization did not match")
	}
	match_value := Motion(&Animator[fn () !]{running: true, value: result_action})
	match match_value {
		Animator[fn () !] {
			mut called := false
			match_value.value() or {
				assert err.msg() == "result callback"
				called = true
			}
			assert called
		}
		else {
			panic("void result callback match arm did not match")
		}
	}
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'import animation
fn main() { animation.verify_callbacks() }
') or { panic(err) }
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	built := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-o', binary, root])
	assert built.exit_code == 0, built.output
	ran := os.exec([binary])
	assert ran.exit_code == 0, ran.output
}

fn test_interface_generic_patterns_still_require_known_compatible_types() {
	root := generic_interface_pattern_fixture('invalid')
	defer { os.rmdir_all(root) or {} }
	for pattern in [
		'Missing[f32]',
		'&Missing[f32]',
		'Animator[Missing]',
		'&Animator[Missing]',
		'Animator[[]Missing]',
		'Animator[map[string]Missing]',
		'Animator[fn (...Missing) int]',
		'Animator[fn (int, ...Missing) int]',
		'Animator[fn (...Animator[Missing]) !]',
		'Animator[fn (fn (...Missing) !) ?]',
		'Animator[fn (mut Missing) !]',
		'Animator[fn () (Missing, string)]',
		'Animator[fn () (int, Missing)]',
		'Animator[fn () (int, []Missing)]',
		'Animator[fn () (map[string]Missing, string)]',
		'Animator[fn () (int, fn () (Missing, string))]',
		'Animator[fn () !(int, Missing)]',
		'Animator[fn () (int, Animator[f32, f64])]',
		'Animator[Animator[f32, f64]]',
		'Animator[[]Animator[f32, f64]]',
		'Animator[map[string]Animator[f32, f64]]',
		'Animator[f32, f64]',
		'&Animator[f32, f64]',
		'Plain[f32]',
		'Wrong[f32]',
		'&Wrong[f32]',
	] {
		os.write_file(os.join_path(root, 'animation', 'invalid.v'), 'module animation
pub fn invalid(mut value Motion) {
	if mut value is ${pattern} { println(value) }
}
') or { panic(err) }
		os.write_file(os.join_path(root, 'main.v'), 'import animation
fn main() {
	mut value := animation.Motion(animation.Plain{})
	animation.invalid(mut value)
}
') or { panic(err) }
		checked := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-check', root])
		assert checked.exit_code != 0, '${pattern}: ${checked.output}'
		assert checked.output.contains('unknown type') || checked.output.contains("doesn\'t implement interface"), '${pattern}: ${checked.output}'
	}
}
