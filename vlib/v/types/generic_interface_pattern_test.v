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
	for arg in ['f32', 'f64', '[]f32', '[2]f32', 'map[string]f32', 'fn (f32) f64', 'Animator[f32]'] {
		pattern := tc.resolve_interface_match_pattern(flat.source_type_id('Animator[${arg}]')) or {
			panic('unresolved pattern Animator[${arg}]')
		}
		assert flat.source_type_syntax(pattern).application_base() == flat.source_type_id('animation.Animator')
		assert tc.parse_type(pattern).source_type_id() == pattern
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
	root := generic_interface_pattern_fixture('runtime')
	defer { os.rmdir_all(root) or {} }
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

fn test_interface_generic_patterns_still_require_known_compatible_types() {
	root := generic_interface_pattern_fixture('invalid')
	defer { os.rmdir_all(root) or {} }
	for pattern in [
		'Missing[f32]',
		'Animator[Missing]',
		'Animator[[]Missing]',
		'Animator[map[string]Missing]',
		'Animator[Animator[f32, f64]]',
		'Animator[[]Animator[f32, f64]]',
		'Animator[map[string]Animator[f32, f64]]',
		'Animator[f32, f64]',
		'Plain[f32]',
		'Wrong[f32]',
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
