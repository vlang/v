module c

import os
import v.cmdexec
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

fn test_manual_free_value_receiver_in_selector_and_lowered_calls() ! {
	path := os.join_path(os.vtmp_dir(), 'manual_free_receiver_${os.getpid()}.v')
	os.write_file(path, 'struct Resource {}
@[unsafe]
fn (mut value Resource) free() {
	unsafe { free(value) }
}
fn main() {
	mut value := Resource{}
	unsafe { value.free() }
}
')!
	defer { os.rm(path) or {} }
	for lowered in [false, true] {
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		if lowered {
			transform.transform(mut a, &tc)
			tc.annotate_types()
		}
		used := markused.mark_used(a, tc)
		mut g := FlatGen.new()
		g.set_target(pref.target_from('linux', 'amd64')!)
		generated := g.gen_with_used_options(a, used, &tc, true)
		assert generated.contains('Resource__free((main__Resource*)memdup(&value,'), generated
		assert !generated.contains('Resource__free(&value)'), generated
	}
}

fn test_manual_free_shadowing_with_unchanged_method_body() ! {
	root := os.join_path(os.vtmp_dir(), 'manual_free_selected_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	freefn := os.join_path(root, 'freefn.vh')
	shadowdrop := os.join_path(root, 'shadowdrop.vh')
	main := os.join_path(root, 'main.v')
	os.write_file(freefn, 'module freefn
pub fn free(_ voidptr)
')!
	os.write_file(main, 'import shadowdrop
fn main() {
	mut shadow := shadowdrop.Resource{value: 21}
	shadow.free()
}
')!
	for declaration in ['import freefn { free }', 'pub fn free(_ voidptr) {}', ''] {
		os.write_file(shadowdrop, 'module shadowdrop
${declaration}
pub struct Resource {
pub mut:
	value int
}
pub fn (mut value Resource) free() {
	value.value = 22
	unsafe { free(value) }
}
')!
		for lowered in [false, true] {
			mut p := parser.Parser.new(pref.new_preferences())
			mut a := p.parse_files([freefn, shadowdrop, main])
			assert p.diagnostics.len == 0, p.diagnostics.str()
			mut tc := types.TypeChecker.new(a)
			tc.collect(a)
			// Incremental builds check the edited program, leaving the cached
			// method body without resolved call names for its free call.
			tc.check_semantics_selected({
				'main': true
			})
			assert tc.errors.len == 0, tc.errors.str()
			if lowered {
				transform.transform(mut a, &tc)
				tc.annotate_types()
			}
			used := markused.mark_used(a, tc)
			mut g := FlatGen.new()
			g.set_target(pref.target_from('linux', 'amd64')!)
			generated := g.gen_with_used_options(a, used, &tc, true)
			if declaration.len == 0 {
				assert generated.contains('shadowdrop__Resource__free((shadowdrop__Resource*)memdup(&shadow,'), generated
			} else {
				assert generated.contains('shadowdrop__Resource__free(&shadow)'), generated
			}
		}
	}
}

fn test_manual_free_imported_receivers_in_cold_and_warm_cache() ! {
	cc := os.find_abs_path_of_executable('gcc') or {
		os.find_abs_path_of_executable('clang') or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'manual_free_cache_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'manual_free_cache' }\n")!
	for name, source in {
		'selfdrop':   'module selfdrop
pub struct Resource {
pub mut:
	value int
}
@[unsafe]
pub fn (mut value Resource) free() {
	value.value = -1
	unsafe { free(voidptr((value))) }
}
'
		'childdrop':  'module childdrop
pub struct Resource {
pub:
	child &u8
pub mut:
	value int
}
@[unsafe]
pub fn (mut value Resource) free() {
	unsafe { free(value.child) }
	value.value = 34
}
'
		'freefn':     'module freefn
pub fn free(_ voidptr) {}
'
		'shadowdrop': 'module shadowdrop
import freefn { free }
pub struct Resource {
pub mut:
	value int
}
pub fn (mut value Resource) free() {
	free(value)
	value.value = 22
}
'
	} {
		os.mkdir_all(os.join_path(root, name))!
		os.write_file(os.join_path(root, name, 'resource.v'), source)!
	}
	source := os.join_path(root, 'main.v')
	executable := os.join_path(root, 'probe')
	for mode in ['uncached', 'cold', 'warm'] {
		os.write_file(source, 'import selfdrop
import childdrop
import shadowdrop
fn main() {
	mut owned := selfdrop.Resource{value: 13}
	unsafe { owned.free() }
	assert owned.value == 13
	mut child := childdrop.Resource{child: unsafe { malloc(1) }, value: 17}
	unsafe { child.free() }
	assert child.value == 34
	mut shadow := shadowdrop.Resource{value: 21}
	shadow.free()
	assert shadow.value == 22
	println("${mode}")
}
')!
		mut args := ['-new-compiler', '-no-retry-compilation', '-cc', cc, '-gc', 'none', '-show-timings',
			'-o', executable]
		if mode == 'uncached' {
			args << '-nocache'
		}
		args << source
		compiled := cmdexec.run_in_merged_with_env(@VEXE, args, root, {
			'VFLAGS':                              ''
			'V3CACHE':                             os.join_path(root, 'cache')
			'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE': '1'
			'V3_DISABLE_UNCHANGED_BUILD_CACHE':    '1'
			'VJOBS':                               '2'
		})
		assert compiled.exit_code == 0, '${mode}: ${compiled.output}'
		if mode == 'warm' {
			header_count := compiled.output.all_after('parsed .vh files').trim_space().all_before(' ').int()
			assert header_count > 0, compiled.output
		}
		run := os.exec([executable])
		assert run.exit_code == 0, '${mode}: ${run.output}'
		assert run.output.trim_space() == mode, run.output
	}
}
