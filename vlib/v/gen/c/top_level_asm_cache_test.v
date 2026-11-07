module c

import os
import v.cmdexec
import v.modulecache
import v.parser
import v.pref
import v.types

fn test_top_level_asm_keeps_file_ownership_and_conditional_guards() ! {
	root := os.join_path(os.vtmp_dir(), 'top_level_asm_owners_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	entry := root + os.path_separator + '.' + os.path_separator + 'entry.v'
	test_file := os.join_path(root, 'package_test.v')
	dependency := os.join_path(root, 'dependency.v')
	inline_only := os.join_path(root, 'inline_only.v')
	for file, module_name in {
		entry:      'entry_package'
		test_file:  'tested_package'
		dependency: 'dependency'
	} {
		os.write_file(file, 'module ${module_name}
\$if linux {
	asm amd64 {
		.global ${module_name}_asm_label
		${module_name}_asm_label:
		.byte 7
	}
}
')!
	}
	os.write_file(inline_only, 'module inline_only
fn nop() {
	asm amd64 {
		nop
	}
}
')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', 'amd64')!
	prefs.output_cross_c = true
	// Build a retained conditional AST to exercise the generator's guarded output.
	prefs.preserve_comptime_conditionals = true
	mut p := parser.Parser.new(prefs)
	for file in [dependency, entry, test_file, inline_only] {
		p.parse_file(file)
	}
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert p.a.nodes.any(it.kind == .comptime_if)
	p.a.resolve_source_paths()
	mut tc := types.TypeChecker.new(p.a)
	tc.collect(p.a)
	for body_only in [false, true] {
		mut g := FlatGen.new()
		g.a = p.a
		g.tc = &tc
		g.set_target(prefs.target)
		g.set_cache_split(true)
		g.set_output_cross_c(true)
		g.set_program_body_only(body_only)
		g.set_cache_program_files(p.a, [os.real_path(entry)])
		g.test_files[test_file] = true
		g.writeln('/* V3CACHE_BODY_BEGIN */')
		g.gen_top_level_asm()
		g.writeln('/* V3CACHE_BODY_END */')
		source := g.sb.str()
		split := modulecache.split_generated_c(source)!
		main := split.modules['main']
		assert main.contains('entry_package_asm_label'), source
		assert main.contains('tested_package_asm_label'), source
		assert !main.contains('dependency_asm_label'), source
		assert main.count('#if') == 2, source
		assert main.count('#endif') == 2, source
		assert 'inline_only' !in split.modules, source
		if body_only {
			assert split.modules.len == 1, source
		} else {
			assert split.modules.len == 2, source
			imported := split.modules['dependency']
			assert imported.contains('dependency_asm_label'), source
			assert imported.count('#if') == 1, source
			assert imported.count('#endif') == 1, source
			assert !imported.contains('entry_package_asm_label'), source
			assert !imported.contains('tested_package_asm_label'), source
		}
	}
}

fn test_top_level_asm_links_in_monolithic_parallel_cold_and_warm_builds() ! {
	$if !amd64 || (!linux && !freebsd) {
		return
	}
	cc := os.find_abs_path_of_executable('gcc') or {
		os.find_abs_path_of_executable('clang') or { return }
	}
	root := os.join_path(os.vtmp_dir(), 'top_level_asm_cache_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'asmdata'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'top_level_asm_cache' }\n")!
	os.write_file(os.join_path(root, 'asmdata', 'data.v'), 'module asmdata
asm amd64 {
	.global v3_cache_imported_asm_value
	v3_cache_imported_asm_value:
	.quad 35
}
pub fn value() i64 {
	mut result := i64(0)
	asm amd64 {
		mov result, [rip + v3_cache_imported_asm_value]
		; =r (result)
	}
	return result
}
')!
	source := os.join_path(root, 'main.v')
	executable := os.join_path(root, 'probe')
	for mode in ['uncached', 'parallel', 'cold', 'warm'] {
		// Editing the entry assembly must not replace the imported module's object.
		value := if mode == 'warm' { 41 } else { 37 }
		os.write_file(source, 'import asmdata
asm amd64 {
	.global v3_cache_main_asm_value
	v3_cache_main_asm_value:
	.quad ${value}
}
fn main() {
	mut result := i64(0)
	asm amd64 {
		mov result, [rip + v3_cache_main_asm_value]
		; =r (result)
	}
	assert result == ${value}
	assert asmdata.value() == 35
	println(result + asmdata.value())
}
')!
		mut args := ['-new-compiler', '-no-retry-compilation', '-cc', cc, '-gc', 'none', '-show-timings',
			'-o', executable]
		if mode in ['uncached', 'parallel'] {
			args << '-nocache'
		}
		if mode == 'parallel' {
			args << ['-parallel-cc', '-showcc']
		}
		args << source
		compiled := cmdexec.run_in_merged_with_env(@VEXE, args, root, {
			'VFLAGS':                              ''
			'V3CACHE':                             os.join_path(root, 'cache')
			'V3_CACHE_TRACE':                      '1'
			'V3_CACHE_DISABLE_PROGRAM_EXECUTABLE': '1'
			'V3_DISABLE_UNCHANGED_BUILD_CACHE':    '1'
			'VJOBS':                               '2'
		})
		assert compiled.exit_code == 0, '${mode}: ${compiled.output}'
		if mode == 'parallel' {
			assert compiled.output.contains('unit_0.c'), compiled.output
			assert compiled.output.contains('unit_1.c'), compiled.output
		}
		if mode == 'warm' {
			header_count := compiled.output.all_after('parsed .vh files').trim_space().all_before(' ').int()
			assert header_count > 0, compiled.output
		}
		run := os.exec([executable])
		assert run.exit_code == 0, '${mode}: ${run.output}'
		assert run.output.trim_space() == '${value + 35}', '${mode}: ${run.output}'
	}
}
