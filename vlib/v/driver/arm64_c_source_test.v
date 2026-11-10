module driver

import v.flat
import v.pref

fn test_arm64_rejects_native_source_includes() {
	for directive in ['include', 'insert', 'preinclude', 'postinclude'] {
		for ext in ['c', 'cc', 'cpp', 'cxx', 'C', 'm', 'mm', 's', 'S'] {
			mut a := flat.FlatAst.new()
			a.add_node(flat.Node{
				kind:  .directive
				value: directive
				typ:   flat.source_type_id('"@VMODROOT/native.${ext}"')
			})
			message := unsupported_arm64_c_source_error(a) or { '' }
			assert message.contains('built-in arm64 linker cannot link native source'), message
			assert message.contains('native.${ext}'), message
			assert message.contains('arm64_internal_link'), message
		}
	}
}

fn test_arm64_accepts_header_includes_and_other_directives() {
	mut a := flat.FlatAst.new()
	for path in ['<signal.h>', '"local.c.h"', '"@VMODROOT/native.hpp"'] {
		a.add_node(flat.Node{
			kind:  .directive
			value: 'include'
			typ:   flat.source_type_id(path)
		})
	}
	a.add_node(flat.Node{
		kind:  .directive
		value: 'flag'
		typ:   flat.source_type_id('-DNAME=file.c')
	})
	assert unsupported_arm64_c_source_error(a) == none
}

fn directive(value string, arg string) flat.Node {
	return flat.Node{
		kind:  .directive
		value: value
		typ:   flat.source_type_id(arg)
	}
}

fn test_arm64_link_mode_keeps_the_builtin_linker_for_libc_programs() {
	mut a := flat.FlatAst.new()
	a.add_node(directive('include', '<signal.h>'))
	mode, reason := arm64_link_mode(a, ['-lm', '-lpthread', '-I', '/x', '-DNAME=1', '-l', 'dl'],
		[]string{})
	assert mode == .internal
	assert reason == ''
}

fn test_arm64_link_mode_uses_the_system_linker_for_native_code() {
	mut a := flat.FlatAst.new()
	mode0, _ := arm64_link_mode(a, ['-framework', 'Metal'], []string{})
	assert mode0 == .external
	mode1, reason1 := arm64_link_mode(a, ['-lfreetype'], []string{})
	assert mode1 == .external
	assert reason1.contains('-lfreetype')
	mode2, _ := arm64_link_mode(a, ['/opt/lib/libgc.a'], []string{})
	assert mode2 == .external
	mode3, _ := arm64_link_mode(a, ['@VEXEROOT/thirdparty/libgc/gc.o'], []string{})
	assert mode3 == .external
	mode4, reason4 := arm64_link_mode(a, []string{}, ['gcboehm'])
	assert mode4 == .external
	assert reason4.contains('Boehm')
	a.add_node(directive('include', '"@VMODROOT/embedder.m"'))
	mode5, reason5 := arm64_link_mode(a, []string{}, []string{})
	assert mode5 == .external
	assert reason5.contains('embedder.m')
	mode6, _ := arm64_link_mode(a, ['-framework', 'Metal'], ['arm64_internal_link'])
	assert mode6 == .internal
	mode7, _ := arm64_link_mode(flat.FlatAst.new(), []string{}, ['arm64_external_link'])
	assert mode7 == .external
}

fn test_arm64_link_mode_uses_the_system_linker_for_project_headers() {
	// A project header may hold `static inline` functions and macros, which
	// only exist in the C side.
	for header in ['"@VMODROOT/desc.h"', '"local.h"', 'darwin "@VMODROOT/desc.h"'] {
		mut a := flat.FlatAst.new()
		a.add_node(directive('include', '<stdio.h>'))
		a.add_node(directive('include', header))
		mode, reason := arm64_link_mode(a, []string{}, []string{})
		assert mode == .external, header
		assert reason.contains('.h'), reason
	}
	// System headers and V's own vlib headers keep the built-in linker.
	mut a := flat.FlatAst.new()
	for header in ['<stdio.h>', '"@VEXEROOT/vlib/builtin/x.h"', '"@VMODROOT/vlib/os/y.h"'] {
		a.add_node(directive('include', header))
	}
	mode, _ := arm64_link_mode(a, []string{}, []string{})
	assert mode == .internal
}

fn test_native_c_support_source_follows_the_target() {
	mut a := flat.FlatAst.new()
	a.add_node(directive('include', 'linux <sys/timerfd.h>'))
	a.add_node(directive('include', 'darwin <mach/mach_time.h>'))
	a.add_node(directive('define', 'linux V_ONLY_ON_LINUX'))
	a.add_node(directive('define', 'SOKOL_METAL'))
	a.add_node(directive('include', '<stdio.h>'))
	support := native_c_support_source(a, @VEXEROOT, pref.Target{
		os:   'macos'
		arch: 'arm64'
	}, map[string]string{})
	assert !support.text.contains('timerfd'), support.text
	assert !support.text.contains('V_ONLY_ON_LINUX'), support.text
	assert support.text.contains('#include <mach/mach_time.h>'), support.text
	assert support.text.contains('#define SOKOL_METAL'), support.text
	assert support.text.index('SOKOL_METAL') or { -1 } < support.text.index('<stdio.h>') or { -1 }
}

fn test_arm64_c_constant_probe_lines_isolate_each_name() {
	text := arm64_c_constant_probe_lines(['FOO', 'BAR', 'BAZ'], [true, false, true], 0)
	assert text.contains('#line 1 "${arm64_c_probe_file}"')
	assert !text.contains('(BAR)')
	assert text.contains('#line 3 "${arm64_c_probe_file}"')
	assert text.contains('__v_cconst_i_2 = (long long)(BAZ)')
	assert arm64_is_c_identifier('SOKOL_MAX_IMAGES')
	assert !arm64_is_c_identifier('a.b')
	assert !arm64_is_c_identifier('1abc')
}

fn test_arm64_c_constants_leave_declared_c_functions_to_the_function_probe() {
	mut a := flat.FlatAst.new()
	a.add_node(flat.Node{
		kind:  .c_fn_decl
		value: 'C.ext_add'
	})
	for name in ['ext_add', 'EXT_LIMIT'] {
		child := a.add_node(flat.Node{
			kind:  .ident
			value: 'C'
		})
		start := a.children.len
		a.add_child(child)
		a.add_node(flat.Node{
			kind:           .selector
			value:          name
			children_start: start
			children_count: 1
		})
	}
	assert arm64_c_constant_names(a) == ['EXT_LIMIT']
	names := arm64_c_function_decl_names(a)
	assert 'ext_add' in names
	assert 'strtod' in names
}

fn test_arm64_c_probe_lines_share_one_line_space() {
	keep := []bool{len: 4, init: true}
	functions := arm64_c_function_probe_lines(['first'], keep, 0)
	assert functions.contains('#line 1 "${arm64_c_probe_file}"')
	constants := arm64_c_constant_probe_lines(['SECOND'], keep, 1)
	assert constants.contains('#line 2 "${arm64_c_probe_file}"')
	structs := arm64_c_struct_probe_lines([
		Arm64CStruct{
			v_name: 'C.third'
			c_type: 'struct third'
			fields: ['f']
		},
	], keep, 2)
	assert structs.contains('#line 3 "${arm64_c_probe_file}"')
	assert structs.contains('#line 4 "${arm64_c_probe_file}"')
}

fn test_arm64_c_compiler_replaces_tinycc() {
	assert arm64_c_compiler('') == 'cc'
	assert arm64_c_compiler('tcc') == 'cc'
	assert arm64_c_compiler('clang') == 'clang'
}
