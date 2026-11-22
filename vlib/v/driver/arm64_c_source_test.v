module driver

import os
import time
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

// Check that closure-free arm64 probes retain C lines and diagnostic line numbers.
fn test_arm64_probe_line_source_preserves_all_probe_lines() {
	request := Arm64CProbeRequest{
		functions: ['first', 'second']
		constants: ['THIRD']
		structs:   [
			Arm64CStruct{
				v_name: 'C.fourth'
				c_type: 'struct fourth'
				fields: ['last']
			},
		]
	}
	keep := [true, false, true, true, false]
	preamble := '#include <probe.h>\n'
	source := Arm64ProbeLineSource{
		request:      request
		preamble:     preamble
		with_layouts: true
	}
	expected := arm64_c_function_probe_lines(request.functions, keep, 0) + preamble +
		arm64_c_constant_probe_lines(request.constants, keep, 2) +
		arm64_c_struct_probe_lines(request.structs, keep, 3)
	assert source.render(keep) == expected
	assert source.render([false, true, true, true, true]) != expected

	functions_only := Arm64ProbeLineSource{
		request: Arm64CProbeRequest{
			functions: ['first', 'second']
		}
	}
	actual := functions_only.render([true, false])
	assert actual == arm64_c_function_probe_lines(['first', 'second'], [true, false], 0)
	assert !actual.contains('#include')
}

// arm64_cache_fixture is a directory of headers and a C unit that names them, kept
// apart from other runs by a comment. The headers are aged so that the cache takes
// them as settled.
fn arm64_cache_fixture(name string) (string, string) {
	root := os.join_path(os.vtmp_dir(), 'arm64_c_cache_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root, '// ${root}\n#include <choice.h>\nint value(void) { return CHOICE; }\n'
}

fn arm64_cache_age(paths []string) {
	stamp := time.utc().unix() - 30
	for path in paths {
		os.utime(path, stamp, stamp) or { panic(err) }
	}
}

fn arm64_cache_hit(args []string, text string) ?string {
	mut stats := CObjectCacheStats{}
	return arm64_cached_object_with('cc', args, text, pref.host_target(), mut stats)
}

fn arm64_cache_compile(args []string, text string) string {
	mut stats := CObjectCacheStats{}
	mut inputs := Arm64CInputs{}
	return arm64_compile_unit_with('cc', args, text, pref.host_target(), false, mut stats, mut
		inputs) or { panic(err) }
}

// The depfile lists the header that was found, not the earlier directory where a
// new one would be taken first.
fn test_arm64_native_headers_do_not_certify_object_reuse() {
	$if macos && arm64 {
		root, text := arm64_cache_fixture('earlier')
		defer {
			os.rmdir_all(root) or {}
		}
		earlier := os.join_path(root, 'earlier')
		later := os.join_path(root, 'later')
		os.mkdir_all(earlier)!
		os.mkdir_all(later)!
		header := os.join_path(later, 'choice.h')
		os.write_file(header, '#define CHOICE 1\n')!
		arm64_cache_age([header, earlier, later])
		args := ['-I', earlier, '-I', later]
		first := arm64_cache_compile(args, text)
		defer { os.rm(first) or {} }
		assert first.contains('.tmp.'), first
		assert arm64_cache_hit(args, text) == none
		os.write_file(os.join_path(earlier, 'choice.h'), '#define CHOICE 2\n')!
		second := arm64_cache_compile(args, text)
		defer { os.rm(second) or {} }
		assert os.read_file(first)! != os.read_file(second)!
		assert arm64_cache_hit(args, text) == none
	}
}

// A file replaced with the same size and modification time has other contents.
fn test_arm64_object_follows_a_header_with_the_same_size_and_time() {
	$if macos && arm64 {
		root, text := arm64_cache_fixture('same_stat')
		defer {
			os.rmdir_all(root) or {}
		}
		header := os.join_path(root, 'choice.h')
		os.write_file(header, '#define CHOICE 1\n')!
		arm64_cache_age([header, root])
		args := ['-I', root]
		first := arm64_cache_compile(args, text)
		defer { os.rm(first) or {} }
		assert first.contains('.tmp.'), first
		assert arm64_cache_hit(args, text) == none
		before := os.stat(header)!
		os.write_file(header, '#define CHOICE 2\n')!
		os.utime(header, before.atime, before.mtime)!
		after := os.stat(header)!
		assert after.size == before.size && after.mtime == before.mtime
		second := arm64_cache_compile(args, text)
		defer { os.rm(second) or {} }
		assert os.read_file(first)! != os.read_file(second)!
		assert arm64_cache_hit(args, text) == none
	}
}

// Retargeting a header after cc returns leaves its old object usable for this
// build but must not create a reusable manifest for the new target.
fn test_arm64_object_is_not_published_when_a_header_moved_during_the_compile() {
	$if macos && arm64 {
		root, text := arm64_cache_fixture('moved')
		defer { os.rmdir_all(root) or {} }
		one := os.join_path(root, 'one.h')
		two := os.join_path(root, 'two.h')
		header := os.join_path(root, 'choice.h')
		os.write_file(one, '#define CHOICE 1\n')!
		os.write_file(two, '#define CHOICE 2\n')!
		os.symlink(one, header)!
		arm64_cache_age([one, two])
		wrapper := os.join_path(root, 'wrapper')
		os.write_file(wrapper, '#!/bin/sh\n/usr/bin/cc "$@" || exit $?\n' +
			'case " $* " in *" -c "*) ln -sfn ${os.quoted_path(two)} ${os.quoted_path(header)} ;; esac\n')!
		os.chmod(wrapper, 0o755)!
		args := ['-I', root]
		mut stats := CObjectCacheStats{}
		mut inputs := Arm64CInputs{}
		first := arm64_compile_unit_with(wrapper, args, text, pref.host_target(), false,
			mut stats, mut inputs)!
		defer { os.rm(first) or {} }
		assert os.readlink(header)! == two
		assert first.contains('.tmp.'), first
		assert inputs.unknown == v3_native_header_cache_bypass
		assert arm64_cached_object_with(wrapper, args, text, pref.host_target(), mut stats) == none
	}
}

fn test_arm64_object_declines_reuse_with_a_time_macro_in_a_header() {
	$if macos && arm64 {
		root := os.join_path(@VEXEROOT, 'arm64_cache_time_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		header := os.join_path(root, 'choice.h')
		os.write_file(header, '#define CHOICE __TIME__[7]\n')!
		arm64_cache_age([header])
		text := '// ${root}\n#include <choice.h>\nint value(void) { return CHOICE; }\n'
		args := ['-I', root]
		mut stats := CObjectCacheStats{}
		mut inputs := Arm64CInputs{}
		object := arm64_compile_unit_with('cc', args, text, pref.host_target(), false, mut stats,
			mut inputs)!
		defer { os.rm(object) or {} }
		assert object.contains('.tmp.'), object
		assert inputs.unknown == v3_native_header_cache_bypass
		assert arm64_cache_hit(args, text) == none
	}
}

fn test_arm64_headerless_freestanding_c_object_is_reused() {
	$if macos && arm64 {
		text := '// arm64 standalone ${os.getpid()}\nint value(void) { return 42; }'
		args := ['-ffreestanding', '-isysroot', '/']
		first := arm64_cache_compile(args, text)
		assert !first.contains('.tmp.'), first
		assert arm64_cache_hit(args, text) or { '' } == first
	}
}

fn test_arm64_builds_an_opaque_compilers_object_locally() ! {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_opaque_compiler_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		config := os.join_path(root, 'setting')
		wrapper := os.join_path(root, 'cc')
		os.write_file(wrapper, '#!/bin/sh
exec /usr/bin/cc "-DVALUE=$(cat ${os.quoted_path(config)})" "$@"
')!
		os.chmod(wrapper, 0o755)!
		main_c := os.join_path(root, 'main.c')
		os.write_file(main_c, 'int value(void); int main(void) { return value(); }\n')!
		program := os.join_path(root, 'program')
		text := '// ${root}\nint value(void) { return VALUE; }\n'
		args := ['-ffreestanding', '-isysroot', '/']
		mut stats := CObjectCacheStats{}
		mut inputs := Arm64CInputs{}
		defer { inputs.cleanup() }
		for value in [1, 2] {
			os.write_file(config, value.str())!
			object := arm64_compile_unit_with(wrapper, args, text, pref.host_target(), false,
				mut stats, mut inputs)!
			assert object in inputs.temporary_objects
			assert arm64_cached_object_with(wrapper, args, text, pref.host_target(), mut stats) == none
			linked := os.exec(['/usr/bin/cc', main_c, object, '-o', program])
			assert linked.exit_code == 0, linked.output
			result := os.exec([program])
			assert result.exit_code == value, result.output
		}
	}
}
