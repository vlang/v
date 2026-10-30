module driver

import os
import v.gen.c as cgen
import v.parser
import v.pref

fn test_native_bridge_preserves_headers_without_opening_or_expanding_them() {
	source := '#include "does-not-exist.h"\n#include <project/header.h>\n#import "also-not-present.h"\n#include "/native/implementation.m"\nint value;\n'
	result := v3_tcc_dylib_declarations(source, ['/native/implementation.m'])
	assert result == '#define V3CACHE_NATIVE_INTERFACE 1\n#include "does-not-exist.h"\n#include <project/header.h>\n#import "also-not-present.h"\nint value;\n'
}

fn test_native_bridge_owner_retains_native_types_without_reading_headers() {
	bindings := v3_tcc_dylib_function_bindings({
		'bridge_value': true
	})
	assert bindings.owner == '__typeof__(&bridge_value) v3_native_bridge_bridge_value = &bridge_value;\n'
	assert v3_tcc_dylib_native_flags(['-D__TINYC__', '-D', '__TINYC__=1', '-DKEEP=1']) == ['-DKEEP=1']
}

fn test_native_bridge_keeps_fallback_for_mixed_native_language_contexts() ! {
	$if !macos {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'native mixed contexts ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'first.m'), 'static int first(void) { return 1; }\n')!
	os.write_file(os.join_path(root, 'second.mm'), 'extern "C" int second(void) { return 2; }\n')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include "@DIR/first.m"\n#include "@DIR/second.mm"\nfn main() {}\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	closure := v3_tcc_dylib_native_closure(a, prefs, [], cc, &cgen.CacheNativeInputs{})
	assert closure.unassignable == 'mixed Objective-C and Objective-C++ bridge contexts'
}

fn test_native_bridge_dependency_manifest_tracks_project_headers() ! {
	$if !macos {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'native bridge manifest ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	compiler := os.join_path(root, 'manifest compiler.sh')
	os.write_file(compiler, '#!/bin/sh\nfor arg do\ncase "$arg" in -E|-dD|-dI) exit 93;; esac\ndone\nexec ${os.quoted_path(cc)} "$@"\n')!
	os.chmod(compiler, 0o755)!

	header := os.join_path(root, 'inner.h')
	os.write_file(header, '#define BRIDGE_VALUE 42\n')!
	os.write_file(os.join_path(root, 'callback.h'), 'typedef void (*BridgeCallback)(void *, int);\n')!
	binding := os.join_path(root, 'binding.h')
	os.write_file(binding, '#pragma once\n#import <CoreText/CoreText.h>\n#import "callback.h"\ntypedef struct { int value; } BridgeValue;\nstatic int bridge_header(BridgeValue value, BridgeCallback callback) { return value.value; }\nstatic CFIndex private_count(CFArrayRef value) { return CFArrayGetCount(value); }\n')!
	native := os.join_path(root, 'native.m')
	os.write_file(native, '#include "inner.h"\n#include "inner.h"\n#include "binding.h"\nenum NativeAnswer { NATIVE_ANSWER = 42 };\nstatic int bridge_value(void) { return BRIDGE_VALUE; }\n')!
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, '#include "@DIR/native.m"\nfn C.bridge_value() int\nfn C.bridge_header(value voidptr) int\nfn main() { assert C.NATIVE_ANSWER == 42 }\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(source)
	resource := os.join_path(root, 'data.txt')
	os.write_file(resource, 'abcdef')!
	native_inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'assets': [header, resource]
		}
		native_paths:  {
			header: true
		}
	}
	closure := v3_tcc_dylib_native_closure(a, prefs, [], compiler, &native_inputs)
	assert closure.unassignable == ''
	assert 'bridge_value' in closure.functions
	assert 'bridge_header' in closure.functions
	assert 'private_count' !in closure.functions
	assert closure.inputs['assets'] == [resource]
	assert closure.native_sources == [os.real_path(native)]
	assert os.real_path(native) in closure.inputs['__v3_c_flags__']
	assert os.real_path(header) in closure.inputs['__v3_c_flags__']
	os.rm(header)!
	missing := v3_tcc_dylib_native_closure(a, prefs, [], compiler, &native_inputs)
	assert missing.unassignable != ''
}

fn test_native_bridge_framework_headers_use_ordinary_include_paths() ! {
	root := os.join_path(os.vtmp_dir(), 'native framework headers ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	headers := os.join_path(root, 'Example.framework', 'Versions', 'A', 'Headers')
	os.mkdir_all(headers)!
	path := os.join_path(headers, 'Example.h')
	os.write_file(path, 'typedef int Example;\n')!
	flags := v3_tcc_dylib_sdk_flags(root, [path])
	assert '-D__MACH__=1' in flags
	assert '-D__has_extension(x)=0' in flags
	include_dir := flags.last().trim_string_left('-I')
	assert os.read_file(os.join_path(include_dir, 'Example', 'Example.h'))! == 'typedef int Example;\n'
}

fn test_native_bridge_links_only_program_frameworks() ! {
	root := os.join_path(os.vtmp_dir(), 'native program frameworks ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	directory := os.join_path(root, 'System', 'Library', 'Frameworks', 'Example.framework')
	os.mkdir_all(directory)!
	library := os.join_path(directory, 'Example.tbd')
	os.write_file(library, 'stub')!
	flags := v3_tcc_dylib_program_link_flags(['-framework', 'Example', '-framework', 'Cocoa', '-lobjc'], '#include <Example/Example.h>\n', root)
	assert flags == [library, '-lobjc']
}

fn test_native_bridge_keeps_basic_objc_runtime_types() {
	text := tcc_cached_main_source('#include <objc/objc.h>\n#include <Foundation/Foundation.h>\n', '')
	assert text.contains('#include <objc/objc.h>')
	assert !text.contains('Foundation/Foundation.h')
}
