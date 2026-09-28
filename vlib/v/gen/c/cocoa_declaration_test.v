module c

import os
import v.cmdexec
import v.flat
import v.pref
import v.types

fn cocoa_declaration_test_gen() FlatGen {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.target = pref.target_from('macos', 'arm64') or { panic(err) }
	g.register_struct_decl_info('C.NSFont', 'C.NSFont', 'ui', 'ui.c.v', flat.Node{})
	return g
}

fn test_cocoa_function_like_include_macros() {
	mut g := cocoa_declaration_test_gen()
	for source, provides_font in {
		'#define UI_HEADER(framework, header) <framework/header.h>\n#include UI_HEADER(Cocoa, Cocoa)':                                                                                        true
		'#define UI_HEADER(framework, header) <framework/header.h>\n#define UI UI_HEADER\n#define FRAMEWORK AppKit\n#include UI(FRAMEWORK, NSFont)':                                          true
		'#define UI_HEADER(framework, header) <framework/header.h>\n#define UI(header) UI_HEADER(AppKit, header)\n#include UI(NSFont)':                                                       true
		'#define QUOTE(header) #header\n#include QUOTE(AppKit/NSFont.h)':                                                                                                                     true
		'#define JOIN(a, b) a ## b\n#define APPLE_UI <Cocoa/Cocoa.h>\n#include JOIN(APPLE, _UI)':                                                                                             true
		'#define UI_HEADER(...) <__VA_ARGS__>\n#include UI_HEADER(AppKit/NSFont.h)':                                                                                                          true
		'#define UI_HEADER(framework, header) <framework/header.h>\n#undef UI_HEADER\n#include UI_HEADER(Cocoa, Cocoa)':                                                                      false
		'#define UI_HEADER(framework, header) <framework/header.h>\n#if FEATURE\n#include UI_HEADER(Cocoa, Cocoa)\n#endif':                                                                   false
		'#if FEATURE\n#define UI_HEADER(framework, header) <framework/header.h>\n#else\n#define UI_HEADER(framework, header) <framework/header.h>\n#endif\n#include UI_HEADER(Cocoa, Cocoa)': true
		'#if FEATURE\n#define UI_HEADER(framework, header) <framework/header.h>\n#else\n#define UI_HEADER(framework, header) <X11/Xlib.h>\n#endif\n#include UI_HEADER(Cocoa, Cocoa)':         false
		'#define UI_HEADER(header) UI_HEADER(header)\n#include UI_HEADER(Cocoa)':                                                                                                             false
	} {
		g.preinclude_directives = [source]
		assert g.header_c_struct_needs_compat_typedef('C.NSFont') == !provides_font, source
	}
	g.c_flags = ['-DUI_HEADER(framework,header)=<framework/header.h>']
	g.preinclude_directives = ['#include UI_HEADER(Cocoa, Cocoa)']
	assert !g.header_c_struct_needs_compat_typedef('C.NSFont')
}

fn test_cocoa_wrappers_declare_nsfont_directly() {
	root := os.join_path(os.vtmp_dir(), 'v3_cocoa_class_wrapper_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'wrapper.h')
	mut g := cocoa_declaration_test_gen()
	g.c_flags = ['-x', 'objective-c']
	g.preinclude_directives = ['#include "${header}"']
	for source, provides_font in {
		'@class NSFont;':                                                    true
		'@class Other,\nNSFont;':                                            true
		'@interface NSFont : NSObject\n@end':                                true
		'#if __OBJC__\n@class NSFont;\n#endif':                              true
		'#if FEATURE\n@class NSFont;\n#else\n@class Other, NSFont;\n#endif': true
		'#if FEATURE\n@class NSFont;\n#endif':                               false
		'#if 0\n@class NSFont;\n#endif':                                     false
		'// @class NSFont;\nconst char *text = "@class NSFont;";':           false
		'/* hidden\n@class NSFont;\n*/\n@class NSFontDescriptor;':           false
		'@class Container<NSFont>;':                                         false
	} {
		os.write_file(header, source + '\n')!
		assert g.header_c_struct_needs_compat_typedef('C.NSFont') == !provides_font, source
	}
	os.write_file(header, '@class NSFont;\n')!
	g.c_flags = ['-x', 'objective-c', '-imacros', header]
	g.preinclude_directives = []
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
}

fn test_cocoa_wrappers_declare_nsfont_compatibility_aliases() {
	root := os.join_path(os.vtmp_dir(), 'v3_cocoa_alias_wrapper_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	header := os.join_path(root, 'wrapper.h')
	mut g := cocoa_declaration_test_gen()
	g.c_flags = ['-x', 'objective-c']
	g.preinclude_directives = ['#include "${header}"']
	for source, provides_font in {
		'@class SomeFont;\n@compatibility_alias NSFont SomeFont;':                                               true
		'@class SomeFont;\n@compatibility_alias\nNSFont /* alias */ SomeFont;':                                  true
		'#if FEATURE\n@compatibility_alias NSFont First;\n#else\n@compatibility_alias NSFont Second;\n#endif':   true
		'@compatibility_alias Other NSFont;':                                                                    false
		'@compatibility_alias NSFontDescriptor SomeFont;':                                                       false
		'#if FEATURE\n@compatibility_alias NSFont SomeFont;\n#endif':                                            false
		'#if 0\n@compatibility_alias NSFont SomeFont;\n#endif':                                                  false
		'// @compatibility_alias NSFont SomeFont;\nconst char *text = "@compatibility_alias NSFont SomeFont;";': false
	} {
		os.write_file(header, source + '\n')!
		assert g.header_c_struct_needs_compat_typedef('C.NSFont') == !provides_font, source
	}
	os.write_file(header, '@class SomeFont;\n@compatibility_alias NSFont SomeFont;\n')!
	g.c_flags = ['-x', 'objective-c', '-imacros', header]
	g.preinclude_directives = []
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
}

fn test_cocoa_function_like_condition_macros() {
	mut g := cocoa_declaration_test_gen()
	for source, provides_font in {
		'#define ENABLED(x) x\n#if ENABLED(1)\n#include <Cocoa/Cocoa.h>\n#endif':                                                   true
		'#define ENABLED(x) x\n#if ENABLED(0)\n#include <Cocoa/Cocoa.h>\n#endif':                                                   false
		'#define ENABLED(x) x\n#if 0\n#elif ENABLED(1)\n#include <Cocoa/Cocoa.h>\n#endif':                                          true
		'#define ENABLED(x) x\n#define ON ENABLED(1)\n#if ON\n#include <Cocoa/Cocoa.h>\n#endif':                                    true
		'#define ENABLED(x) x\n#define APPLY ENABLED\n#if APPLY(1)\n#include <Cocoa/Cocoa.h>\n#endif':                              true
		'#define ENABLED(x) x\n#define BOTH(a,b) ((a) && (b))\n#if BOTH(ENABLED(1), ENABLED(1))\n#include <Cocoa/Cocoa.h>\n#endif': true
		'#define ENABLED(x) x || 1\n#if ENABLED(1) && 0\n#include <Cocoa/Cocoa.h>\n#endif':                                         true
		'#define ENABLED(x) x + 1\n#if ENABLED(0) * 0 == 1\n#include <Cocoa/Cocoa.h>\n#endif':                                      false
		'#define ENABLED() 1\n#if ENABLED()\n#include <Cocoa/Cocoa.h>\n#endif':                                                     true
		'#define ENABLED(...) (__VA_ARGS__)\n#if ENABLED(1 + 1) == 2\n#include <Cocoa/Cocoa.h>\n#endif':                            true
		'#define JOIN(a,b) a ## b\n#define ENABLED_1 1\n#if JOIN(ENABLED_,1)\n#include <Cocoa/Cocoa.h>\n#endif':                    true
		'#define ENABLED(x) x\n#define PRESENT 1\n#if defined(PRESENT) && ENABLED(PRESENT)\n#include <Cocoa/Cocoa.h>\n#endif':      true
		'#define ENABLED(x) x\n#if ENABLED(FEATURE)\n#include <Cocoa/Cocoa.h>\n#endif':                                             false
		'#define ENABLED(x) x\n#undef ENABLED\n#if ENABLED(1)\n#include <Cocoa/Cocoa.h>\n#endif':                                   false
		'#define ENABLED(x) ENABLED(x) + 1\n#if ENABLED(1)\n#include <Cocoa/Cocoa.h>\n#endif':                                      false
		'#if FEATURE\n#define ENABLED(x) 1\n#else\n#define ENABLED(x) 0\n#endif\n#if ENABLED(1)\n#include <Cocoa/Cocoa.h>\n#endif': false
	} {
		g.preinclude_directives = [source]
		assert g.header_c_struct_needs_compat_typedef('C.NSFont') == !provides_font, source
	}
	g.c_flags = ['-DENABLED(x)=x']
	g.preinclude_directives = ['#if ENABLED(1)\n#include <Cocoa/Cocoa.h>\n#endif']
	assert !g.header_c_struct_needs_compat_typedef('C.NSFont')
}

fn test_cocoa_precompiled_forced_headers_declare_nsfont() {
	$if macos {
		root := os.join_path(os.vtmp_dir(), 'v3_cocoa_pch_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		header := os.join_path(root, 'prefix header.h')
		pch := os.join_path(root, 'prefix header.pch')
		mut g := cocoa_declaration_test_gen()
		g.target = pref.host_target()
		g.ccompiler = 'clang'
		g.c_flags = ['-x', 'objective-c', '-include-pch', pch]
		for source, provides_font in {
			'@class NSFont;':                                                   true
			'@interface SomeFont\n@end\n@compatibility_alias NSFont SomeFont;': true
			'#if ENABLE_FONT\n@class NSFont;\n#endif':                          true
			'@interface NSFont
@end':                                           true
			'@class NSFontDescriptor;':                                         false
			'#if 0
@class NSFont;
#endif':                                      false
		} {
			os.write_file(header, source + '
')!
			compiled := cmdexec.run('clang', ['-DENABLE_FONT=1', '-x', 'objective-c-header', header,
				'-o', pch])
			assert compiled.exit_code == 0, compiled.output
			assert g.header_c_struct_needs_compat_typedef('C.NSFont') == !provides_font, source
		}
		g.c_flags = ['-x', 'objective-c', '-include-pch', os.join_path(root, 'missing.pch')]
		assert g.header_c_struct_needs_compat_typedef('C.NSFont')
	}
}

fn test_cocoa_guards_use_compiler_predefined_macros() {
	$if macos {
		mut g := cocoa_declaration_test_gen()
		g.target = pref.host_target()
		g.ccompiler = 'clang'
		for guard in ['defined(__clang__)', '__LP64__', '__SIZEOF_POINTER__ == 8',
			'__STDC_VERSION__ >= 201112L'] {
			g.c_flags = ['-std=c11', '-framework', 'Cocoa']
			g.preinclude_directives = ['#if ${guard}\n#include <Cocoa/Cocoa.h>\n#endif']
			assert !g.header_c_struct_needs_compat_typedef('C.NSFont'), guard
		}
		g.preinclude_directives = ['#ifdef __clang__\n#include <Cocoa/Cocoa.h>\n#endif']
		g.c_flags = ['-U__clang__']
		assert g.header_c_struct_needs_compat_typedef('C.NSFont')
		g.preinclude_directives = ['#if __LP64__\n#include <Cocoa/Cocoa.h>\n#endif']
		g.c_flags = ['-D__LP64__=0']
		assert g.header_c_struct_needs_compat_typedef('C.NSFont')
	}
}

fn test_cocoa_header_names_resolve_before_class_detection() {
	root := os.join_path(os.vtmp_dir(), 'v3_cocoa_shadow_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut g := cocoa_declaration_test_gen()
	g.register_struct_decl_info('C.NSFont', 'C.NSFont', 'ui', os.join_path(root, 'ui.c.v'), flat.Node{})
	for name in ['Cocoa/Cocoa.h', 'AppKit/AppKit.h', 'AppKit/NSFont.h'] {
		path := os.join_path(root, name)
		os.mkdir_all(os.dir(path))!
		os.write_file(path, 'struct NSFont { int value; };\n')!
		for include in ['"${name}"', '<${name}>'] {
			g.c_flags = if include.starts_with('"') { []string{} } else { ['-I', root] }
			g.preinclude_directives = ['#include ${include}']
			assert g.header_c_struct_needs_compat_typedef('C.NSFont'), include
		}
		os.write_file(path, '@class NSFont;\n')!
		assert !g.header_c_struct_needs_compat_typedef('C.NSFont'), name
	}
	os.write_file(os.join_path(root, 'AppKit/NSFont.h'), 'struct NSFont { int value; };\n')!
	g.preinclude_directives = ['#if FEATURE\n#define UI_HEADER <Cocoa/Cocoa.h>\n#else\n#define UI_HEADER <AppKit/NSFont.h>\n#endif\n#include UI_HEADER']
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
	$if macos {
		framework := os.join_path(root, 'Cocoa.framework/Headers')
		os.mkdir_all(framework)!
		os.write_file(os.join_path(framework, 'Cocoa.h'), 'struct NSFont { int value; };\n')!
		g.c_flags = ['-F', root]
		g.preinclude_directives = ['#include <Cocoa/Cocoa.h>']
		assert g.header_c_struct_needs_compat_typedef('C.NSFont')
	}
}

fn test_cocoa_macro_class_declarations() {
	mut g := cocoa_declaration_test_gen()
	g.c_flags = ['-x', 'objective-c']
	for source, provides_font in {
		'#define DECLARE_CLASS(name) @class name;\nDECLARE_CLASS(NSFont)':                                              true
		'#define FONT NSFont\n@class FONT;':                                                                            true
		'#define FONT NSFont\n@class\nFONT;':                                                                           true
		'#define DECLARE_CLASS(name) @class name;\n#define DECLARE DECLARE_CLASS\nDECLARE(NSFont)':                     true
		'#define DECLARE_CLASS(name) @class name;\n#define FONT NSFont\nDECLARE_CLASS(FONT)':                           true
		'#define JOIN(a,b) a ## b\n@class JOIN(NS,Font);':                                                              true
		'#define FONT NSFont\n@compatibility_alias FONT ExistingFont;':                                                 true
		'#define DECLARE @interface NSFont\nDECLARE\n@end':                                                             true
		'#define defined @class NSFont;\ndefined':                                                                      true
		'#define DECLARE_CLASS(name) @class name;\n#if 0\nDECLARE_CLASS(NSFont)\n#endif':                               false
		'#define FONT OtherFont\n@class FONT;':                                                                         false
		'#define DECLARE_CLASS(name) @class name;\n// DECLARE_CLASS(NSFont)\nconst char *s = "DECLARE_CLASS(NSFont)";': false
		'#define END */ @class NSFont; /*\n/* END */':                                                                  false
		'#if FEATURE\n#define FONT NSFont\n#else\n#define FONT OtherFont\n#endif\n@class FONT;':                        false
	} {
		g.preinclude_directives = [source]
		assert g.header_c_struct_needs_compat_typedef('C.NSFont') == !provides_font, source
	}
}

fn test_portable_cocoa_guards_use_target_abi_without_a_compiler() {
	mut g := cocoa_declaration_test_gen()
	g.target = pref.target_from('linux', 'amd64')!
	g.output_cross_c = true
	g.ccompiler = 'v-cocoa-unavailable-compiler-for-test'
	for guard in ['__LP64__', '__SIZEOF_POINTER__ == 8', '__SIZEOF_LONG__ == 8',
		'defined(__APPLE__) && __MACH__'] {
		g.preinclude_directives = ['#if ${guard}\n#include <Cocoa/Cocoa.h>\n#endif']
		assert !g.header_c_struct_needs_compat_typedef('C.NSFont'), guard
	}
	g.preinclude_directives = ['#if __SIZEOF_POINTER__ == 4\n#include <Cocoa/Cocoa.h>\n#endif']
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
	g.preinclude_directives = ['#if __LP64__\n#include <Cocoa/Cocoa.h>\n#endif']
	g.c_flags = ['-U__LP64__']
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
	g.c_flags = ['-D__LP64__=0']
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
	g.c_flags = ['-m32']
	g.preinclude_directives = ['#if !defined(__LP64__) && __SIZEOF_POINTER__ == 4\n#include <Cocoa/Cocoa.h>\n#endif']
	assert !g.header_c_struct_needs_compat_typedef('C.NSFont')
	g.c_flags = ['-undef']
	g.preinclude_directives = ['#if defined(__LP64__) || defined(__APPLE__)\n#include <Cocoa/Cocoa.h>\n#endif']
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
}

fn test_cocoa_search_probe_keeps_cross_compiler_macros() {
	$if macos {
		target := pref.target_from('linux', 'amd64')!
		paths := c_header_compiler_search_paths('clang', ['--target=x86_64-unknown-linux-gnu'],
			'c', target, false)
		assert '__linux__ 1' in paths.predefined_macros
		assert '__LP64__ 1' in paths.predefined_macros
		assert '__APPLE__ 1' !in paths.predefined_macros
	}
}

fn test_cocoa_include_next_continues_after_wrapper_search_directory() {
	root := os.join_path(os.vtmp_dir(), 'v3_cocoa_include_next_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut g := cocoa_declaration_test_gen()
	for framework in [false, true] {
		mut roots := []string{}
		mut headers := []string{}
		for label in ['first', 'second', 'last'] {
			dir := os.join_path(root, if framework { 'frameworks' } else { 'includes' }, label)
			header := os.join_path(dir, if framework {
				'Cocoa.framework/Headers/Cocoa.h'
			} else {
				'Cocoa/Cocoa.h'
			})
			os.mkdir_all(os.dir(header))!
			roots << dir
			headers << header
		}
		os.write_file(headers[0], '#pragma once\n#include_next <Cocoa/Cocoa.h>\n')!
		os.write_file(headers[1], '#define NEXT <Cocoa/Cocoa.h>\n#include_next NEXT\n')!
		os.write_file(headers[2], '@class NSFont;\n')!
		g.c_flags = ['-nostdinc', '-x', 'objective-c']
		for dir in roots { g.c_flags << [if framework { '-F' } else { '-I' }, dir] }
		g.preinclude_directives = ['#include <Cocoa/Cocoa.h>']
		assert !g.header_c_struct_needs_compat_typedef('C.NSFont'), framework.str()
		os.write_file(headers[2], 'struct NSFont { int value; };\n')!
		assert g.header_c_struct_needs_compat_typedef('C.NSFont'), framework.str()
	}
	first := os.join_path(root, 'includes/first')
	second := os.join_path(root, 'includes/second')
	os.write_file(os.join_path(first, 'redirect.h'), '#include_next <font.h>\n')!
	os.write_file(os.join_path(first, 'font.h'), 'struct NSFont { int value; };\n')!
	os.write_file(os.join_path(second, 'font.h'), '@class NSFont;\n')!
	g.c_flags = ['-nostdinc', '-x', 'objective-c', '-I', first, '-I', second]
	g.preinclude_directives = ['#include <redirect.h>']
	assert !g.header_c_struct_needs_compat_typedef('C.NSFont')
	os.write_file(os.join_path(first, 'font.h'), '@class NSFont;\n')!
	os.write_file(os.join_path(second, 'font.h'), 'struct NSFont { int value; };\n')!
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
}

fn test_cocoa_include_next_keeps_quote_and_framework_search_order() {
	root := os.join_path(os.vtmp_dir(), 'v3_cocoa_include_next_order_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	last := os.join_path(root, 'last')
	for dir in [first, second, last] { os.mkdir_all(dir)! }
	os.write_file(os.join_path(second, 'font.h'), '@class NSFont;\n')!
	os.write_file(os.join_path(last, 'font.h'), 'struct NSFont { int value; };\n')!
	mut g := cocoa_declaration_test_gen()
	g.c_flags = ['-nostdinc', '-x', 'objective-c', '-iquote', first, '-iquote', second, '-I', last]
	g.preinclude_directives = ['#include "wrapper.h"']
	for include_arg in ['<font.h>', '"font.h"'] {
		os.write_file(os.join_path(first, 'wrapper.h'), '#include_next ${include_arg}\n')!
		assert !g.header_c_struct_needs_compat_typedef('C.NSFont'), include_arg
	}
	for first_framework in [true, false] {
		mut flags := ['-nostdinc', '-x', 'objective-c']
		for index, dir in [first, second, last] {
			framework := (index != 1) == first_framework
			header := os.join_path(dir, if framework {
				'Cocoa.framework/Headers/Cocoa.h'
			} else {
				'Cocoa/Cocoa.h'
			})
			os.mkdir_all(os.dir(header))!
			os.write_file(header, if index == 0 {
				'#define WRAPPER_FOUND 1\n#include_next <Cocoa/Cocoa.h>\n'
			} else if index == 1 {
				'#include_next <Cocoa/Cocoa.h>\n'
			} else {
				'#if WRAPPER_FOUND\n@class NSFont;\n#endif\n'
			})!
			flags << [if framework { '-F' } else { '-I' }, dir]
		}
		g.c_flags = flags
		g.preinclude_directives = ['#include <Cocoa/Cocoa.h>']
		assert !g.header_c_struct_needs_compat_typedef('C.NSFont'), first_framework.str()
	}
}

fn test_cocoa_include_next_predicate_uses_current_search_position() {
	root := os.join_path(os.vtmp_dir(), 'v3_cocoa_has_include_next_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first')
	second := os.join_path(root, 'second')
	for dir in [first, second] { os.mkdir_all(dir)! }
	wrapper := os.join_path(first, 'wrapper.h')
	font := os.join_path(second, 'font.h')
	os.write_file(os.join_path(first, 'font.h'), 'struct NSFont { int value; };\n')!
	mut g := cocoa_declaration_test_gen()
	g.c_flags = ['-nostdinc', '-x', 'objective-c', '-I', first, '-I', second]
	g.preinclude_directives = ['#include <wrapper.h>']
	os.write_file(wrapper, '#define NEXT <font.h>\n#if __has_include_next(NEXT)\n#include_next NEXT\n#endif\n')!
	os.write_file(font, '@class NSFont;\n')!
	assert !g.header_c_struct_needs_compat_typedef('C.NSFont')
	os.rm(font)!
	os.write_file(wrapper, '#if !__has_include_next(<font.h>)\n@class NSFont;\n#endif\n')!
	assert !g.header_c_struct_needs_compat_typedef('C.NSFont')
	os.write_file(font, 'struct NSFont { int value; };\n')!
	assert g.header_c_struct_needs_compat_typedef('C.NSFont')
}
