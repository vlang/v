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
			'@class NSFont;':                          true
			'#if ENABLE_FONT\n@class NSFont;\n#endif': true
			'@interface NSFont
@end':                  true
			'@class NSFontDescriptor;':                false
			'#if 0
@class NSFont;
#endif':             false
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
