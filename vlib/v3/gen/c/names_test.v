module c

import os
import v3.flat
import v3.pref
import v3.types

// test_c_name_sanitize_operator_overloads validates this v3 regression case.
fn test_c_name_sanitize_operator_overloads() {
	assert c_name('Point.<') == 'Point__lt'
	assert c_name('Point.<=') == 'Point__le'
	assert c_name('Point.>') == 'Point__gt'
	assert c_name('Point.>=') == 'Point__ge'
	assert c_name('Point.[]') == 'Point__op_index'
	assert c_name('Point.[]=') == 'Point__op_index_set'
}

fn test_c_name_sanitize_escaped_keywords() {
	assert c_name('@true') == '_v_true'
	assert c_name('@false') == '_v_false'
	assert c_name('Kind.@asm') == 'Kind___v_asm'
}

fn test_c_name_sanitizes_compound_generic_type_arguments() {
	name :=
		c_name('json2.StructKeyDecodeResult[fn(&mbedtls.SSLListener, string) !&mbedtls.SSLCerts]')
	assert name.bytes().all((it >= `a` && it <= `z`) || (it >= `A` && it <= `Z`)
		|| (it >= `0` && it <= `9`) || it == `_`)
}

fn test_c_name_libc_collision_abs() {
	assert c_name('abs') == 'v_abs'
	assert c_name('send') == 'v_send'
	assert c_name('C.abs') == 'abs'
	assert c_name('printf') == 'v_printf'
	assert c_name('C.printf') == 'printf'
	assert c_name('C.send') == 'send'
}

fn test_c_name_generated_string_symbol_collision() {
	assert c_name('_str_1') == 'v__str_1'
	assert c_name('_str_002') == 'v__str_002'
	assert c_name('_str_value') == '_str_value'
	assert c_name('C._str_3') == '_str_3'
}

fn test_direct_call_uses_custom_enum_method_symbol() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.enum_names['token.Kind'] = true
	tc.cur_module = 'ast'
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	assert g.direct_call_name('token.Kind.str') == 'token__Kind_str'
	assert g.direct_call_name_for_call(flat.empty_node, 'token.Kind.str') == 'token__Kind_str'
	tc.enum_names['ast.Kind'] = true
	assert g.direct_call_name('Kind.str') == 'Kind_str'
}

fn test_context_lookup_cache_tracks_source_file_imports() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.file_imports['one.v\nkind'] = 'first.token'
	tc.file_imports['two.v\nkind'] = 'second.token'
	tc.enum_names['first.token.Kind'] = true
	tc.enum_names['second.token.Kind'] = true
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	tc.cur_file = 'one.v'
	assert g.import_alias_module('kind')? == 'first.token'
	assert g.enum_selector_base_name('kind.Kind')? == 'first.token.Kind'
	tc.cur_file = 'two.v'
	assert g.import_alias_module('kind')? == 'second.token'
	assert g.enum_selector_base_name('kind.Kind')? == 'second.token.Kind'
}

fn test_cgen_flattened_generic_receiver_short_variants() {
	assert cgen_flattened_generic_receiver_short_variants('foo__Bar_baz__Qux') == [
		'Bar_Qux',
	]
	assert cgen_flattened_generic_receiver_short_variants('mod.foo__Bar_baz__Qux') == [
		'Bar_Qux',
		'mod.Bar_Qux',
	]
}

fn test_cgen_typeof_display_canonicalizes_fixed_array_generic_args() {
	assert typeof_display_type_name('Box[fn () int]') == 'Box[fn () int]'
	assert typeof_display_type_name('Box[chan int]') == 'Box[chan int]'
	assert typeof_display_type_name('chan int[3]') == 'chan [3]int'
	assert typeof_display_type_name('Box[int[3]]') == 'Box[[3]int]'
	assert typeof_display_type_name('Pair[int[3], Box[string[2]]]') == 'Pair[[3]int, Box[[2]string]]'
	assert typeof_display_type_name('Box[int][3]') == '[3]Box[int]'
	fixed_maps := types.Type(types.ArrayFixed{
		elem_type: types.Type(types.Map{
			key_type:   types.Type(types.String{})
			value_type: types.Type(types.int_)
		})
		len:       3
	})
	assert typeof_display_resolved_type_name(fixed_maps) == '[3]map[string]int'
}

fn test_fixed_array_typedef_allows_opaque_pointer_elements() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.tc = &tc
	opaque := types.Type(types.Struct{
		name: 'C.Foo'
	})
	assert g.fixed_array_type_has_unknown_struct(opaque)
	assert !g.fixed_array_type_has_unknown_struct(types.Type(types.Pointer{
		base_type: opaque
	}))
}

fn test_sum_type_index_rejects_ambiguous_qualified_suffix() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types['a.tast.Value'] = ['a.tast.First', 'a.tast.Target']
	tc.sum_types['b.tast.Value'] = ['b.tast.Target', 'b.tast.Second']
	mut g := FlatGen.new()
	g.tc = &tc
	assert g.sum_type_index('tast.Value', 'b.tast.Target') == 0
}

fn test_typeof_type_index_fallback_uses_matching_sum_variant() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	tc.sum_types['main.Value'] = ['main.Foo', 'main.Bar']
	tc.sum_types['main.Other'] = ['main.Baz', 'main.Qux']
	mut g := FlatGen.new()
	g.tc = &tc
	assert g.type_index_for_type_name('Foo') == 1
	assert g.type_index_for_type_name('Bar') == 2
	assert g.type_index_for_type_name('Qux') == 2
	assert g.type_index_for_type_name('NotVariant') == 0
}

fn test_fn_decl_variadic_resolves_alias_before_short_fallback() {
	mut g := FlatGen.new()
	g.modules['http'] = 'b.http'
	g.fn_decl_variadic['b.http.total'] = true
	g.fn_decl_variadic['total'] = false
	g.fn_decl_variadic_short_counts['total'] = 2
	assert g.fn_decl_is_variadic('http.total', 'http.total')
	assert !g.fn_decl_is_variadic('missing.total', 'missing.total')
	assert !g.fn_decl_is_variadic('missing__total', 'total')
	g.fn_decl_variadic_short_counts['total'] = 1
	g.fn_decl_variadic['total'] = true
	assert g.fn_decl_is_variadic('missing.total', 'missing.total')
}

fn test_guarded_preamble_externs_keep_explicit_declarations() {
	mut g := FlatGen.new()
	assert g.should_emit_c_extern_decl('fseeko')
	assert g.should_emit_c_extern_decl('ftello')
	assert g.should_emit_c_extern_decl('mkdir')
	assert g.should_emit_c_extern_decl('chmod')
	assert g.should_emit_c_extern_decl('symlink')
	assert g.should_emit_c_extern_decl('request')
	g.c_directives << CDirective{
		text: '#include <math.h>'
	}
	for name in ['accept', 'bind', 'chdir', 'execve', 'getuid', 'gmtime_r', 'ioctl', 'rmdir'] {
		assert !g.should_emit_c_extern_decl(name)
	}
}

fn test_preserved_system_include_declarations_are_header_specific() {
	assert c_preserved_system_include_declared_fns('<stdio.h>').len == 0
	assert c_preserved_system_include_declared_fns('<openssl/ssl.h>') == ['X509_free']
	assert c_preserved_system_include_declared_fns('<openssl/x509.h>') == [
		'X509_free',
	]
	assert c_preserved_system_include_declared_fns('<objc/message.h>') == [
		'objc_msgSend',
	]
	assert c_preserved_system_include_struct_names('<poll.h>') == ['pollfd']
}

fn test_apple_framework_include_does_not_match_x11() {
	assert c_is_apple_framework_include('<Cocoa/Cocoa.h>')
	assert c_is_apple_framework_include('<CoreFoundation/CFString.h>')
	assert !c_is_apple_framework_include('<X11/Xlib.h>')
	assert !c_is_apple_framework_include('<sys/ptrace.h>')
}

fn test_objective_c_header_detection() {
	assert c_header_text_needs_objective_c('#import <Cocoa/Cocoa.h>\n')
	assert c_header_text_needs_objective_c('@interface AppDelegate : NSObject\n@end\n')
	assert c_header_text_needs_objective_c('@class ForwardDeclaredClass;\n')
	assert c_header_text_needs_objective_c('@protocol ForwardDeclaredProtocol;\n')
	assert c_header_text_needs_objective_c('id value = @"Objective-C string";\n')
	assert c_header_text_needs_objective_c('static inline id<NSCopying> copy(id<NSCopying> value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline id identity(id obj) { return obj; }\n')
	assert c_header_text_needs_objective_c('static inline instancetype identity(instancetype obj) { return obj; }\n')
	assert c_header_text_needs_objective_c('static inline Class identity(Class value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline SEL identity(SEL value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline Protocol *identity(Protocol *value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline NSArray<NSString *> *copy_names(NSArray<NSString *> *value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline NSObject<NSCopying> *copy(NSObject<NSCopying> *value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(void) { return @42; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(void) { return @YES; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(void) { return @[]; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(void) { return @{}; }\n')
	assert c_header_text_needs_objective_c('static inline id answer(int value) { return @(value); }\n')
	assert c_header_text_needs_objective_c('id value = (__bridge id)pointer;\n')
	assert c_header_text_needs_objective_c('void *value = (__bridge_retained void *)pointer;\n')
	assert c_header_text_needs_objective_c('Object value = (__bridge_transfer Object)pointer;\n')
	assert c_header_text_needs_objective_c('typedef struct objc_class *Class; static __strong Class identity(__strong Class value) { return value; }\n')
	assert c_header_text_needs_objective_c('static __weak Class weak_value; __autoreleasing Class *out_value; __unsafe_unretained Class unsafe_value;\n')
	assert c_header_text_needs_objective_c('static __kindof Class identity(__kindof Class value) { return value; }\n')
	assert c_header_text_needs_objective_c('static inline id helper(id obj) { return [obj description]; }\n')
	assert c_header_text_needs_objective_c('static inline void *identity(void *value) { return (id)value; }\n')
	assert c_header_text_needs_objective_c('static inline void *identity(void *value) { return (Class *)value; }\n')
	assert !c_header_text_needs_objective_c('static inline int helper(int *values, int i) { return values[i]; }\nint table[4] = {[0] = 1};\n[[gnu::unused]] static int state;\n// [obj description] @42\nconst char *message = "[obj description] @42";\n')
	assert !c_header_text_needs_objective_c('static inline int id(int obj) { return obj; }\n')
	assert !c_header_text_needs_objective_c('typedef unsigned id; static inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('typedef void *Class; static inline Class identity(Class value) { return value; }\n')
	assert !c_header_text_needs_objective_c('typedef unsigned SEL; static inline SEL identity(SEL value) { return value; }\n')
	assert !c_header_text_needs_objective_c('typedef void *Protocol; static inline Protocol identity(Protocol value) { return value; }\n')
	assert !c_header_text_needs_objective_c('struct Class { int value; }; union id { int value; }; enum SEL { sel_value }; struct Protocol { int value; }; union instancetype { int value; };\nstatic struct Class class_identity(struct Class value) { return value; }\nstatic union id id_identity(union id value) { return value; }\nstatic enum SEL sel_identity(enum SEL value) { return value; }\nstatic struct Protocol protocol_identity(struct Protocol value) { return value; }\nstatic union instancetype instance_identity(union instancetype value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#define id unsigned\n#define Class unsigned\n#define SEL unsigned\n#define Protocol unsigned\n#define instancetype unsigned\nid id_value; Class class_value; SEL selector_value; Protocol protocol_value; instancetype instance_value;\n')
	assert !c_header_text_needs_objective_c('static int id; static inline int consume(int value) { return value; } static inline int use(void) { return consume(id) + 1; }\n')
	assert c_header_text_needs_objective_c('#define id unsigned\n#undef id\nstatic inline void *identity(void *value) { return (id)value; }\n')
	assert c_header_text_needs_objective_c('static inline void *identity(void *value) { return (id)value; }\n#define id unsigned\n')
	assert c_header_text_needs_objective_c('#define id(value) value\nstatic inline id identity(id value) { return value; }\n')
	assert c_header_text_needs_objective_c('#if 0\ntypedef unsigned id;\n#endif\nstatic inline id identity(id value) { return value; }\n')
	assert c_header_text_needs_objective_c('#define IGNORE(...)\nIGNORE(typedef unsigned id);\nstatic inline id identity(id value) { return value; }\n')
	assert !c_header_text_needs_objective_c('#define __bridge\nvoid *value = (__bridge void *)pointer;\n')
	assert !c_header_text_needs_objective_c('#define __bridge_retained\nvoid *value = (__bridge_retained void *)pointer;\n')
	assert !c_header_text_needs_objective_c('#define __bridge_transfer\nvoid *value = (__bridge_transfer void *)pointer;\n')
	assert !c_header_text_needs_objective_c('#define __strong\n#define __weak\n#define __autoreleasing\n#define __unsafe_unretained\n#define __kindof\n__strong void *strong_value;\n__weak void *weak_value;\n__autoreleasing void **out_value;\n__unsafe_unretained void *unsafe_value;\n__kindof void *kind_value;\n')
	assert !c_header_text_needs_objective_c_for_target('__strong void *value;\n', [
		'-D__strong',
	], false, pref.host_target())
	assert c_header_text_needs_objective_c('#define __bridge\n#undef __bridge\nid value = (__bridge id)pointer;\n')
	assert c_header_text_needs_objective_c('#define __bridge(value) value\nid value = (__bridge id)pointer;\n')
	assert !c_header_text_needs_objective_c('#include <CoreFoundation/CFString.h>\n')
	assert !c_header_text_needs_objective_c('#include <X11/Xlib.h>\n')
	assert !c_header_text_needs_objective_c('// @interface CommentOnly\n/* @implementation CommentOnly\nid value = (__bridge id)pointer;\n#import <Cocoa/Cocoa.h>\n*/\nconst char *description = "@interface string only";\nconst char *cast = "__bridge";\n')
	assert !c_header_text_needs_objective_c('#if 0\n@interface Disabled\n@end\nid value = (__bridge id)pointer;\n#import <Cocoa/Cocoa.h>\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0 // disabled\n@interface DisabledByLineComment\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0 /* disabled */\n@interface DisabledByBlockComment\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0L\n@interface DisabledByLongZero\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0x0ULL\n@interface DisabledByHexZero\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c("#if '\\0'\n@interface DisabledByNullCharacter\n@end\n#endif\n")
	assert c_header_text_needs_objective_c("#if 'A'\n@interface EnabledByCharacter\n@end\n#endif\n")
	assert !c_header_text_needs_objective_c("#if 'a' - 'a'\n@interface DisabledByCharacterArithmetic\n@end\n#endif\n")
	assert !c_header_text_needs_objective_c('#if 0 == 1\nstatic inline id disabled(id obj) { return [obj description]; }\n#endif\n')
	assert c_header_text_needs_objective_c('#if 2 >= 1\nstatic inline id enabled(id obj) { return [obj description]; }\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 1 - 1\nstatic inline id disabled(id obj) { return [obj description]; }\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 2 * 3 - 6\nstatic inline id disabled(id obj) { return [obj description]; }\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0 ? 1 : 0\n@interface DisabledByConditional\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 1 || 0 ? 0 : 0\n@interface DisabledByConditionalPrecedence\n@end\n#endif\n')
	assert c_header_text_needs_objective_c('#if 0 ? 0 : 1\n@interface EnabledByConditional\n@end\n#endif\n')
	assert c_header_text_needs_objective_c('#if (5 - 1) / 2\nstatic inline id enabled(id obj) { return [obj description]; }\n#endif\n')
	assert !c_header_text_needs_objective_c('#define ONE 1\n#if ONE - 1\n@class DisabledByMacroArithmetic;\n#endif\n')
	assert !c_header_text_needs_objective_c('#define FOO 1\n#if defined(FOO) - 1\n@class DisabledByDefinedArithmetic;\n#endif\n')
	assert c_header_text_needs_objective_c('#if 2 + 2 == 4\n@class EnabledByArithmeticComparison;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if (1 & 0)\n@class DisabledByBitwiseAnd;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if (0 | 0)\n@class DisabledByBitwiseOr;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if (1 ^ 1)\n@class DisabledByBitwiseXor;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if (1 << 3) - 8\n@class DisabledByLeftShift;\n#endif\n')
	assert c_header_text_needs_objective_c('#if (8 >> 2) == 2\n@class EnabledByRightShift;\n#endif\n')
	assert !c_header_text_needs_objective_c('#if 0 & 1 == 0\n@class DisabledByBitwiseComparisonPrecedence;\n#endif\n')
	assert c_header_text_needs_objective_c('#if 1 // enabled\n@interface EnabledByLineComment\n@end\n#endif\n')
	assert c_header_text_needs_objective_c('#if 1UL\n@interface EnabledByLongOne\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#ifdef __OBJC__\n@implementation Disabled\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if defined(__OBJC__) && __has_feature(objc_arc)\n@implementation Disabled\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if __has_feature(objc_arc) && defined(__OBJC__)\n@implementation Disabled\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c('#if defined(FEATURE) && \\\ndefined(__OBJC__)\n@interface DisabledByContinuedGuard\n@end\n#endif\n')
	assert !c_header_text_needs_objective_c_for_target('#if FEATURE\n@interface Disabled\n@end\n#endif\n', [
		'-DFEATURE=0',
	], false, pref.host_target())
	assert c_header_text_needs_objective_c_for_target('#if FEATURE\n@interface Enabled\n@end\n#endif\n', [
		'-DFEATURE=1',
	], false, pref.host_target())
	assert !c_header_text_needs_objective_c_for_target('#if FEATURE\n@class DisabledByChainedFlag;\n#endif\n', [
		'-DOFF=0',
		'-DFEATURE=OFF',
	], false, pref.host_target())
	assert !c_header_text_needs_objective_c('#define OFF 0\n#define FEATURE OFF\n#if FEATURE\n@protocol DisabledByChainedDirective;\n#endif\n')
	assert c_header_text_needs_objective_c('#define ON 1\n#define FEATURE ON\n#if FEATURE\n@class EnabledByChainedDirective;\n#endif\n')
	assert c_header_text_needs_objective_c('#define FIRST SECOND\n#define SECOND FIRST\n#if FIRST\n@class ConservativelyEnabledByMacroCycle;\n#endif\n')
	assert !c_header_text_needs_objective_c_for_target('#if !defined(FEATURE)\n@interface Disabled\n@end\n#endif\n', [
		'-DFEATURE(x)=x',
	], false, pref.host_target())
	assert c_header_text_needs_objective_c('#if 0\n@interface Disabled\n@end\n#else\n@interface Enabled\n@end\n#endif\n')
	assert c_header_text_needs_objective_c('#ifdef COMPILER_MACRO\n@interface PossiblyEnabled\n@end\n#endif\n')
	imports :=
		c_header_objective_c_framework_imports('#ifdef _WIN32\n#include <windows.h>\n#endif\n#import <Cocoa/Cocoa.h>\n#include <QuartzCore/QuartzCore.h>\n')
	assert imports == '#import <Cocoa/Cocoa.h>\n#include <QuartzCore/QuartzCore.h>'
	guarded :=
		c_header_objective_c_framework_imports('#ifdef __APPLE__\n#include <Cocoa/Cocoa.h>\n#else\n#include <X11/Xlib.h>\n#endif\n')
	assert guarded == '#ifdef __APPLE__\n#include <Cocoa/Cocoa.h>\n#endif'
}

fn test_large_transitive_header_tree_is_preserved() {
	root := os.join_path(os.temp_dir(), 'v3_large_transitive_header_tree_test')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	padding := 'x'.repeat(140_000)
	os.write_file(os.join_path(root, 'a.h'), '/*${padding}*/\n') or { panic(err) }
	os.write_file(os.join_path(root, 'b.h'), '/*${padding}*/\n') or { panic(err) }
	one_path := os.join_path(root, 'one.h')
	two_path := os.join_path(root, 'two.h')
	os.write_file(one_path, '#include "a.h"\n') or { panic(err) }
	os.write_file(two_path, '#include "a.h"\n#include "b.h"\n') or { panic(err) }
	mut one_size := CHeaderTreeSize{}
	assert !c_header_tree_exceeds_inline_limit(one_path, '', []string{}, mut one_size)
	mut two_size := CHeaderTreeSize{}
	assert c_header_tree_exceeds_inline_limit(two_path, '', []string{}, mut two_size)
}
