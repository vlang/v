module modulecache

import os
import crypto.sha256

fn test_cached_relative_flag_paths_preserve_path_selection_expressions() {
	base_dir := os.join_path(os.vtmp_dir(), 'v3_modulecache_flags')
	value := r"darwin -I$when_first_existing('/opt/local/include','/opt/homebrew/include') -L$first_existing('/opt/local/lib','/opt/homebrew/lib')"
	assert cached_resolve_relative_flag_paths(value, os.join_path(base_dir, 'source.v')) == value
}

fn test_cached_dependency_inputs_restore_empty_variable_value() {
	expected_head := 'format=test\n'
	stamp := expected_head + 'dependency=fixed\tvalue\n' + 'dependency=external-root-owner:c:0\t\n'
	restored := cached_dependency_inputs_from_stamp(stamp, expected_head, {
		'fixed': 'value'
	}, ['external-root-owner:']) or { panic('expected cache dependency restore') }
	assert 'external-root-owner:c:0' in restored
	assert restored['external-root-owner:c:0'] == ''
}

fn test_native_declaration_api_macro_definition_is_not_localized() {
	source := '#ifdef LIB_IMPL\nLIB_API_DECL void exported(void) {\n}\n#else\nLIB_API_DECL void exported(void);\n#endif\nint helper(void) {\n\treturn 1;\n}\n'
	declarations := c_native_declaration_directives(source)
	assert declarations.contains('LIB_API_DECL void exported(void) {')
	assert !declarations.contains('static LIB_API_DECL void exported(void) {')
	assert declarations.contains('static int helper(void) {')
}

fn test_cached_file_line_uses_source_file_name() {
	source := 'return [@FILE, @FILE_LINE, @LINE]'
	source_file := os.join_path(os.vtmp_dir(), 'nested', 'origin.v')
	rewritten := cached_embedded_source_paths(source, '', source_file, 5)
	assert rewritten == "return ['${os.real_path(source_file)}', 'origin.v:5', '5']"
}

fn test_without_duplicate_static_string_definitions_keeps_new_literals() {
	existing := '#include <Cocoa/Cocoa.h>
static string _v3_lit_1_44bd55d473cd3ef7 = {".", 1, 1};
static inline int native_value(void) { return 42; }
'
	source := 'static string _v3_lit_1_44bd55d473cd3ef7 = {".", 1, 1};
static string _v3_lit_1_44bd54d473cd3d44 = {"/", 1, 1};
'
	cleaned := without_duplicate_static_string_definitions(source, existing)
	assert !cleaned.contains('_v3_lit_1_44bd55d473cd3ef7')
	assert cleaned.contains('_v3_lit_1_44bd54d473cd3d44')
}

fn test_type_declarations_omit_functions_with_local_typedefs() {
	source := 'typedef struct VisibleType {
	int value;
} VisibleType;
static int local_state = 7;
static int local_static_function(void) {
	typedef struct LocalStaticType {
		int value;
	} LocalStaticType;
	LocalStaticType value = {local_state};
	return value.value;
}
inline int local_inline_function(void) {
	typedef int LocalInlineType;
	return (LocalInlineType)local_state;
}
'
	types := c_source_type_declarations(source)
	assert types.contains('VisibleType')
	assert !types.contains('local_static_function')
	assert !types.contains('LocalStaticType')
	assert !types.contains('local_inline_function')
	assert !types.contains('LocalInlineType')
	assert !types.contains('local_state')
}

fn test_type_declarations_keep_type_macro_invocations() {
	source := '#define DECLARE_TYPE(name) typedef struct { int value; } name
DECLARE_TYPE(Item);
'
	types, complete := c_source_type_declarations_with_status(source)
	assert complete
	assert types.contains('#define DECLARE_TYPE')
	assert types.contains('DECLARE_TYPE(Item);')

	object_source := '#define DECLARE_ITEM typedef int Item
DECLARE_ITEM;
'
	object_types, object_complete := c_source_type_declarations_with_status(object_source)
	assert object_complete
	assert object_types.contains('#define DECLARE_ITEM')
	assert object_types.contains('DECLARE_ITEM;')

	_, unknown_complete := c_source_type_declarations_with_status('UNKNOWN_DECL(Item);\n')
	assert !unknown_complete
	_, unknown_object_complete := c_source_type_declarations_with_status('UNKNOWN_DECL;\n')
	assert !unknown_object_complete
}

fn test_source_typedef_identifiers_ignore_comments_and_parse_declarators() {
	source := '// typedef unsigned CommentOnly;\n#define TYPE_MACRO typedef unsigned MacroOnly\n#define IGNORE(...)\nIGNORE(typedef unsigned MacroArgument);\nconst char *text = "typedef unsigned StringOnly";\ntypedef unsigned id; static inline id identity(id value) { return value; }\ntypedef void *Class;\ntypedef void (*SEL)(void);\ntypedef int Protocol(void);\nstatic inline void helper(void) { typedef unsigned LocalOnly; }\nextern "C" { typedef unsigned External; }\n'
	identifiers := c_source_typedef_identifiers(source)
	assert identifiers['id']
	assert identifiers['Class']
	assert identifiers['SEL']
	assert identifiers['Protocol']
	assert identifiers['External']
	assert !identifiers['CommentOnly']
	assert !identifiers['LocalOnly']
	assert !identifiers['MacroOnly']
	assert !identifiers['MacroArgument']
	assert !identifiers['StringOnly']
}

fn test_source_typedef_identifiers_resume_after_macro_decorated_function() {
	source := 'SOKOL_API_IMPL void draw(void) { if (1) { while (0) {} } }\ntypedef unsigned AfterBody;\n'
	identifiers := c_source_typedef_identifiers(source)
	assert identifiers['AfterBody']
}

fn test_static_variable_identifiers_ignore_asm_labels() {
	assert c_static_variable_declaration_identifiers('static int state __asm__("state_alias");') == [
		'state',
	]
	identifiers, complete :=
		c_source_static_variable_identifiers('static int state __asm__("state_alias");\n')
	assert complete
	assert identifiers['state'], identifiers.str()
	assert !identifiers['state_alias']
	function_identifiers, function_complete :=
		c_source_static_variable_identifiers('static int helper(void) __asm__("helper_alias");\n')
	assert function_complete
	assert !function_identifiers['helper']
	assert !function_identifiers['helper_alias']
}

fn test_static_storage_detects_macro_generated_declarations() {
	assert c_source_has_static_storage('#define DECL(name) static int name;\nDECL(shared_state)\n')
	assert c_source_has_static_storage('#define STORAGE static\n#define DECL(name) STORAGE int name;\nDECL(shared_state)\n')
	assert c_source_has_static_storage('#define LOCAL_FN(name) static int name(void)\nLOCAL_FN(helper) { return 1; }\n')
	assert !c_source_has_static_storage('#define DECL(name) int name;\nDECL(shared_state)\n')
}

fn test_static_variable_identifiers_classify_attributes() {
	identifiers, complete := c_source_static_variable_identifiers('__attribute__((availability(macos,introduced=14.0))) static const unsigned long DynamicStride = 42;
static inline __attribute__((__always_inline__)) __attribute__((__overloadable__)) int simd_any(int value);
')
	assert complete
	assert identifiers.keys() == ['DynamicStride']
}

fn test_static_variable_identifiers_ignore_preprocessor_directives() {
	identifiers, complete := c_source_static_variable_identifiers('/* declaration guard */
#if defined(ENABLE_STATE) \\
	&& !defined(DISABLE_STATE)
static int state;
#endif
')
	assert complete
	assert identifiers.keys() == ['state']
}

fn test_static_variable_identifiers_ignore_objc_declarations() {
	identifiers, complete := c_source_static_variable_identifiers('@interface CacheDelegate : NSObject
- (void)finish:(int)value;
@end
@protocol ForwardDeclaration;
static int state;
')
	assert complete
	assert identifiers.keys() == ['state']
}

fn test_static_variable_identifiers_track_objc_function_braces() {
	identifiers, complete := c_source_static_variable_identifiers('static void helper(void) {
	@autoreleasepool {
		static int local_state;
		if (local_state) {
			local_state++;
		}
	}
}
static int file_state;
')
	assert complete
	assert identifiers.keys() == ['file_state']
}

fn test_static_variable_identifiers_keep_anonymous_aggregate_declarator() {
	identifiers, complete := c_source_static_variable_identifiers('static struct {
	const char *str;
	int code;
} keymap[] = {
	{"Enter", 1},
};
')
	assert complete
	assert identifiers.keys() == ['keymap']
}

fn test_static_variable_identifiers_scan_extern_c_block() {
	identifiers, complete := c_source_static_variable_identifiers('extern "C" {
static int state;
}
')
	assert complete
	assert identifiers.keys() == ['state']
}

fn test_function_identifiers_keep_name_before_suffix_macro() {
	identifiers, complete :=
		c_source_function_identifiers_with_status('#define API_SUFFIX(tag)\nstatic int api(void) API_SUFFIX(tag) {\n\treturn 1;\n}\n')
	assert complete
	assert identifiers['api']
	assert !identifiers['API_SUFFIX']
}

fn test_static_function_identifiers_exclude_exported_functions() {
	identifiers, complete :=
		c_source_static_function_identifiers_with_status('static int local_helper(void) { return 1; }\nint exported_helper(void) { return local_helper(); }\n')
	assert complete
	assert identifiers['local_helper']
	assert !identifiers['exported_helper']
}

fn test_function_identifiers_keep_name_after_return_type_macro() {
	identifiers, complete :=
		c_source_function_identifiers_with_status('#define RET(T) T\nRET(int) api(void) {\n\treturn 1;\n}\n')
	assert complete
	assert identifiers['api']
	assert !identifiers['RET']
}

fn test_function_identifiers_keep_name_before_parameter_list_macro() {
	identifiers, complete :=
		c_source_function_identifiers_with_status('#define P_(x) x\nstatic int api P_((void)) {\n\treturn 1;\n}\n')
	assert complete
	assert identifiers['api']
	assert !identifiers['P_']
	single_identifiers, single_complete :=
		c_source_function_identifiers_with_status('#define P(x) (x)\nstatic int api P(void) {\n\treturn 1;\n}\n')
	assert single_complete
	assert single_identifiers['api']
	assert !single_identifiers['P']
	old_style_identifiers, old_style_complete :=
		c_source_function_identifiers_with_status('#define EXPORT\ntypedef int MyType;\nEXPORT MyType API(foo)\nint foo;\n{\n\treturn foo;\n}\n')
	assert old_style_complete
	assert old_style_identifiers['API']
	assert !old_style_identifiers['MyType']
}

fn test_function_identifiers_unwrap_parenthesized_declarator() {
	identifiers, complete :=
		c_source_function_identifiers_with_status('static int (api)(void) {\n\treturn 1;\n}\n')
	assert complete
	assert identifiers['api']
	assert !identifiers['int']
	nested_identifiers, nested_complete :=
		c_source_function_identifiers_with_status('static int ((api))(void) {\n\treturn 1;\n}\n')
	assert nested_complete
	assert nested_identifiers['api']
	assert !nested_identifiers['int']
	attributed_identifiers, attributed_complete :=
		c_source_function_identifiers_with_status('static int (__attribute__((noinline)) api)(void) {\n\treturn 1;\n}\n')
	assert attributed_complete
	assert attributed_identifiers['api']
	assert !attributed_identifiers['int']
}

fn test_function_identifiers_recognize_function_pointer_return() {
	identifiers, complete :=
		c_source_function_identifiers_with_status('static int (*api(void))(int) {\n\treturn 0;\n}\n')
	assert complete
	assert identifiers['api']
	assert !identifiers['int']
	assert c_static_declaration_head_is_function('static int (*api(void))(int)')
	assert !c_static_declaration_head_is_function('static int (*callback)(int)')
	redundant_identifiers, redundant_complete :=
		c_source_function_identifiers_with_status('static int (*((api))(void))(int) {\n\treturn 0;\n}\n')
	assert redundant_complete
	assert redundant_identifiers['api']
	assert !redundant_identifiers['int']
	assert c_static_declaration_head_is_function('static int (*((api))(void))(int)')
}

fn test_function_identifiers_preserve_old_style_parameter_declarations() {
	identifiers, complete :=
		c_source_function_identifiers_with_status('static int api(a)\nint a;\n{\n\treturn a;\n}\n')
	assert complete
	assert identifiers['api']
}

fn test_macro_identifiers_referencing_static_helpers() {
	wrappers := c_sources_macro_identifiers_referencing([
		'#define CALL_HELPER() helper()
#define CALL_OUTER() CALL_HELPER()
#define COMMENT_ONLY() /* helper() */
#define STRING_ONLY() "helper"
',
		'#define CROSS_FILE() CALL_OUTER()',
	], {
		'helper': true
	})
	assert wrappers['CALL_HELPER']
	assert wrappers['CALL_OUTER']
	assert wrappers['CROSS_FILE']
	assert !wrappers['COMMENT_ONLY']
	assert !wrappers['STRING_ONLY']
}

fn test_source_signature_cache_content_requires_stable_metadata() {
	expected_digest := 'a'.repeat(sha256.size * 2)
	details := SourceSignatureDetails{
		signature:      'content-signature'
		validation:     ['env=NAME\tvalue']
		source_digests: [expected_digest]
	}
	if _ := source_signature_cache_content('before', 'after', details) {
		assert false, 'changed metadata must prevent source signature caching'
	}
	if _ := source_signature_cache_content('', '', details) {
		assert false, 'missing metadata must prevent source signature caching'
	}

	content := source_signature_cache_content('stable', 'stable', details) or {
		assert false, 'stable metadata should allow source signature caching'
		return
	}
	assert content.contains('metadata=stable\n')
	assert content.contains('digest=${expected_digest}\n')
	assert content.contains('source=content-signature\n')
	assert content.ends_with('complete=1\n')
}

fn test_cached_source_signature_keeps_per_file_sha256_digests() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_source_digests_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	first_path := os.join_path(root, 'first.v')
	second_path := os.join_path(root, 'second.v')
	first_source := 'module sample\n\npub fn first() {}\n'
	second_source := 'module sample\n\npub fn second() {}\n'
	os.write_file(first_path, first_source)!
	os.write_file(second_path, second_source)!
	cache_dir := os.join_path(root, 'cache')
	details := cached_source_signature_details_with_build_values(cache_dir, 'digests', [
		second_path,
		first_path,
	], '', '')
	assert details.signature.len > 0
	assert details.source_digests == [sha256.hexhash(first_source),
		sha256.hexhash(second_source)]
	// The metadata-valid fast path must restore the same per-file digests without
	// dropping them from the cache validity result.
	cached := cached_source_signature_details_with_build_values(cache_dir, 'digests', [
		second_path,
		first_path,
	], '', '')
	assert cached.signature == details.signature
	assert cached.source_digests == details.source_digests
}

fn test_version_pseudo_signature_ignores_build_clock() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_version_pseudo_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'version.v')
	os.write_file(source, 'module version\n\nconst current = @VCURRENTHASH\n') or { panic(err) }

	first := source_signature_details([source], 'build-clock-1', 'version-1')
	second := source_signature_details([source], 'build-clock-2', 'version-1')
	changed := source_signature_details([source], 'build-clock-2', 'version-2')
	assert first.signature == second.signature
	assert first.signature != changed.signature
	assert first.validation.any(it.starts_with('version='))
	assert !first.validation.any(it.starts_with('build='))
}

fn test_source_uses_pseudo_in_quoted_compile_time_paths() {
	roots := ['@VMODROOT', '@VMOD_FILE', '@VROOT']
	assert source_uses_pseudo("module m\n\nconst data = \$embed_file('@VMODROOT/data.bin')", roots)
	assert source_uses_pseudo('module m\n\n#include "@VMODROOT/header.h"', roots)
	assert source_uses_pseudo('module m\n\n#flag -I "@VMODROOT/include"', roots)
	assert source_uses_pseudo('module m\n\nconst p = \$embed_file(r"@VROOT/x")', roots)
	// a pseudo after a string containing `//` must still be seen
	assert source_uses_pseudo("module m\n\nconst u = 'http://x' + \$embed_file('@VMODROOT/y')",
		roots)
	// comments stay inert
	assert !source_uses_pseudo('module m\n\n// mentions @VMODROOT only in a comment', roots)
	assert !source_uses_pseudo("module m\n\nconst s = 'plain text'", roots)
	assert !source_uses_pseudo("module m\n\nconst s = '@VMODROOT/inert'", roots)
	assert !source_uses_pseudo('module m\n\nconst s = r"@VROOT/inert"', roots)
	assert !source_uses_pseudo('module m\n\n#define MARKER "@VMODROOT/inert"', roots)
	assert !source_uses_pseudo("module m\n\n#define X /*\n@VMODROOT\n*/\nconst s = 'inert'", roots)
	// name-boundary check still applies inside literals
	assert !source_uses_pseudo("module m\n\nconst s = \$embed_file('@VROOTX/not-a-pseudo')", roots)

	build := ['@BUILD_TIMESTAMP', '@BUILD_DATE', '@BUILD_TIME', '@VHASH', '@VCURRENTHASH']
	assert !source_uses_pseudo("module m\n\npub const marker = '@BUILD_DATE'", build)
	assert source_uses_pseudo('module m\n\npub const marker = @BUILD_DATE', build)
	assert source_uses_pseudo('module m\n\npub const build_hash = @VHASH', build)
	assert source_uses_pseudo('module m\n\npub const current_hash = @VCURRENTHASH', build)
	assert source_uses_pseudo(r"module m\n\npub const stamp = 'built ${@BUILD_TIMESTAMP}'", build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = 'literal @BUILD_TIMESTAMP ${1}'",
		build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = 'built \${@BUILD_TIMESTAMP}'", build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = r'built ${@BUILD_TIMESTAMP}'", build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = 'built ${/* @BUILD_TIMESTAMP */ 1}'",
		build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = 'built ${'@BUILD_TIMESTAMP'}'",
		build)
	assert source_uses_pseudo(r"module m\n\npub const stamp = 'built ${if ok { @BUILD_TIMESTAMP } else { 0 }}'",
		build)
	assert source_uses_pseudo(r"module m\n\npub const root = 'root ${@VMODROOT}'", roots)
}

fn test_vmodhash_changes_cached_source_signature_without_source_edits() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_vmodhash_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, '.git', 'refs', 'heads')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'cache_vmodhash' }\n")!
	os.write_file(os.join_path(root, '.git', 'HEAD'), 'ref: refs/heads/main\n')!
	ref_file := os.join_path(root, '.git', 'refs', 'heads', 'main')
	os.write_file(ref_file, '0123456789abcdef0123456789abcdef01234567\n')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main\n\nconst project_hash = @VMODHASH\n')!
	cache_dir := os.join_path(root, 'cache')

	first := cached_source_signature(cache_dir, 'vmodhash', [source])
	assert first.len > 0
	details := source_signature_details([source], '', '')
	assert details.validation.any(it.starts_with('vmodhash='))

	os.write_file(ref_file, 'abcdef0123456789abcdef0123456789abcdef01\n')!
	second := cached_source_signature(cache_dir, 'vmodhash', [source])
	assert second.len > 0
	assert second != first
}
