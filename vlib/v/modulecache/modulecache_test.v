module modulecache

import os
import time
import crypto.sha256
import v.flat
import v.parser
import v.pref
import v.types as vtypes

fn test_cached_vmod_roots_stop_at_project_boundaries() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_vmod_boundary_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer {
		os.rmdir_all(root) or {}
	}
	parent := os.join_path(root, 'parent')
	project := os.join_path(parent, 'project')
	source_dir := os.join_path(project, 'src')
	os.mkdir_all(source_dir)!
	parent_vmod := os.join_path(parent, 'v.mod')
	os.write_file(parent_vmod, "Module { name: 'parent' }\n")!
	source_file := os.join_path(source_dir, 'main.v')
	os.write_file(source_file, 'module main\n')!
	root_before, file_before := signature_vmod_root(source_file)
	assert root_before == os.real_path(parent)
	assert file_before == os.real_path(parent_vmod)
	assert cached_vmod_root(source_file) == os.real_path(parent)

	for marker in [pref.module_search_stop_marker, '.git', '.hg', '.svn'] {
		marker_path := os.join_path(project, marker)
		os.write_file(marker_path, '')!
		bounded_root, bounded_file := signature_vmod_root(source_file)
		assert bounded_root == os.real_path(source_dir)
		assert bounded_file == ''
		assert cached_vmod_root(source_file) == os.real_path(source_dir)
		os.rm(marker_path)!
	}
}

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

fn test_declaration_header_keeps_macro_static_inline_function() {
	source := '#ifdef _MSC_VER
#define V_TEST_STATIC_INLINE static __inline
#else
#define V_TEST_STATIC_INLINE static inline
#endif
V_TEST_STATIC_INLINE int local_helper(void) {
	return 42;
}
int external_helper(void) {
	return local_helper();
}
'
	header := declaration_header(source)
	assert header.contains('V_TEST_STATIC_INLINE int local_helper(void) {')
	assert header.contains('return 42;')
	assert header.contains('int external_helper(void);')
	assert !header.contains('return local_helper();')
}

fn test_replicated_function_static_storage_detection() {
	assert c_source_replicated_function_has_static_storage('static inline int next_value(void) {
	static int state = 0;
	return ++state;
}
')
	assert c_source_replicated_function_has_static_storage('#define LOCAL_STORAGE static
static inline int next_value(void) {
	LOCAL_STORAGE int state = 0;
	return ++state;
}
')
	assert c_source_replicated_function_has_static_storage('extern "C" { static inline int next_value(void) { static int state; return ++state; } }
')
	assert c_source_replicated_function_has_static_storage('#define DEF(name) \\
	static inline int name(void) { \\
		static int state; \\
		return ++state; \\
	}
DEF(next_value)
')
	assert !c_source_replicated_function_has_static_storage('int next_value(void) {
	static int state = 0;
	return ++state;
}
')
	assert !c_source_replicated_function_has_static_storage('static inline int next_value(void) {
	// static int comment_state;
	const char *text = "static int string_state";
	return text[0];
}
')
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
	assert details.source_digests == [sha256.hexhash(first_source), sha256.hexhash(second_source)]
	// The metadata-valid fast path must restore the same per-file digests without
	// dropping them from the cache validity result.
	cached := cached_source_signature_details_with_build_values(cache_dir, 'digests', [
		second_path,
		first_path,
	], '', '')
	assert cached.signature == details.signature
	assert cached.source_digests == details.source_digests
}

fn test_cached_source_signature_tracks_vml_inputs() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_vml_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	vml := os.join_path(root, 'form.vml')
	os.write_file(source, "module main\n\nfn build() { _ = \$vml('form.vml') }\n")!
	os.write_file(vml, 'Label { text: "A" }')!
	cache_dir := os.join_path(root, 'cache')

	first := cached_source_signature(cache_dir, 'vml', [source])
	assert first.len > 0
	assert source_signature_details([source], '', '').cacheable
	os.write_file(vml, 'Label { text: "Changed" }')!
	second := cached_source_signature(cache_dir, 'vml', [source])
	assert second.len > 0
	assert second != first
	ignored_paths, ignored_lookups, ignored_candidates, ignored_unresolved := compile_time_vml_paths('// \$vml(\'ignored.vml\')\nconst s = "\$vml(\'also_ignored.vml\')"', source)
	assert ignored_paths.len == 0
	assert ignored_lookups.len == 0
	assert ignored_candidates.len == 0
	assert !ignored_unresolved

	os.write_file(source, "module main\n\nconst form_path = 'form.vml'\nfn build() { _ = \$vml(form_path) }\n")!
	before_cache_entries := os.ls(cache_dir)!.len
	dynamic := cached_source_signature_details_with_build_values(cache_dir, 'dynamic-vml', [
		source,
	], '', '')
	assert dynamic.signature.len > 0
	assert !dynamic.cacheable
	assert os.ls(cache_dir)!.len == before_cache_entries
	dynamic_paths, dynamic_lookups, dynamic_candidates, dynamic_unresolved := compile_time_vml_paths(os.read_file(source)!, source)
	assert dynamic_paths.len == 0
	assert dynamic_lookups.len == 0
	assert dynamic_candidates.len == 0
	assert dynamic_unresolved
	concat_paths, concat_lookups, concat_candidates, concat_unresolved := compile_time_vml_paths("fn build() { _ = \$vml(template_dir + '/form.vml') }", source)
	assert concat_paths.len == 0
	assert concat_lookups.len == 0
	assert concat_candidates.len == 0
	assert concat_unresolved
	os.write_file(os.join_path(root, 'form'), 'not the selected template')!
	os.write_file(source, "module main\n\nfn build() { _ = \$vml('form' + '.vml') }\n")!
	literal_concat := source_signature_details([source], '', '')
	assert literal_concat.signature.len > 0
	assert !literal_concat.cacheable
	literal_paths, literal_lookups, literal_candidates, literal_unresolved := compile_time_vml_paths(os.read_file(source)!, source)
	assert literal_paths.len == 0
	assert literal_lookups.len == 0
	assert literal_candidates.len == 0
	assert literal_unresolved
	manager := Manager{
		dir:     os.join_path(root, 'module-cache')
		enabled: true
		salt:    'dynamic-vml-test'
	}
	manager.write_header('dynamic_vml', [source], '// generated header')!
	if _ := manager.valid_header('dynamic_vml', [source]) {
		assert false, 'an unresolved compile-time VML path must disable cache reuse'
	}
}

fn test_vml_signature_scanner_preserves_raw_paths() {
	call := r'$' + r"vml(r'C:\views\form.vml')"
	raw_path, next_pos, ok, is_raw := signature_string_call_arg(call, 4)
	assert ok
	assert is_raw
	assert next_pos == call.len
	assert raw_path == r'C:\views\form.vml'

	target, expected_candidates := resolve_signature_vml_path(raw_path, @FILE)
	paths, lookups, candidates, unresolved := compile_time_vml_paths(call, @FILE)
	assert paths == [os.real_path(target)]
	assert lookups == [target]
	assert candidates == expected_candidates.map(os.real_path(it))
	assert !unresolved

	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_vml_raw_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	relative_call := r'$' + r"vml(r'views\form.vml')"
	os.write_file(source, 'module main\n\nfn build() { _ = ' + relative_call + ' }\n')!
	vml_path, _ := resolve_signature_vml_path(r'views\form.vml', source)
	os.mkdir_all(os.dir(vml_path))!
	os.write_file(vml_path, 'Label { text: "Raw" }')!
	cache_dir := os.join_path(root, 'cache')
	first := cached_source_signature(cache_dir, 'vml-raw', [source])
	assert first.len > 0
	assert source_signature_details([source], '', '').cacheable
	assert cached_source_signature(cache_dir, 'vml-raw', [source]) == first
}

fn test_cached_source_signature_tracks_shadowing_vml_paths() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_vml_shadow_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'templates')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	direct_vml := os.join_path(root, 'form.vml')
	direct_candidate := os.join_path(os.real_path(root), 'form.vml')
	template_vml := os.join_path(root, 'templates', 'form.vml')
	os.write_file(source, "module main\n\nfn build() { _ = \$vml('form.vml') }\n")!
	os.write_file(template_vml, 'Label { text: "Template" }')!
	cache_dir := os.join_path(root, 'cache')

	first := cached_source_signature(cache_dir, 'vml-shadow', [source])
	assert first.len > 0
	paths, lookups, candidates, unresolved := compile_time_vml_paths(os.read_file(source)!, source)
	assert paths == [os.real_path(template_vml)]
	assert lookups == [os.join_path(os.real_path(root), 'templates', 'form.vml')]
	assert candidates == [direct_candidate]
	assert !unresolved
	details := source_signature_details([source], '', '')
	assert details.validation.any(it == 'vmlcandidate=${direct_candidate}\tmissing')

	os.write_file(direct_vml, 'Label { text: "Direct" }')!
	second := cached_source_signature(cache_dir, 'vml-shadow', [source])
	assert second.len > 0
	assert second != first
	shadowing_paths, shadowing_lookups, shadowing_candidates, _ := compile_time_vml_paths(os.read_file(source)!, source)
	assert shadowing_paths == [os.real_path(direct_vml)]
	assert shadowing_lookups == [direct_candidate]
	assert shadowing_candidates.len == 0
}

fn test_cached_source_signature_tracks_vml_symlink_target() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_vml_symlink_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	first_target := os.join_path(root, 'first.vml')
	second_target := os.join_path(root, 'second.vml')
	vml_link := os.join_path(root, 'form.vml')
	os.write_file(source, "module main\n\nfn build() { _ = \$vml('form.vml') }\n")!
	os.write_file(first_target, 'Label { text: "First" }')!
	os.write_file(second_target, 'Label { text: "Second" }')!
	os.symlink(first_target, vml_link) or { return }
	cache_dir := os.join_path(root, 'cache')

	first := cached_source_signature(cache_dir, 'vml-symlink', [source])
	assert first.len > 0
	lookup_path := os.join_path(os.real_path(root), 'form.vml')
	details := source_signature_details([source], '', '')
	assert details.validation.any(it.starts_with('vmllookup=${lookup_path}\t${os.real_path(first_target)}\t'))

	os.rm(vml_link)!
	os.symlink(second_target, vml_link)!
	second := cached_source_signature(cache_dir, 'vml-symlink', [source])
	assert second.len > 0
	assert second != first
}

// without_file_metadata makes file_metadata_signature report nothing for
// `paths`, as it does for a file without a usable identity, and returns the
// previous setting for restore_file_metadata.
fn without_file_metadata(paths []string) (string, bool) {
	was_set := 'V3_TEST_NO_FILE_METADATA' in os.environ()
	old := os.getenv('V3_TEST_NO_FILE_METADATA')
	os.setenv('V3_TEST_NO_FILE_METADATA', paths.join(os.path_delimiter), true)
	return old, was_set
}

fn restore_file_metadata(old string, was_set bool) {
	if was_set {
		os.setenv('V3_TEST_NO_FILE_METADATA', old, true)
	} else {
		os.unsetenv('V3_TEST_NO_FILE_METADATA')
	}
}

fn test_file_change_signature_falls_back_to_the_contents() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_change_signature_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	with_metadata := os.join_path(root, 'with_metadata.vml')
	without_metadata := os.join_path(root, 'without_metadata.vml')
	absent := os.join_path(root, 'absent.vml')
	os.write_file(with_metadata, 'first')!
	os.write_file(without_metadata, 'first')!
	old, was_set := without_file_metadata([without_metadata, absent])
	defer {
		restore_file_metadata(old, was_set)
	}
	assert file_change_signature(with_metadata) == file_metadata_signature(with_metadata)
	assert file_metadata_signature(without_metadata) == ''
	first := file_change_signature(without_metadata)
	assert first.starts_with('content:')
	assert file_change_signature_matches(without_metadata, first)
	// Same size, different bytes: only the contents tell the edit apart.
	os.write_file(without_metadata, 'other')!
	assert !file_change_signature_matches(without_metadata, first)
	assert file_change_signature(absent) == 'missing'
	os.write_file(absent, 'appeared')!
	assert !file_change_signature_matches(absent, 'missing')
}

// A source file on a file system with file identities can depend on inputs on one
// without them. Edits to those inputs, and inputs that appear there, must still
// invalidate the memoized source signature.
fn test_cached_source_signature_tracks_vml_inputs_without_file_metadata() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_vml_no_metadata_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'templates')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	template_vml := os.join_path(root, 'templates', 'form.vml')
	direct_vml := os.join_path(root, 'form.vml')
	os.write_file(source, "module main\n\nfn build() { _ = \$vml('form.vml') }\n")!
	os.write_file(template_vml, 'Label { text: "First" }')!
	old, was_set := without_file_metadata([template_vml, direct_vml])
	defer {
		restore_file_metadata(old, was_set)
	}
	assert file_metadata_signature(source) != ''
	assert file_metadata_signature(template_vml) == ''
	cache_dir := os.join_path(root, 'cache')

	first := cached_source_signature(cache_dir, 'vml-no-metadata', [source])
	assert first.len > 0
	assert source_signature_details([source], '', '').cacheable
	assert cached_source_signature(cache_dir, 'vml-no-metadata', [source]) == first

	os.write_file(template_vml, 'Label { text: "Other" }')!
	second := cached_source_signature(cache_dir, 'vml-no-metadata', [source])
	assert second.len > 0
	assert second != first

	os.write_file(direct_vml, 'Label { text: "Direct" }')!
	third := cached_source_signature(cache_dir, 'vml-no-metadata', [source])
	assert third.len > 0
	assert third != second
}

fn test_cached_source_signature_tracks_vmod_edits_without_file_metadata() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_vmod_no_metadata_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	vmod := os.join_path(root, 'v.mod')
	os.write_file(source, 'module main\n\nconst manifest = @VMOD_FILE\n')!
	os.write_file(vmod, "Module {\n\tname: 'first'\n}\n")!
	old, was_set := without_file_metadata([vmod])
	defer {
		restore_file_metadata(old, was_set)
	}
	cache_dir := os.join_path(root, 'cache')

	first := cached_source_signature(cache_dir, 'vmod-no-metadata', [source])
	assert first.len > 0
	assert cached_source_signature(cache_dir, 'vmod-no-metadata', [source]) == first

	os.write_file(vmod, "Module {\n\tname: 'other'\n}\n")!
	second := cached_source_signature(cache_dir, 'vmod-no-metadata', [source])
	assert second.len > 0
	assert second != first
}

// On FAT, exFAT and HFS+ a same-size edit within one timestamp step keeps the
// file's metadata identical. os.utime sets whole-second times, which reproduces
// that on any file system.
fn test_cached_source_signature_tracks_edits_within_a_coarse_timestamp_step() {
	root := os.join_path(os.vtmp_dir(), 'v3_modulecache_coarse_mtime_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	template_vml := os.join_path(root, 'form.vml')
	os.write_file(source, "module main\n\nfn build() { _ = \$vml('form.vml') }\n")!
	old_time := time.utc().unix() - 600
	os.utime(source, old_time, old_time)!
	assert file_metadata_signature(source) != ''
	step := time.utc().unix()
	os.write_file(template_vml, 'Label { text: "First" }')!
	os.utime(template_vml, step, step)!
	cache_dir := os.join_path(root, 'cache')

	first := cached_source_signature(cache_dir, 'coarse-mtime', [source])
	assert first.len > 0
	os.write_file(template_vml, 'Label { text: "Other" }')!
	os.utime(template_vml, step, step)!
	second := cached_source_signature(cache_dir, 'coarse-mtime', [source])
	assert second.len > 0
	assert second != first
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
	assert source_uses_pseudo("module m\n\nconst p = \$tmpl('@VMODROOT' + '/x.html')", roots)
	// a pseudo after a string containing `//` must still be seen
	assert source_uses_pseudo("module m\n\nconst u = 'http://x' + \$embed_file('@VMODROOT/y')", roots)
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
	assert !source_uses_pseudo(r"module m\n\npub const stamp = 'literal @BUILD_TIMESTAMP ${1}'", build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = 'built \${@BUILD_TIMESTAMP}'", build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = r'built ${@BUILD_TIMESTAMP}'", build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = 'built ${/* @BUILD_TIMESTAMP */ 1}'", build)
	assert !source_uses_pseudo(r"module m\n\npub const stamp = 'built ${'@BUILD_TIMESTAMP'}'", build)
	assert source_uses_pseudo(r"module m\n\npub const stamp = 'built ${if ok { @BUILD_TIMESTAMP } else { 0 }}'", build)
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

fn global_qualifier_test_field(mut a flat.FlatAst, name string, type_text string, value string, qualifiers []string) flat.NodeId {
	literal := a.add_node(flat.Node{
		kind:  .int_literal
		value: value
	})
	start := a.children.len
	a.children << literal
	return a.add_node(flat.Node{
		kind:           .field_decl
		value:          name
		typ:            type_text
		payload:        flat.node_payload(qualifiers)
		children_start: i32(start)
		children_count: flat.child_count(1)
	})
}

// A cached module's globals are written out as V source and parsed back, so a
// qualifier dropped on the way out is lost only for consumers of the cache --
// the worst way for it to differ, since an uncached build of the same source
// keeps it. This walks that round trip: global_text() serializes, and the
// parser reads the result back.
fn test_cached_global_text_round_trips_qualifiers() {
	mut a := flat.FlatAst.new()
	fields := [
		global_qualifier_test_field(mut a, 'beacon', 'u64', '7', ['volatile']),
		global_qualifier_test_field(mut a, 'plain_counter', 'u64', '0', []),
		global_qualifier_test_field(mut a, 'fixed_limit', 'u64', '8', ['const']),
	]
	start := a.children.len
	a.children << fields
	node_id := a.add_node(flat.Node{
		kind:           .global_decl
		children_start: i32(start)
		children_count: flat.child_count(fields.len)
	})
	mut tc := vtypes.TypeChecker.new(&a)
	text := global_text(&a, &tc, 'beaconmod', a.nodes[int(node_id)])

	assert text.contains('volatile beacon u64 = 7'), text
	assert text.contains('const fixed_limit u64 = 8'), text
	// The neighbours must not pick up a qualifier of their own.
	assert text.contains('\n\tplain_counter u64 = 0\n'), text
	assert !text.contains('volatile plain_counter'), text
	assert !text.contains('volatile fixed_limit'), text
	assert !text.contains('const beacon'), text

	// And the reparse half: what a warm build reads back has to carry the
	// qualifiers the original declaration did.
	path := os.join_path(os.vtmp_dir(), 'v3_cached_global_round_trip_${os.getpid()}.v')
	os.write_file(path, 'module beaconmod\n\n${text}\n') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	mut p := parser.Parser.new(prefs)
	reparsed := p.parse_file(path)
	mut seen := 0
	for node in reparsed.nodes {
		if node.kind != .global_decl {
			continue
		}
		for i in 0 .. node.children_count {
			field := reparsed.child_node(&node, i)
			qualifiers := field.generic_params()
			match field.value {
				'beacon' {
					assert 'volatile' in qualifiers, 'beacon lost volatile: ${qualifiers}'
					assert 'const' !in qualifiers, 'beacon gained const: ${qualifiers}'
					seen++
				}
				'fixed_limit' {
					assert 'const' in qualifiers, 'fixed_limit lost const: ${qualifiers}'
					assert 'volatile' !in qualifiers, 'fixed_limit gained volatile: ${qualifiers}'
					seen++
				}
				'plain_counter' {
					assert qualifiers.len == 0, 'plain_counter gained ${qualifiers}'
					seen++
				}
				else {}
			}
		}
	}
	assert seen == 3, 'the reparsed header did not describe all three globals (${seen})'
}

fn test_module_header_preserves_module_attributes() {
	mut a := flat.FlatAst.new()
	module_id := a.add_node(flat.Node{
		kind:  .module_decl
		value: 'guarded'
	})
	a.add_node(flat.Node{
		kind:    .directive
		value:   '@attributes:${int(module_id)}'
		payload: flat.node_payload(['has_globals'])
	})
	file_children := a.begin_children()
	a.add_child(module_id)
	a.add_node(flat.Node{
		kind:           .file
		value:          'guarded.v'
		children_start: file_children
		children_count: 1
	})
	tc := vtypes.TypeChecker.new(&a)
	header := module_header(&a, &tc, 'guarded', '', map[string]string{})
	assert header.starts_with('@[has_globals]\nmodule guarded\n'), header
}

// A module that imports another one is as much a matter of declarations as one
// that does not. The node of an import carries the path it was spelled with in the
// payload that holds the type parameters of a declaration, and reading that as
// "generic" marked the header of nearly every module as one that needs its
// sources, so a warm build parsed `builtin` and the rest of them again.
fn test_module_header_of_importing_module_needs_no_source_bodies() {
	root := os.join_path(os.vtmp_dir(), 'v3_header_imports_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'shapes.v')
	os.write_file(source, 'module shapes

import strings
import math.bits as b

pub struct Marker {}

pub struct Square {
pub:
	side int
}

pub fn (s Square) area() int {
	return s.side * s.side
}

pub fn describe(s Square) string {
	mut out := strings.new_builder(16)
	out.write_string(b.len_32(u32(s.area())).str())
	return out.str()
}
') or {
		panic(err)
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	tc := vtypes.TypeChecker.new(a)
	header := module_header(a, &tc, 'shapes', '', map[string]string{})
	assert !header.contains(source_body_marker), header
	assert header.contains('import strings'), header
	assert header.contains('pub fn describe(s Square) string\n'), header
	// A struct without fields keeps its body: `struct Marker` alone does not parse.
	assert header.contains('pub struct Marker {\n}'), header
	header_path := os.join_path(root, 'shapes.vh')
	os.write_file(header_path, header) or { panic(err) }
	mut header_parser := parser.Parser.new(pref.new_preferences())
	reparsed := header_parser.parse_file(header_path)
	assert header_parser.diagnostics.len == 0, header_parser.diagnostics.str()
	mut structs := []string{}
	for node in reparsed.nodes {
		if node.kind == .struct_decl {
			structs << node.value
		}
	}
	assert structs == ['Marker', 'Square']
}

// header_of_source returns the header of the module `name` whose only file has
// the text `source`.
fn header_of_source(root string, name string, source string) string {
	path := os.join_path(root, '${name}.v')
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	tc := vtypes.TypeChecker.new(a)
	return module_header(a, &tc, name, '', map[string]string{})
}

fn test_cached_constant_tables_are_typed_declarations() {
	root := os.join_path(os.vtmp_dir(), 'v3_cached_const_declarations_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	header := header_of_source(root, 'tables', 'module tables
pub const data = [u64(17), 29, 41]
pub const fixed = [u8(1), 2, 3]!
pub const width = 3
pub const cast_width = i64(3)
')
	assert header.contains('pub const data []u64'), header
	assert header.contains('pub const fixed [3]u8'), header
	assert header.contains('pub const width = 3'), header
	assert header.contains('pub const cast_width = i64(3)'), header
	assert !header.contains('29'), header
	path := os.join_path(root, 'tables.vh')
	os.write_file(path, header)!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := vtypes.TypeChecker.new(a)
	tc.collect(a)
	assert (tc.const_types['tables.data'] or { panic('missing data') }).name() == '[]u64'
	assert (tc.const_types['tables.fixed'] or { panic('missing fixed') }) is vtypes.ArrayFixed
	assert 'tables.data' !in tc.const_exprs
	assert 'tables.fixed' !in tc.const_exprs
	assert 'tables.cast_width' in tc.const_exprs
}

// A header declares the functions of a module and leaves out their code. A
// program that reaches a `recover()` in that code cannot tell from the header,
// and the object of the module was compiled before the program did, so the header
// says that the call is there.
fn test_module_header_says_that_the_code_of_the_module_calls_recover() {
	root := os.join_path(os.vtmp_dir(), 'v3_header_recover_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	direct := header_of_source(root, 'guarded', 'module guarded

pub fn checked(n int) int {
	defer {
		if msg := recover() {
			println(msg)
		}
	}
	if n > 2 {
		panic("too big")
	}
	return n
}
')
	assert header_text_calls_recover(direct), direct
	assert direct.contains('pub fn checked(n int) int\n'), direct
	assert !direct.contains('too big'), direct
	// The call can be in a function that the header does not even declare.
	indirect := header_of_source(root, 'wrapped', 'module wrapped

fn stop() bool {
	defer {
		recover() or {}
	}
	return true
}

pub fn run() bool {
	return stop()
}
')
	assert header_text_calls_recover(indirect), indirect
	// Nothing but a call counts: not the name of a field, nor a function that the
	// module declares under that name and does not call.
	plain := header_of_source(root, 'plain', 'module plain

pub struct State {
pub:
	recover bool
}

pub fn recover_later(s State) bool {
	return s.recover
}
')
	assert !header_text_calls_recover(plain), plain
	// A program that spells the marker in a string of its own is not a header that
	// has the line.
	assert !header_text_calls_recover("module quoted\n\npub const text = '${recover_call_marker}'\n")
}

// The stamp of a header answers for it in a build that does not read the header
// itself to decide how to parse the module.
fn test_cached_entry_keeps_the_recover_flag_of_its_header() {
	root := os.join_path(os.vtmp_dir(), 'v3_header_recover_entry_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	manager := Manager{
		dir:        os.join_path(root, 'cache')
		enabled:    true
		salt:       'recover'
		pkg_probes: &PkgConfigProbes{}
	}
	for name, calls in {
		'guarded': true
		'plain':   false
	} {
		source := os.join_path(root, '${name}.v')
		os.write_file(source, 'module ${name}\n\npub fn value() int {\n\treturn 1\n}\n')!
		marker := if calls { '${recover_call_marker}\n\n' } else { '' }
		manager.write_header(name, [source], 'module ${name}\n\n${marker}pub fn value() int\n')!
		entry := manager.valid_header(name, [source]) or { panic('no valid header for ${name}') }
		assert header_calls_recover(entry) == calls, name
		assert !header_needs_source(entry)
	}
}

fn test_generic_receiver_names_exclude_array_and_map_receivers() {
	assert receiver_has_generic_type_args('Stack[int].push')
	assert receiver_has_generic_type_args('datatypes.Stack[int].push')
	assert receiver_has_generic_type_args('&Pair[string, int].swap')
	// `[]u8.hex` is a method of an array, not of a generic `u8`. Stripping its
	// brackets gives `u8.hex`, and every caller of that would be taken for the
	// user of a generic method, its body embedded in the header for nothing.
	assert !receiver_has_generic_type_args('[]u8.hex')
	assert !receiver_has_generic_type_args('[]string.join')
	assert !receiver_has_generic_type_args('[4]int.sum')
	assert !receiver_has_generic_type_args('map[string]int.keys')
	assert !receiver_has_generic_type_args('?[]u8.hex')
	assert !receiver_has_generic_type_args('string.free')
}

// fake_pkg_config writes a `pkg-config` that looks up `<name>.pc` in `packages`
// and logs each of its invocations to `log`. A package that requires `dep`
// exists while `dep.pc` is at version 2, the way a real pkg-config walks the
// requirements of a package before it says that the package is there.
fn fake_pkg_config(dir string, packages string, log string) {
	os.mkdir_all(dir) or { panic(err) }
	path := os.join_path(dir, 'pkg-config')
	os.write_file(path, '#!/bin/sh
echo "\$@" >> "${log}"
if [ "\$1" = "--variable" ]; then
	echo "${packages}"
	exit 0
fi
if [ "\$1" = "--exists" ] && [ -f "${packages}/\$2.pc" ]; then
	if grep -q "^Requires: dep" "${packages}/\$2.pc" && ! grep -q "^Version: 2" "${packages}/dep.pc"; then
		exit 1
	fi
	exit 0
fi
exit 1
') or {
		panic(err)
	}
	os.chmod(path, 0o700) or { panic(err) }
}

struct FakePkgConfig {
	root      string
	packages  string
	log       string
	cache_dir string
	saved     map[string]string
	unset     []string
}

// use_fake_pkg_config puts a fake pkg-config first in PATH and clears the
// variables that would redirect its search.
fn use_fake_pkg_config(name string) FakePkgConfig {
	root := os.join_path(os.vtmp_dir(), 'v3_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	packages := os.join_path(root, 'packages')
	os.mkdir_all(packages) or { panic(err) }
	log := os.join_path(root, 'invocations.log')
	fake_pkg_config(os.join_path(root, 'bin'), packages, log)
	mut saved := map[string]string{}
	mut unset := []string{}
	for variable, value in os.environ() {
		if variable == 'PATH' || variable.starts_with('PKG_CONFIG') {
			saved[variable] = value
			if variable != 'PATH' {
				unset << variable
			}
		}
	}
	for variable in unset {
		os.unsetenv(variable)
	}
	os.setenv('PATH', '${os.join_path(root, 'bin')}${os.path_delimiter}${saved['PATH']}', true)
	return FakePkgConfig{
		root:      root
		packages:  packages
		log:       log
		cache_dir: os.join_path(root, 'cache')
		saved:     saved
		unset:     unset
	}
}

fn (f FakePkgConfig) restore() {
	for variable, value in f.saved {
		os.setenv(variable, value, true)
	}
	os.rmdir_all(f.root) or {}
}

fn (f FakePkgConfig) invocations() []string {
	return (os.read_file(f.log) or { '' }).split_into_lines()
}

// settled reports whether the file system can tell this state of a package file
// from the next one already; pkg-config is asked every time until it can.
fn (f FakePkgConfig) settled(name string) bool {
	return file_metadata_signature(os.join_path(f.packages, '${name}.pc')) != ''
}

fn test_pkg_config_answers_are_recorded_until_its_packages_change() {
	$if windows {
		return
	}
	fake := use_fake_pkg_config('pkgconfig_answers')
	defer {
		fake.restore()
	}
	// One process asks once, whatever it validates.
	mut probes := &PkgConfigProbes{}
	assert !pkg_config_exists('v3-absent', fake.cache_dir, probes)
	assert !pkg_config_exists('v3-absent', fake.cache_dir, probes)
	assert fake.invocations() == ['--variable pc_path pkg-config', '--exists v3-absent']
	// The next process reads what the same pkg-config answered.
	assert !pkg_config_exists('v3-absent', fake.cache_dir, &PkgConfigProbes{})
	assert fake.invocations().len == 2
	// Installing the package changes its directory, and with it the answer.
	os.write_file(os.join_path(fake.packages, 'v3-absent.pc'), 'Name: v3-absent\n') or {
		panic(err)
	}
	if !fake.settled('v3-absent') {
		return
	}
	assert pkg_config_exists('v3-absent', fake.cache_dir, &PkgConfigProbes{})
	assert fake.invocations().last() == '--exists v3-absent'
	asked := fake.invocations().len
	assert pkg_config_exists('v3-absent', fake.cache_dir, &PkgConfigProbes{})
	assert fake.invocations().len == asked
	// Without a cache directory there is nothing to read an answer from.
	assert pkg_config_exists('v3-absent', '', &PkgConfigProbes{})
	assert fake.invocations().len == asked + 1
}

// A package is there while what it requires is there too, in a version that it
// accepts. Editing the file of a requirement in place changes neither the file
// of the package nor the listing of its directory, and still changes the answer.
fn test_pkg_config_answer_follows_an_edited_requirement() {
	$if windows {
		return
	}
	fake := use_fake_pkg_config('pkgconfig_requirement')
	defer {
		fake.restore()
	}
	os.write_file(os.join_path(fake.packages, 'foo.pc'), 'Name: foo\nRequires: dep >= 2\n') or {
		panic(err)
	}
	dep := os.join_path(fake.packages, 'dep.pc')
	os.write_file(dep, 'Name: dep\nVersion: 2\n') or { panic(err) }
	if !fake.settled('foo') || !fake.settled('dep') {
		return
	}
	assert pkg_config_exists('foo', fake.cache_dir, &PkgConfigProbes{})
	asked := fake.invocations().len
	assert pkg_config_exists('foo', fake.cache_dir, &PkgConfigProbes{})
	assert fake.invocations().len == asked
	foo_before := file_metadata_signature(os.join_path(fake.packages, 'foo.pc'))
	os.write_file(dep, 'Name: dep\nVersion: 1\n') or { panic(err) }
	if !fake.settled('dep') {
		return
	}
	assert file_metadata_signature(os.join_path(fake.packages, 'foo.pc')) == foo_before
	assert !pkg_config_exists('foo', fake.cache_dir, &PkgConfigProbes{})
	assert fake.invocations().last() == '--exists foo'
	assert fake.invocations().len == asked + 1
	// The answer for the edited requirement is recorded in its turn.
	assert !pkg_config_exists('foo', fake.cache_dir, &PkgConfigProbes{})
	assert fake.invocations().len == asked + 1
}

// Search permission lets pkg-config read a known package file even when the
// directory cannot be listed. A readable directory elsewhere in the search
// must not allow answers to persist without the unlisted packages' identities.
fn test_pkg_config_answers_do_not_persist_for_an_unlistable_search_directory() {
	$if windows {
		return
	}
	fake := use_fake_pkg_config('pkgconfig_unlistable')
	defer {
		os.chmod(fake.packages, 0o700) or { panic(err) }
		os.unsetenv('PKG_CONFIG_PATH')
		fake.restore()
	}
	readable := os.join_path(fake.root, 'readable')
	os.mkdir_all(readable) or { panic(err) }
	os.setenv('PKG_CONFIG_PATH', readable, true)
	executable := os.join_path(fake.root, 'bin', 'pkg-config')
	// Settle coarse file timestamps so ordinary runs reach the directory boundary.
	old_time := time.utc().unix() - coarse_mtime_recent_seconds - 1
	os.utime(executable, old_time, old_time) or { panic(err) }
	if file_metadata_signature(executable) == '' {
		// This compiler would already avoid persistent answers without a settled
		// executable identity, so the directory boundary needs no further guard.
		return
	}
	foo := os.join_path(fake.packages, 'foo.pc')
	dep := os.join_path(fake.packages, 'dep.pc')
	os.write_file(foo, 'Name: foo\nRequires: dep >= 2\n') or { panic(err) }
	os.write_file(dep, 'Name: dep\nVersion: 2\n') or { panic(err) }
	os.chmod(fake.packages, 0o111) or { panic(err) }
	if _ := os.ls(fake.packages) {
		// A privileged user may still list the directory, so this permission
		// boundary cannot be exercised by that runner.
		return
	}
	assert (os.read_file(foo) or { '' }) == 'Name: foo\nRequires: dep >= 2\n'
	assert pkg_config_exists('foo', fake.cache_dir, &PkgConfigProbes{})
	os.write_file(dep, 'Name: dep\nVersion: 1\n') or { panic(err) }
	assert !pkg_config_exists('foo', fake.cache_dir, &PkgConfigProbes{})
	assert fake.invocations().filter(it == '--exists foo').len == 2
	assert pkg_config_state_key(fake.cache_dir) == 0
}
