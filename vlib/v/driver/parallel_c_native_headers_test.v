module driver

import os

fn native_manifest(text string, check_header bool) !V3ParallelCManifest {
	root := os.join_path(os.vtmp_dir(), 'parallel_c_manifest_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'unit.c')
	return v3_parallel_c_native_manifest('clang', []string{}, false, text, path, []string{},
		{
			'native_value': true
			'NATIVE_SCALE': true
		}, check_header)
}

fn test_native_manifest_accepts_declarations_and_read_only_data() {
	os.find_abs_path_of_executable('clang') or { return }
	manifest := native_manifest('typedef struct Point { int x; int y; } Point;
extern int shared_counter;
int native_value(int);
static inline int twice(int x) { return x * 2; }
static const int table[3] = {1, 2, 3};
static const char *const names[] = {"a", "b"};
__attribute__((weak)) void hook(void *p) { (void)p; }
__attribute__((weak)) int weak_default = 1;', true)!
	assert manifest.hazards.len == 0, manifest.hazards.str()
	assert 'native_value' in manifest.declared
}

fn test_native_manifest_rejects_mutable_storage_and_definitions() {
	os.find_abs_path_of_executable('clang') or { return }
	for text in ['static int counter;', 'static const char *names;',
		'static inline int next(void) { static int id; return ++id; }', 'int global_value = 3;',
		'int compute(int x) { return x + 1; }',
		'static __attribute__((constructor)) void startup(void) {}'] {
		manifest := native_manifest(text, true)!
		assert manifest.hazards.len == 1, '${text}: ${manifest.hazards}'
	}
}

fn test_native_manifest_differences_keep_owner_declarations_and_macros() {
	os.find_abs_path_of_executable('clang') or { return }
	owner := native_manifest('#define NATIVE_SCALE 3
int native_value(int x) { return x * NATIVE_SCALE; }', false)!
	header := native_manifest('#define NATIVE_SCALE 2
', true)!
	names := v3_parallel_c_owner_names(owner, header)
	assert 'native_value' in names
	assert 'NATIVE_SCALE' in names
}

fn test_native_manifest_failure_is_not_treated_as_safe() {
	native_manifest('#include "definitely_missing_parallel_cc_header.h"', true) or { return }
	assert false, 'missing native toolchain input must refuse a split'
}

fn test_native_const_metadata_distinguishes_mutable_pointers() {
	assert v3_parallel_c_const_object_type('const int[3]')
	assert v3_parallel_c_const_object_type('const char *const[2]')
	assert !v3_parallel_c_const_object_type('const char *')
	assert !v3_parallel_c_const_object_type('int (*)(const int)')
	assert !v3_parallel_c_const_object_type('void (*)(int *const)')
}

fn test_used_identifiers_skip_comments_literals_and_numbers() {
	mut used := v3_c_used_identifiers([
		'void f(void) { g(0x1f, "h", \'i\'); /* j */ }\n// k\n',
		'int l = 2e5;',
	]).keys()
	used.sort()
	assert used == ['f', 'g', 'int', 'l', 'void']
}

fn test_body_parts_using_owner_names_are_detected_by_identifier() {
	names := {
		'native_draw': true
	}
	assert v3_c_text_uses_any('void f(void) { native_draw(1, 2); }', names)
	assert !v3_c_text_uses_any('void f(void) { native_draw_rect(1, 2); my_native_draw(); }', names)
	assert !v3_c_text_uses_any('void f(void) { 0native_draw; }', names)
	assert !v3_c_text_uses_any('void f(void) { puts("native_draw"); /* native_draw */ }', names)
	assert v3_c_text_uses_any('#if defined(native_draw)\nint x;\n#endif', names)
	assert !v3_c_text_uses_any('void f(void) { native_draw(); }', map[string]bool{})
}

fn test_partition_moves_only_the_functions_that_use_owner_names() {
	names := {
		'native_draw': true
	}
	part := '/* V3CACHE_MODULE main */
void main__paint(int x) {
	if (x) { native_draw(x, "}"); }
}
string main__label(void) {
	return _S("native_draw(\\"{\\")");
}
/* V3CACHE_MODULE gfx */
int gfx__count(void) {
	// native_draw(
	return 0;
}
void gfx__flush(void) { char c = \'}\'; native_draw(c, 0); }
'
	owner, rest := v3_c_partition_items(part, names)
	assert owner == '/* V3CACHE_MODULE main */
void main__paint(int x) {
	if (x) { native_draw(x, "}"); }
}

void gfx__flush(void) { char c = \'}\'; native_draw(c, 0); }
'
	// A name in a literal or a comment is not a use.
	assert rest == '
string main__label(void) {
	return _S("native_draw(\\"{\\")");
}
/* V3CACHE_MODULE gfx */
int gfx__count(void) {
	// native_draw(
	return 0;
}
'
}

fn test_partition_keeps_generated_conditional_groups_together() {
	text := '#if defined(NATIVE_ENABLED)\nvoid first(void) { native_draw(); }\nvoid second(void) {}\n#endif\n'
	owner, rest := v3_c_partition_items(text, {
		'native_draw': true
	})
	assert owner == text
	assert rest == ''
}

fn test_native_manifest_json_rejects_incomplete_metadata() {
	for text in ['', '{', '{"kind":"TranslationUnitDecl","inner":[{'] {
		mut reader := V3ParallelCManifestReader{ text: text }
		reader.declaration() or { continue }
		assert false, text
	}
}

fn test_native_manifest_refuses_unknown_declaration_kinds() {
	mut manifest := V3ParallelCManifest{}
	manifest.collect_declaration(V3ParallelCDecl{ kind: 'UnknownDecl' }, map[string]bool{},
		[]string{}, true, 'native.h')
	assert manifest.hazards.len == 1
}
