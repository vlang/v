module driver

import os
import v.flat
import v.modulecache
import v.types

fn test_cached_program_prefix_keeps_only_its_reachable_wrappers() ! {
	root := os.join_path(os.vtmp_dir(), 'cached_program_wrappers_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	mut state := V3ModuleCacheState{
		manager:      modulecache.Manager{
			dir:     root
			enabled: true
		}
		bundle_valid: true
	}
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	used := map[string]bool{}
	generated := 'int main_target(int value);
int specialization_target(int value);
int support_target(int value);
int prefix_target(int value);
int imported_target(int value);
/* V3CACHE_PROGRAM_WRAPPERS */
static int main_adapter(int value) { return main_target(value); }
static int specialization_adapter(int value) { return specialization_target(value); }
static int support_adapter(int value) { return support_target(value); }
static int prefix_adapter(int value) { return prefix_target(value); }
static int imported_adapter(int value) { return imported_target(value); }
typedef void (*__v3_callback_identity_fn)(void);
static __v3_callback_identity_fn __v3_callback_identity(__v3_callback_identity_fn __v3_identity_candidate) {
	if (__v3_identity_candidate == (__v3_callback_identity_fn)main_adapter) { return (__v3_callback_identity_fn)main_target; }
	if (__v3_identity_candidate == (__v3_callback_identity_fn)imported_adapter) { return (__v3_callback_identity_fn)imported_target; }
	return __v3_identity_candidate;
}
/* V3CACHE_PROGRAM_WRAPPERS_END */
static int (*prefix_callback)(int) = prefix_adapter;
static int prefix_helper(int value) { return prefix_callback(value); }
/* V3CACHE_BODY_BEGIN */
/* V3CACHE_MODULE __v3_program_specializations */
int specialization(void) { return specialization_adapter(2); }
/* V3CACHE_MODULE __v3_program_support */
int support(void) { return support_adapter(3); }
/* V3CACHE_MODULE main */
int main(void) {
	__v3_callback_identity((__v3_callback_identity_fn)main_adapter);
	return main_adapter(1) + specialization() + support() + prefix_helper(4);
}
/* V3CACHE_MODULE imported */
int imported(void) { return imported_adapter(5); }
/* V3CACHE_BODY_END */
'
	prepared := prepare_v3_module_cache(generated, &used, &used, &tc, '-std=gnu11', '',
		'', '', []string{}, false, '', 'wrappers', unsafe { nil }, mut state)!
	// The executable, saved prefix and declaration plans must agree. A cached
	// module owns imported_adapter; no program unit may retain its stale target.
	for source in [prepared.main_source, prepared.program_prefix_source, prepared.program_declarations,
		prepared.tcc_program_declarations, prepared.tcc_main_source] {
		assert !source.contains('imported_adapter'), source
		for name in ['main_adapter', 'specialization_adapter', 'support_adapter', 'prefix_adapter'] {
			assert source.contains('static int ${name}(int value)'), source
		}
		assert source.contains('static int (*prefix_callback)(int) = prefix_adapter;'), source
		assert source.contains('static int prefix_helper(int value)'), source
		assert source.contains('(__v3_callback_identity_fn)main_adapter) { return'), source
		assert source.contains('\treturn __v3_identity_candidate;'), source
	}
}
