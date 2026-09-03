module fastc

import v3.pref

struct FastcPendingReferences {
	references map[string]bool
}

struct FastcPendingInterfaceDispatches {
	dispatches string
}

fn fastc_start_referenced_function_names(sources []FastcSourceFile, prefs &pref.Preferences, functions map[string]FastcFunctionSignature) FastcPendingReferences {
	return FastcPendingReferences{
		references: fastc_collect_referenced_function_names(sources, prefs, functions)
	}
}

fn fastc_wait_referenced_function_names(mut pending FastcPendingReferences) map[string]bool {
	return pending.references
}

fn fastc_start_interface_dispatches(declared_kinds map[string]FastcDeclaredTypeKind, functions map[string]FastcFunctionSignature, interface_methods map[string]bool, used_function_names map[string]bool, selfhost bool, _prefs &pref.Preferences) FastcPendingInterfaceDispatches {
	return FastcPendingInterfaceDispatches{
		dispatches: fastc_generate_interface_dispatches(declared_kinds, functions, interface_methods, used_function_names, selfhost)
	}
}

fn fastc_wait_interface_dispatches(mut pending FastcPendingInterfaceDispatches) string {
	return pending.dispatches
}

fn fastc_preload_memo(memo_path string, memo FastcResolveMemo, prefs &pref.Preferences, canonical_vlib string, entry_paths []string, entry_real_paths []string, mut module_path_cache map[string]string, mut module_dir_files map[string][]string, mut entry_files map[string][]string, mut real_path_cache map[string]string, mut loaded map[string]FastcLoadedSource) {
	probe_tasks := fastc_memo_probe_tasks(memo, entry_paths, entry_real_paths, prefs)
	mut probe_results := []FastcMemoResult{len: probe_tasks.len}
	for index, task in probe_tasks {
		probe_results[index] = fastc_run_memo_task(task, index, prefs, canonical_vlib)
	}
	fastc_apply_memo_results(probe_tasks, probe_results, prefs, mut module_path_cache, mut module_dir_files, mut entry_files, mut real_path_cache, mut loaded)
	blob := fastc_read_memo_blob(memo_path, memo)
	current_stamps := fastc_memo_current_stamps(probe_tasks, probe_results, memo.files.len)
	read_tasks := fastc_memo_read_tasks(memo, current_stamps, blob)
	mut read_results := []FastcMemoResult{len: read_tasks.len}
	for index, task in read_tasks {
		read_results[index] = fastc_run_memo_task(task, index, prefs, canonical_vlib)
	}
	fastc_apply_memo_results(read_tasks, read_results, prefs, mut module_path_cache, mut module_dir_files, mut entry_files, mut real_path_cache, mut loaded)
}

// FastcPendingMemoStore is a written resolve memo: without threads the memo
// is stored before the resolution returns.
struct FastcPendingMemoStore {
mut:
	stored bool
}

fn fastc_start_memo_store(memo_path string, previous_text string, sources []FastcSourceFile, builtin_dir string, lookup_modules []string, lookup_sources []string, prefs &pref.Preferences, module_path_cache map[string]string, module_dir_files map[string][]string, entry_paths []string, real_path_cache map[string]string, entry_files map[string][]string, preloaded map[string]FastcLoadedSource) FastcPendingMemoStore {
	fastc_store_resolve_memo(memo_path, previous_text, sources, builtin_dir, lookup_modules, lookup_sources, prefs, module_path_cache, module_dir_files, entry_paths, real_path_cache, entry_files, preloaded)
	return FastcPendingMemoStore{
		stored: true
	}
}

fn fastc_wait_memo_store(mut pending FastcPendingMemoStore) {
	pending.stored = true
}

// fastc_preload_sources is a no-op without threads: the ordering walk loads
// each file itself.
fn fastc_preload_sources(queue []FastcQueuedSource, prefs &pref.Preferences, canonical_vlib string, mut module_path_cache map[string]string, mut module_dir_files map[string][]string, mut real_path_cache map[string]string) map[string]FastcLoadedSource {
	return map[string]FastcLoadedSource{}
}

// A `-no-parallel` (v3_no_parallel) FastC build omits threaded generation
// entirely, mirroring the AST pipeline's _d_v3_no_parallel variants: both
// phases run serially and no spawn helpers are referenced.
fn fastc_collect_generic_method_sources(mut sources []FastcSourceFile, prefs &pref.Preferences) map[string]FastcGenericMethodSource {
	partial := fastc_collect_generic_method_source_chunk(sources, prefs, 0, sources.len)
	fastc_apply_scan_flags(mut sources, partial.flags, 0)
	return partial.sources
}

fn fastc_collect_generic_and_declaration_indexes(mut sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut constant_spans map[string][]int, mut global_sources map[string]string, mut globals map[string]string, mut public_globals map[string]bool) !map[string]FastcGenericMethodSource {
	generic_method_sources := fastc_collect_generic_method_sources(mut sources, prefs)
	fastc_collect_declaration_indexes(sources, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut global_sources, mut globals, mut public_globals)!
	return generic_method_sources
}

struct FastcPendingTypeDeclarations {
mut:
	result FastcTypeDeclarationResult
}

fn fastc_start_type_declarations(sources []FastcSourceFile, type_sources map[string]string, prefs &pref.Preferences, type_source_paths map[string]bool, declared_types map[string]bool, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, constants map[string]string, public_constants map[string]bool) FastcPendingTypeDeclarations {
	return FastcPendingTypeDeclarations{
		result: fastc_run_type_declarations(sources, type_sources, prefs, type_source_paths, declared_types, declared_kinds, enum_flags, constants, public_constants)
	}
}

fn fastc_wait_type_declarations(mut pending FastcPendingTypeDeclarations) !FastcTypeDeclarationResult {
	if pending.result.failed {
		return error(pending.result.error_message)
	}
	return pending.result
}

struct FastcPendingFieldDefaults {
mut:
	result FastcFieldDefaultsResult
}

fn fastc_start_field_defaults(source_imports map[string]map[string]string, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, fastc_prefixed_c_names []string, declared_kinds map[string]FastcDeclaredTypeKind, enum_flags map[string]bool, enum_field_types map[string]string, enum_field_names map[string][]string, alias_base_types map[string]string, struct_fields map[string]map[string]string, struct_field_info map[string][]FastcStructField, functions map[string]FastcFunctionSignature, constants map[string]string, public_constants map[string]bool, constant_types map[string]string, globals map[string]string, public_globals map[string]bool, global_types map[string]string, sum_types map[string]bool) FastcPendingFieldDefaults {
	return FastcPendingFieldDefaults{
		result: fastc_run_field_defaults(source_imports, prefs, declared_types, declared_type_c_names, fastc_prefixed_c_names, declared_kinds, enum_flags, enum_field_types, enum_field_names, alias_base_types, struct_fields, struct_field_info, functions, constants, public_constants, constant_types, globals, public_globals, global_types, sum_types)
	}
}

fn fastc_wait_field_defaults(mut pending FastcPendingFieldDefaults) !FastcFieldDefaultsResult {
	if pending.result.failed {
		return error(pending.result.error_message)
	}
	return pending.result
}

struct FastcPendingFragments {
mut:
	fragments []FastcSourceFile
}

fn fastc_start_generation_fragments(sources []FastcSourceFile, prefs &pref.Preferences) FastcPendingFragments {
	return FastcPendingFragments{
		fragments: sources
	}
}

fn fastc_wait_generation_fragments(mut pending FastcPendingFragments) []FastcSourceFile {
	return pending.fragments
}

fn fastc_generate_file_outputs(ctx &FastcFileGenContext, sources []FastcSourceFile) FastcFileGenResult {
	mut outputs := []FastcFileGenOutput{cap: sources.len}
	for source_file in sources {
		outputs << fastc_generate_single_file(ctx, source_file)
	}
	return fastc_file_gen_result(outputs)
}

fn fastc_collect_reference_partials(sources []FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
	for source_file in sources {
		fastc_collect_file_references(source_file, prefs, available_names, mut references, mut top_level_references)
	}
}

fn fastc_collect_declaration_indexes(sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut constant_spans map[string][]int, mut global_sources map[string]string, mut globals map[string]string, mut public_globals map[string]bool) ! {
	partial := fastc_collect_declaration_chunk(sources, prefs, 0, sources.len)
	fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut global_sources, mut globals, mut public_globals)!
}

fn fastc_collect_signatures(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut embed_embedders []string, mut embed_embeddeds []string) ! {
	partial := fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, 0, sources.len)
	fastc_merge_signature_partial(partial, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
}

// fastc_parse_constant_files_parallel returns no results: without threads the
// constants phase parses every file serially.
fn fastc_parse_constant_files_parallel(ctx &FastcConstantGenContext, candidates []FastcSourceFile, seed map[string]string) []FastcConstantFileResult {
	return []FastcConstantFileResult{}
}
