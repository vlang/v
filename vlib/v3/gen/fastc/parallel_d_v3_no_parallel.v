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

fn fastc_preload_memo(memo FastcResolveMemo, prefs &pref.Preferences, canonical_vlib string, mut module_path_cache map[string]string, mut module_dir_files map[string][]string) map[string]FastcLoadedSource {
	tasks := fastc_memo_tasks(memo)
	mut results := []FastcMemoResult{len: tasks.len}
	for index, task in tasks {
		results[index] = fastc_run_memo_task(task, index, prefs, canonical_vlib)
	}
	return fastc_apply_memo_results(tasks, results, prefs, mut module_path_cache, mut module_dir_files)
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

fn fastc_collect_generic_and_declaration_indexes(mut sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut constant_spans map[string][]int, mut globals map[string]string, mut public_globals map[string]bool) !map[string]FastcGenericMethodSource {
	generic_method_sources := fastc_collect_generic_method_sources(mut sources, prefs)
	fastc_collect_declaration_indexes(sources, prefs, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut globals, mut public_globals)!
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

struct FastcPendingFieldLookup {
mut:
	lookup map[string]map[string]FastcStructField
}

fn fastc_start_struct_field_lookup(struct_field_info map[string][]FastcStructField, prefs &pref.Preferences) FastcPendingFieldLookup {
	return FastcPendingFieldLookup{
		lookup: fastc_build_struct_field_lookup(struct_field_info)
	}
}

fn fastc_wait_struct_field_lookup(mut pending FastcPendingFieldLookup) map[string]map[string]FastcStructField {
	return pending.lookup
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

fn fastc_generate_file_outputs(ctx &FastcFileGenContext, sources []FastcSourceFile) []FastcFileGenOutput {
	mut outputs := []FastcFileGenOutput{cap: sources.len}
	for source_file in sources {
		outputs << fastc_generate_single_file(ctx, source_file)
	}
	return outputs
}

fn fastc_collect_reference_partials(sources []FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
	for source_file in sources {
		fastc_collect_file_references(source_file, prefs, available_names, mut references, mut top_level_references)
	}
}

fn fastc_collect_declaration_indexes(sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut constant_spans map[string][]int, mut globals map[string]string, mut public_globals map[string]bool) ! {
	partial := fastc_collect_declaration_chunk(sources, prefs, 0, sources.len)
	fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut constant_spans, mut globals, mut public_globals)!
}

fn fastc_collect_signatures(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut embed_embedders []string, mut embed_embeddeds []string) ! {
	partial := fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, 0, sources.len)
	fastc_merge_signature_partial(partial, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
}
