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

fn fastc_load_source_headers(paths []string, prefs &pref.Preferences) []FastcLoadedSource {
	mut loaded := []FastcLoadedSource{cap: paths.len}
	for path in paths {
		loaded << fastc_load_source(path, prefs)
	}
	return loaded
}

// A `-no-parallel` (v3_no_parallel) FastC build omits threaded generation
// entirely, mirroring the AST pipeline's _d_v3_no_parallel variants: both
// phases run serially and no spawn helpers are referenced.
fn fastc_collect_generic_method_sources(sources []FastcSourceFile, prefs &pref.Preferences) map[string]FastcGenericMethodSource {
	return fastc_collect_generic_method_source_chunk(sources, prefs, 0, sources.len)
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

fn fastc_collect_declaration_indexes(sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut globals map[string]string, mut public_globals map[string]bool) ! {
	partial := fastc_collect_declaration_chunk(sources, prefs, 0, sources.len)
	fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut globals, mut public_globals)!
}

fn fastc_collect_signatures(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut embed_embedders []string, mut embed_embeddeds []string) ! {
	partial := fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, 0, sources.len)
	fastc_merge_signature_partial(partial, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
}
