module fastc

import v3.pref

// Descendant FastC generations compile this serial dispatcher: the FastC
// parser has no `spawn`/`thread` lowering, so the parallel variants stay
// outside the selfhost source set until it does.
fn fastc_generate_file_outputs(ctx &FastcFileGenContext, sources []FastcSourceFile) []FastcFileGenOutput {
	mut outputs := []FastcFileGenOutput{cap: sources.len}
	for source_file in sources {
		outputs << fastc_generate_single_file(ctx, source_file)
	}
	return outputs
}

// fastc_collect_reference_partials scans every source file serially in the
// selfhost build.
fn fastc_collect_reference_partials(sources []FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
	for source_file in sources {
		fastc_collect_file_references(source_file, prefs, available_names, mut references, mut
			top_level_references)
	}
}
