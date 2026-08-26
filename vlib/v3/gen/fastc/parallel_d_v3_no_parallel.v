module fastc

import v3.pref

// A `-no-parallel` (v3_no_parallel) FastC build omits threaded generation
// entirely, mirroring the AST pipeline's _d_v3_no_parallel variants: both
// phases run serially and no spawn helpers are referenced.
fn fastc_generate_file_outputs(ctx &FastcFileGenContext, sources []FastcSourceFile) []FastcFileGenOutput {
	mut outputs := []FastcFileGenOutput{cap: sources.len}
	for source_file in sources {
		outputs << fastc_generate_single_file(ctx, source_file)
	}
	return outputs
}

fn fastc_collect_reference_partials(sources []FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
	for source_file in sources {
		fastc_collect_file_references(source_file, prefs, available_names, mut references, mut
			top_level_references)
	}
}
