module fastc

import v.pref

fn fastc_native_source_context(sources []FastcSourceFile, prefs &pref.Preferences) FastcNativeSourceContext {
	jobs := fastc_parallel_job_count(sources.len, prefs)
	if jobs <= 1 {
		return fastc_native_source_context_chunk(sources, prefs, 0, sources.len)
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	mut workers := []thread FastcNativeSourceContext{cap: jobs - 1}
	for chunk in 1 .. jobs {
		workers << spawn fastc_native_source_context_chunk(sources, prefs, bounds[2 * chunk], bounds[2 * chunk + 1])
	}
	mut context := fastc_native_source_context_chunk(sources, prefs, bounds[0], bounds[1])
	for worker in workers {
		fastc_merge_native_source_context(mut context, worker.wait())
	}
	return context
}
