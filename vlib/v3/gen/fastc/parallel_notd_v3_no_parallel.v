module fastc

import os
import v3.pref

fn fastc_parallel_job_count(item_count int, prefs &pref.Preferences) int {
	if prefs.no_parallel {
		return 1
	}
	mut jobs := fastc_nr_cpus()
	vjobs := os.getenv('VJOBS').int()
	if vjobs > 0 {
		jobs = vjobs
	}
	if os.getenv('V3_FASTC_NO_PARALLEL') != '' {
		jobs = 1
	}
	if jobs > item_count {
		jobs = item_count
	}
	if item_count < 4 {
		jobs = 1
	}
	return jobs
}

// fastc_parallel_jobs picks the worker count for a parallel phase; 1 selects
// the serial path. `-no-parallel` and VJOBS mirror the AST pipeline's
// behavior, and V3_FASTC_NO_PARALLEL forces serial for debugging and for
// byte-comparing parallel output against a serial run.
fn fastc_parallel_jobs(sources []FastcSourceFile, prefs &pref.Preferences) int {
	return fastc_parallel_job_count(sources.len, prefs)
}

fn fastc_load_source_chunk(paths []string, prefs &pref.Preferences, start int, end int) []FastcLoadedSource {
	mut loaded := []FastcLoadedSource{cap: end - start}
	for i in start .. end {
		loaded << fastc_load_source(paths[i], prefs)
	}
	return loaded
}

fn fastc_load_source_headers(paths []string, prefs &pref.Preferences) []FastcLoadedSource {
	jobs := fastc_parallel_job_count(paths.len, prefs)
	if jobs <= 1 {
		return fastc_load_source_chunk(paths, prefs, 0, paths.len)
	}
	first_end := paths.len / jobs
	first_thread := spawn fastc_load_source_chunk(paths, prefs, 0, first_end)
	mut chunk_threads := [first_thread]
	for chunk_idx in 1 .. jobs {
		start := paths.len * chunk_idx / jobs
		end := paths.len * (chunk_idx + 1) / jobs
		chunk_threads << spawn fastc_load_source_chunk(paths, prefs, start, end)
	}
	mut loaded := []FastcLoadedSource{cap: paths.len}
	for chunk_thread in chunk_threads {
		loaded << chunk_thread.wait()
	}
	return loaded
}

// fastc_chunk_bounds splits the file list into contiguous chunks balanced by
// source size, returned as flattened [start, end) pairs.
fn fastc_chunk_bounds(sources []FastcSourceFile, jobs int) []int {
	mut total_size := i64(0)
	for source_file in sources {
		total_size += i64(source_file.source.len) + 1
	}
	mut bounds := []int{cap: jobs * 2}
	mut start := 0
	mut consumed := i64(0)
	for chunk_idx in 0 .. jobs {
		mut end := start
		target := total_size * i64(chunk_idx + 1) / i64(jobs)
		// Leave one file for every later chunk even when a large file keeps the
		// current chunk below its cumulative size target.
		last_available := sources.len - (jobs - chunk_idx - 1)
		for end < last_available && (chunk_idx == jobs - 1 || consumed < target || end == start) {
			consumed += i64(sources[end].source.len) + 1
			end++
		}
		bounds << start
		bounds << end
		start = end
	}
	return bounds
}

// fastc_collect_generic_method_sources indexes every generic method and free
// function remaining after source monomorphization. Scanning is independent per
// file, and chunk maps are merged in source order for deterministic duplicates.
fn fastc_collect_generic_method_sources(sources []FastcSourceFile, prefs &pref.Preferences) map[string]FastcGenericMethodSource {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		return fastc_collect_generic_method_source_chunk(sources, prefs, 0, sources.len)
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	first_thread := spawn fastc_collect_generic_method_source_chunk(sources, prefs, bounds[0], bounds[1])
	mut chunk_threads := [first_thread]
	for chunk_idx in 1 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_generic_method_source_chunk(sources, prefs, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	mut result := map[string]FastcGenericMethodSource{}
	for chunk_idx in 0 .. chunk_threads.len {
		chunk := chunk_threads[chunk_idx].wait()
		for key, generic in chunk {
			result[key] = generic
		}
	}
	return result
}

struct FastcIndexedFileGenOutput {
	index  int
	output FastcFileGenOutput
}

// fastc_file_generation_job_indices assigns the largest files first to the
// currently lightest worker. This avoids clustering the generator's largest
// source files at the end of the dependency-ordered source list.
fn fastc_file_generation_job_indices(sources []FastcSourceFile, jobs int) [][]int {
	mut order := []int{len: sources.len}
	for i in 0 .. sources.len {
		order[i] = i
	}
	// There are normally only a few hundred files; insertion sort avoids a
	// closure and keeps this scheduling helper in FastC's self-hostable subset.
	for i in 1 .. order.len {
		index := order[i]
		weight := sources[index].source.len
		mut insert_at := i
		for insert_at > 0 && sources[order[insert_at - 1]].source.len < weight {
			order[insert_at] = order[insert_at - 1]
			insert_at--
		}
		order[insert_at] = index
	}
	mut job_indices := [][]int{len: jobs}
	mut job_weights := []i64{len: jobs}
	for index in order {
		mut lightest := 0
		for job in 1 .. jobs {
			if job_weights[job] < job_weights[lightest] {
				lightest = job
			}
		}
		job_indices[lightest] << index
		job_weights[lightest] += i64(sources[index].source.len) + 1
	}
	return job_indices
}

// fastc_generate_file_chunk generates one worker's files. The backing source
// data is shared and read-only. It runs as a value-returning spawn: under
// -prealloc, V frees a void thread's arena when the thread exits, so outputs
// must travel through the thread result.
fn fastc_generate_file_chunk(ctx &FastcFileGenContext, sources []FastcSourceFile, indices []int) []FastcIndexedFileGenOutput {
	mut outputs := []FastcIndexedFileGenOutput{cap: indices.len}
	for index in indices {
		outputs << FastcIndexedFileGenOutput{
			index: index
			output: fastc_generate_single_file(ctx, sources[index])
		}
	}
	return outputs
}

// fastc_generate_file_outputs runs per-file code generation, in parallel when
// more than one job is available. Results are restored to file order, so the
// emitted C is identical to a serial run.
fn fastc_generate_file_outputs(ctx &FastcFileGenContext, sources []FastcSourceFile) []FastcFileGenOutput {
	jobs := fastc_parallel_jobs(sources, ctx.prefs)
	if jobs <= 1 {
		mut outputs := []FastcFileGenOutput{cap: sources.len}
		for source_file in sources {
			outputs << fastc_generate_single_file(ctx, source_file)
		}
		return outputs
	}
	job_indices := fastc_file_generation_job_indices(sources, jobs)
	first_thread := spawn fastc_generate_file_chunk(ctx, sources, job_indices[0])
	mut chunk_threads := [first_thread]
	for chunk_idx in 1 .. jobs {
		chunk_thread := spawn fastc_generate_file_chunk(ctx, sources, job_indices[chunk_idx])
		chunk_threads << chunk_thread
	}
	mut outputs := []FastcFileGenOutput{len: sources.len}
	for chunk_idx in 0 .. chunk_threads.len {
		chunk_outputs := chunk_threads[chunk_idx].wait()
		for indexed_output in chunk_outputs {
			outputs[indexed_output.index] = indexed_output.output
		}
	}
	return outputs
}

// FastcReferencePartial carries one chunk's function-reference scan across
// its thread boundary; partials union commutatively into program totals.
struct FastcReferencePartial {
mut:
	references           map[string]map[string]bool
	top_level_references map[string]bool
}

fn fastc_collect_reference_chunk(sources []FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, start int, end int) FastcReferencePartial {
	mut references := map[string]map[string]bool{}
	mut top_level_references := map[string]bool{}
	for idx in start .. end {
		fastc_collect_file_references(sources[idx], prefs, available_names, mut references, mut top_level_references)
	}
	return FastcReferencePartial{
		references: references
		top_level_references: top_level_references
	}
}

// fastc_collect_reference_partials fans the per-file reference scan out over
// size-balanced chunks and unions the partials in file order. Reference sets
// union commutatively, so the merged result matches a serial scan exactly.
fn fastc_collect_reference_partials(sources []FastcSourceFile, prefs &pref.Preferences, available_names map[string]bool, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		for source_file in sources {
			fastc_collect_file_references(source_file, prefs, available_names, mut references, mut top_level_references)
		}
		return
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	first_thread := spawn fastc_collect_reference_chunk(sources, prefs, available_names, bounds[0], bounds[1])
	mut chunk_threads := [first_thread]
	for chunk_idx in 1 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_reference_chunk(sources, prefs, available_names, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	for chunk_idx in 0 .. chunk_threads.len {
		chunk := chunk_threads[chunk_idx].wait()
		for function_name in chunk.references.keys() {
			mut combined := map[string]bool{}
			if function_name in references {
				combined = references[function_name].clone()
			}
			for referenced_name, _ in chunk.references[function_name] {
				combined[referenced_name] = true
			}
			references[function_name] = combined.clone()
		}
		for referenced_name, _ in chunk.top_level_references {
			top_level_references[referenced_name] = true
		}
	}
}

fn fastc_collect_declaration_indexes(sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut constants map[string]string, mut public_constants map[string]bool, mut globals map[string]string, mut public_globals map[string]bool) ! {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		partial := fastc_collect_declaration_chunk(sources, prefs, 0, sources.len)
		fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut constants, mut public_constants, mut globals, mut public_globals)!
		return
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	first_thread := spawn fastc_collect_declaration_chunk(sources, prefs, bounds[0], bounds[1])
	mut chunk_threads := [first_thread]
	for chunk_idx in 1 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_declaration_chunk(sources, prefs, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	for chunk_idx in 0 .. chunk_threads.len {
		partial := chunk_threads[chunk_idx].wait()
		fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut constants, mut public_constants, mut globals, mut public_globals)!
	}
}

fn fastc_collect_signatures(sources []FastcSourceFile, prefs &pref.Preferences, declared_types map[string]bool, declared_type_c_names map[string]string, params_structs map[string]bool, mut functions map[string]FastcFunctionSignature, mut interface_methods map[string]bool, mut interface_fields map[string]FastcInterfaceField, mut embed_embedders []string, mut embed_embeddeds []string) ! {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		partial := fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, 0, sources.len)
		fastc_merge_signature_partial(partial, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
		return
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	first_thread := spawn fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, bounds[0], bounds[1])
	mut chunk_threads := [first_thread]
	for chunk_idx in 1 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	for chunk_idx in 0 .. chunk_threads.len {
		partial := chunk_threads[chunk_idx].wait()
		fastc_merge_signature_partial(partial, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
	}
}
