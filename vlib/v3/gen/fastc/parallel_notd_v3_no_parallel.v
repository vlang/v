module fastc

import os
import v3.pref
import v3.scanner
import v3.token

struct FastcPendingReferences {
mut:
	workers    []thread map[string]bool
	references map[string]bool
}

struct FastcPendingInterfaceDispatches {
mut:
	workers    []thread string
	dispatches string
}

fn fastc_start_referenced_function_names(sources []FastcSourceFile, prefs &pref.Preferences, functions map[string]FastcFunctionSignature) FastcPendingReferences {
	if fastc_parallel_job_count(sources.len, prefs) <= 1 {
		return FastcPendingReferences{
			references: fastc_collect_referenced_function_names(sources, prefs, functions)
		}
	}
	return FastcPendingReferences{
		workers: [spawn fastc_collect_referenced_function_names(sources, prefs, functions)]
	}
}

fn fastc_wait_referenced_function_names(mut pending FastcPendingReferences) map[string]bool {
	if pending.workers.len == 0 {
		return pending.references
	}
	return pending.workers[0].wait()
}

fn fastc_start_interface_dispatches(declared_kinds map[string]FastcDeclaredTypeKind, functions map[string]FastcFunctionSignature, interface_methods map[string]bool, used_function_names map[string]bool, selfhost bool, prefs &pref.Preferences) FastcPendingInterfaceDispatches {
	if fastc_parallel_job_count(functions.len, prefs) <= 1 {
		return FastcPendingInterfaceDispatches{
			dispatches: fastc_generate_interface_dispatches(declared_kinds, functions, interface_methods, used_function_names, selfhost)
		}
	}
	return FastcPendingInterfaceDispatches{
		workers: [
			spawn fastc_generate_interface_dispatches(declared_kinds, functions, interface_methods, used_function_names, selfhost),
		]
	}
}

fn fastc_wait_interface_dispatches(mut pending FastcPendingInterfaceDispatches) string {
	if pending.workers.len == 0 {
		return pending.dispatches
	}
	return pending.workers[0].wait()
}

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
	second_start := paths.len / jobs
	second_end := paths.len * 2 / jobs
	second_thread := spawn fastc_load_source_chunk(paths, prefs, second_start, second_end)
	mut chunk_threads := [second_thread]
	for chunk_idx in 2 .. jobs {
		start := paths.len * chunk_idx / jobs
		end := paths.len * (chunk_idx + 1) / jobs
		chunk_threads << spawn fastc_load_source_chunk(paths, prefs, start, end)
	}
	mut loaded := []FastcLoadedSource{cap: paths.len}
	loaded << fastc_load_source_chunk(paths, prefs, 0, first_end)
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
	second_thread := spawn fastc_collect_generic_method_source_chunk(sources, prefs, bounds[2], bounds[3])
	mut chunk_threads := [second_thread]
	for chunk_idx in 2 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_generic_method_source_chunk(sources, prefs, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	mut result := fastc_collect_generic_method_source_chunk(sources, prefs, bounds[0], bounds[1])
	for chunk_thread in chunk_threads {
		chunk := chunk_thread.wait()
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

const fastc_generation_fragment_size = 56 * 1024

fn fastc_source_generation_fragments(source_file FastcSourceFile, prefs &pref.Preferences) []FastcSourceFile {
	if !prefs.building_v || !source_file.header.module_name.ends_with('fastc') || source_file.source.len <= fastc_generation_fragment_size {
		return [source_file]
	}
	part_count := (source_file.source.len + fastc_generation_fragment_size - 1) / fastc_generation_fragment_size
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file(source_file.path, source_file.source.len)
	file.index_lines_without_digest(source_file.source)
	mut scan := scanner.new_scanner(prefs, .normal)
	scan.init(file, source_file.source)
	mut cuts := [0]
	mut depth := 0
	mut next_target := source_file.source.len / part_count
	mut tok := scan.scan()
	for tok != .eof && cuts.len < part_count {
		if tok == .lcbr {
			depth++
		} else if tok == .rcbr {
			depth--
			if depth == 0 && scan.offset >= next_target {
				cuts << scan.offset
				next_target = source_file.source.len * cuts.len / part_count
			}
		}
		tok = scan.scan()
	}
	if cuts.len == 1 {
		return [source_file]
	}
	cuts << source_file.source.len
	mut fragments := []FastcSourceFile{cap: cuts.len - 1}
	mut position_offset := 0
	mut position_lines := 0
	mut position_column := 0
	for i in 0 .. cuts.len - 1 {
		start := cuts[i]
		for position_offset < start {
			if source_file.source[position_offset] == `\n` {
				position_lines++
				position_column = 0
			} else {
				position_column++
			}
			position_offset++
		}
		prefix := '\n'.repeat(position_lines) + ' '.repeat(position_column)
		fragments << FastcSourceFile{
			path: source_file.path
			source: prefix + source_file.source[start..cuts[i + 1]]
			source_offset: source_file.source_offset + start - prefix.len
			header: source_file.header
		}
	}
	return fragments
}

fn fastc_generation_fragments(sources []FastcSourceFile, prefs &pref.Preferences) []FastcSourceFile {
	mut fragments := []FastcSourceFile{cap: sources.len + 8}
	for source_file in sources {
		fragments << fastc_source_generation_fragments(source_file, prefs)
	}
	return fragments
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
	generation_sources := fastc_generation_fragments(sources, ctx.prefs)
	job_indices := fastc_file_generation_job_indices(generation_sources, jobs)
	second_thread := spawn fastc_generate_file_chunk(ctx, generation_sources, job_indices[1])
	mut chunk_threads := [second_thread]
	for chunk_idx in 2 .. jobs {
		chunk_thread := spawn fastc_generate_file_chunk(ctx, generation_sources, job_indices[chunk_idx])
		chunk_threads << chunk_thread
	}
	mut outputs := []FastcFileGenOutput{len: generation_sources.len}
	first_outputs := fastc_generate_file_chunk(ctx, generation_sources, job_indices[0])
	for indexed_output in first_outputs {
		outputs[indexed_output.index] = indexed_output.output
	}
	for chunk_thread in chunk_threads {
		chunk_outputs := chunk_thread.wait()
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

fn fastc_merge_reference_partial(chunk FastcReferencePartial, mut references map[string]map[string]bool, mut top_level_references map[string]bool) {
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
	second_thread := spawn fastc_collect_reference_chunk(sources, prefs, available_names, bounds[2], bounds[3])
	mut chunk_threads := [second_thread]
	for chunk_idx in 2 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_reference_chunk(sources, prefs, available_names, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	first := fastc_collect_reference_chunk(sources, prefs, available_names, bounds[0], bounds[1])
	fastc_merge_reference_partial(first, mut references, mut top_level_references)
	for chunk_thread in chunk_threads {
		chunk := chunk_thread.wait()
		fastc_merge_reference_partial(chunk, mut references, mut top_level_references)
	}
}

fn fastc_collect_declaration_indexes(sources []FastcSourceFile, prefs &pref.Preferences, mut declared_types map[string]bool, mut declared_kinds map[string]FastcDeclaredTypeKind, mut enum_flags map[string]bool, mut params_structs map[string]bool, mut type_source_paths map[string]bool, mut type_sources map[string]string, mut constants map[string]string, mut public_constants map[string]bool, mut constant_sources map[string]string, mut globals map[string]string, mut public_globals map[string]bool) ! {
	jobs := fastc_parallel_jobs(sources, prefs)
	if jobs <= 1 {
		partial := fastc_collect_declaration_chunk(sources, prefs, 0, sources.len)
		fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut globals, mut public_globals)!
		return
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	second_thread := spawn fastc_collect_declaration_chunk(sources, prefs, bounds[2], bounds[3])
	mut chunk_threads := [second_thread]
	for chunk_idx in 2 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_declaration_chunk(sources, prefs, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	first := fastc_collect_declaration_chunk(sources, prefs, bounds[0], bounds[1])
	fastc_merge_declaration_partial(first, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut globals, mut public_globals)!
	for chunk_thread in chunk_threads {
		partial := chunk_thread.wait()
		fastc_merge_declaration_partial(partial, mut declared_types, mut declared_kinds, mut enum_flags, mut params_structs, mut type_source_paths, mut type_sources, mut constants, mut public_constants, mut constant_sources, mut globals, mut public_globals)!
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
	second_thread := spawn fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, bounds[2], bounds[3])
	mut chunk_threads := [second_thread]
	for chunk_idx in 2 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	first := fastc_collect_signature_chunk(sources, prefs, declared_types, declared_type_c_names, params_structs, bounds[0], bounds[1])
	fastc_merge_signature_partial(first, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
	for chunk_thread in chunk_threads {
		partial := chunk_thread.wait()
		fastc_merge_signature_partial(partial, mut functions, mut interface_methods, mut interface_fields, mut embed_embedders, mut embed_embeddeds)!
	}
}
