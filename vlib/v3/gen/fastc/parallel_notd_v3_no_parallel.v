module fastc

import os
import v3.pref

// fastc_parallel_jobs picks the worker count for a parallel phase; 1 selects
// the serial path. `-no-parallel` and VJOBS mirror the AST pipeline's
// behavior, and V3_FASTC_NO_PARALLEL forces serial for debugging and for
// byte-comparing parallel output against a serial run.
fn fastc_parallel_jobs(sources []FastcSourceFile, prefs &pref.Preferences) int {
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
	if jobs > sources.len {
		jobs = sources.len
	}
	if sources.len < 4 {
		jobs = 1
	}
	return jobs
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
		for end < sources.len && (chunk_idx == jobs - 1 || consumed < target || end == start) {
			consumed += i64(sources[end].source.len) + 1
			end++
		}
		bounds << start
		bounds << end
		start = end
	}
	return bounds
}

// fastc_generate_file_chunk generates a contiguous file range on its own
// thread. The array header is passed by value; the backing file data is
// shared and read-only. It runs as a value-returning spawn: under -prealloc,
// V frees a void thread's arena when the thread exits, so the outputs must
// travel through the thread result.
fn fastc_generate_file_chunk(ctx &FastcFileGenContext, sources []FastcSourceFile, start int, end int) []FastcFileGenOutput {
	mut outputs := []FastcFileGenOutput{cap: end - start}
	for idx in start .. end {
		outputs << fastc_generate_single_file(ctx, sources[idx])
	}
	return outputs
}

// fastc_generate_file_outputs runs per-file code generation, in parallel when
// more than one job is available. Files are split into contiguous chunks
// balanced by source size, and results are stitched back in file order, so
// the emitted C is identical to a serial run.
fn fastc_generate_file_outputs(ctx &FastcFileGenContext, sources []FastcSourceFile) []FastcFileGenOutput {
	jobs := fastc_parallel_jobs(sources, ctx.prefs)
	mut outputs := []FastcFileGenOutput{cap: sources.len}
	if jobs <= 1 {
		for source_file in sources {
			outputs << fastc_generate_single_file(ctx, source_file)
		}
		return outputs
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	first_thread := spawn fastc_generate_file_chunk(ctx, sources, bounds[0], bounds[1])
	mut chunk_threads := [first_thread]
	for chunk_idx in 1 .. bounds.len / 2 {
		chunk_thread := spawn fastc_generate_file_chunk(ctx, sources, bounds[chunk_idx * 2], bounds[
			chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	for chunk_idx in 0 .. chunk_threads.len {
		chunk_outputs := chunk_threads[chunk_idx].wait()
		for output in chunk_outputs {
			outputs << output
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
		fastc_collect_file_references(sources[idx], prefs, available_names, mut references, mut
			top_level_references)
	}
	return FastcReferencePartial{
		references:           references
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
			fastc_collect_file_references(source_file, prefs, available_names, mut references, mut
				top_level_references)
		}
		return
	}
	bounds := fastc_chunk_bounds(sources, jobs)
	first_thread := spawn fastc_collect_reference_chunk(sources, prefs, available_names, bounds[0],
		bounds[1])
	mut chunk_threads := [first_thread]
	for chunk_idx in 1 .. bounds.len / 2 {
		chunk_thread := spawn fastc_collect_reference_chunk(sources, prefs, available_names,
			bounds[chunk_idx * 2], bounds[chunk_idx * 2 + 1])
		chunk_threads << chunk_thread
	}
	for chunk_idx in 0 .. chunk_threads.len {
		chunk := chunk_threads[chunk_idx].wait()
		for function_name in chunk.references.keys() {
			chunk_refs := chunk.references[function_name].clone()
			mut combined := map[string]bool{}
			if function_name in references {
				combined = references[function_name].clone()
			}
			for referenced_name, _ in chunk_refs {
				combined[referenced_name] = true
			}
			references[function_name] = combined.clone()
		}
		for referenced_name, _ in chunk.top_level_references {
			top_level_references[referenced_name] = true
		}
	}
}
