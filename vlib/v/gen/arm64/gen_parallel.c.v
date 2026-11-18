module arm64

import v.ssa
import v.workers

fn C.atomic_fetch_add_u32(voidptr, u32) u32

// Parallel code generation.
//
// Functions are generated independently on the compiler's worker pool, as Go
// does in compileFunctions (cmd/compile/internal/gc/compile.go): workers pull
// work from a queue and keep their own reusable tables (ssa.Cache there, a Gen
// here). As in Zig, where codegen_func jobs produce MIR that one thread hands to
// the linker in order (Compilation.zig, link.Queue), each piece of output is
// merged by the calling thread in program order, so the object does not depend
// on which worker generated what.
//
// Contiguous function ranges rather than single functions are the unit of work:
// direct calls inside a range need no relocation and the merge touches fewer
// maps. There are several ranges per worker so slow cores simply take fewer.

const arm64_parallel_min_functions = 64

const arm64_chunks_per_worker = 8

// arm64_words_per_instruction sizes the text buffers: cmd/v emits about 4.1
// arm64 instructions per SSA instruction.
const arm64_words_per_instruction = 6

struct Arm64GenRange {
	start int
	end   int
	cap   int
}

// Arm64GenChunk is what one range of functions produced. Its strings are local
// to the range, and its symbols use chunk-relative addresses.
struct Arm64GenChunk {
	text_words []u32
	macho      &MachOObject = unsafe { nil }
	strings    []string
}

struct Arm64GenBatch {
	parent &Gen
	ranges []Arm64GenRange
mut:
	next   u32
	chunks []Arm64GenChunk
}

// gen_parallel generates the functions on `pool`, plus the calling thread, and
// produces the same object as gen. Module state must be frozen before workers
// share it, so this runs after the SSA module is final.
pub fn (mut g Gen) gen_parallel(pool &workers.Pool) {
	if isnil(pool) || pool.size() == 0 || g.prune_unused
		|| g.m.funcs.len < arm64_parallel_min_functions {
		g.gen()
		return
	}
	g.gen_pre_pass()
	// Freeze all lazily-computed layout data before workers share the SSA module.
	g.m.freeze_type_layouts()
	job_count := pool.size() + 1
	ranges := arm64_function_ranges(g.m, job_count * arm64_chunks_per_worker)
	mut batch := &Arm64GenBatch{
		parent: g
		ranges: ranges
		chunks: []Arm64GenChunk{len: ranges.len}
	}
	mut tasks := []workers.Task{cap: job_count}
	for i in 0 .. job_count {
		tasks << workers.Task{
			run:        arm64_gen_worker
			arg:        voidptr(batch)
			force_sync: i == 0
		}
	}
	mut worker_pool := unsafe { pool }
	worker_pool.run(tasks)
	g.reserve_merged_output(batch.chunks)
	for chunk in batch.chunks {
		g.merge_gen_chunk(chunk)
	}
	g.export_entries = g.gen_export_entries()
	g.materialize_text()
	g.gen_post_pass()
}

fn arm64_function_ranges(m &ssa.Module, wanted int) []Arm64GenRange {
	mut weights := []int{len: m.funcs.len, init: 1}
	mut total := 0
	for fi, function in m.funcs {
		mut weight := 1
		for block in function.blocks {
			weight += m.blocks[block].instrs.len
		}
		weights[fi] = weight
		total += weight
	}
	worker_count := arm64_min_int(wanted, m.funcs.len)
	mut ranges := []Arm64GenRange{cap: worker_count}
	mut start := 0
	mut accumulated := 0
	mut range_weight := 0
	for fi, weight in weights {
		accumulated += weight
		range_weight += weight
		remaining_workers := worker_count - ranges.len
		remaining_functions := m.funcs.len - fi - 1
		target := total / worker_count * (ranges.len + 1)
		if ranges.len + 1 < worker_count && remaining_functions >= remaining_workers - 1
			&& accumulated >= target {
			ranges << Arm64GenRange{
				start: start
				end:   fi + 1
				cap:   range_weight * arm64_words_per_instruction + 1024
			}
			start = fi + 1
			range_weight = 0
		}
	}
	if start < m.funcs.len {
		ranges << Arm64GenRange{
			start: start
			end:   m.funcs.len
			cap:   range_weight * arm64_words_per_instruction + 1024
		}
	}
	return ranges
}

// The value and block tables hold the ids of one function at a time, but a
// function's ids can span most of the module: parameters are numbered before
// any body and inlined code after. Their memory comes from zeroed_table_alloc,
// which the operating system zeroes on first touch, so a worker only pays for the
// ids it reaches; a filled array per worker cost 2 GB of RSS and about 10% of
// the phase.
fn zeroed_int_table(len int) []int {
	mut table := []int{}
	unsafe {
		table.data = zeroed_table_alloc(len * int(sizeof(int)))
		table.len = len
		table.cap = len
	}
	return table
}

fn zeroed_i32_table(len int) []i32 {
	mut table := []i32{}
	unsafe {
		table.data = zeroed_table_alloc(len * 4)
		table.len = len
		table.cap = len
	}
	return table
}

fn zeroed_u8_table(len int) []u8 {
	mut table := []u8{}
	unsafe {
		table.data = zeroed_table_alloc(len)
		table.len = len
		table.cap = len
	}
	return table
}

// free_tables releases the tables a worker allocated.
fn (mut g Gen) free_tables() {
	free_zeroed_table(g.stack_offsets.data, g.stack_offsets.len * 4)
	free_zeroed_table(g.alloca_offsets.data, g.alloca_offsets.len * 4)
	free_zeroed_table(g.alloca_sizes.data, g.alloca_sizes.len * 4)
	free_zeroed_table(g.alloca_alignments.data, g.alloca_alignments.len * 4)
	free_zeroed_table(g.block_offsets.data, g.block_offsets.len * 4)
	free_zeroed_table(g.value_homes.data, g.value_homes.len * 1)
	free_zeroed_table(g.ra_interval.data, g.ra_interval.len * int(sizeof(int)))
	free_zeroed_table(g.ra_value_block.data, g.ra_value_block.len * int(sizeof(int)))
	free_zeroed_table(g.ra_block_index.data, g.ra_block_index.len * int(sizeof(int)))
}

// arm64_gen_worker generates ranges until none is left. Its Gen, and the
// register allocation tables in it, outlives the ranges it generates.
fn arm64_gen_worker(arg voidptr) voidptr {
	mut batch := unsafe { &Arm64GenBatch(arg) }
	mut g := Gen.new(batch.parent.m)
	g.freestanding = batch.parent.freestanding
	g.gc_runtime = batch.parent.gc_runtime
	unsafe {
		g.global_index = batch.parent.global_index
	}
	g.global_index_ready = batch.parent.global_index_ready
	for {
		index := int(C.atomic_fetch_add_u32(&batch.next, 1))
		if index >= batch.ranges.len {
			break
		}
		r := batch.ranges[index]
		g.begin_gen_chunk(r.cap)
		for fi in r.start .. r.end {
			g.gen_func(fi)
		}
		batch.chunks[index] = Arm64GenChunk{
			text_words: g.text_words
			macho:      g.macho
			strings:    g.chunk_strings
		}
	}
	g.free_tables()
	return unsafe { nil }
}

fn (mut g Gen) begin_gen_chunk(text_capacity int) {
	g.macho = MachOObject.new()
	g.text_words = []u32{cap: text_capacity}
	g.fn_offsets = map[string]int{}
	g.string_cache = map[string]int{}
	g.chunk_strings = []string{}
}

fn (mut g Gen) reserve_merged_output(chunks []Arm64GenChunk) {
	mut words := 0
	mut relocs := 0
	for chunk in chunks {
		words += chunk.text_words.len
		relocs += chunk.macho.relocs.len
	}
	g.text_words = []u32{cap: words}
	g.macho.relocs = []RelocationInfo{cap: relocs}
}

// merge_gen_chunk appends a chunk to the object. A chunk's strings are looked up
// by content so a literal used by several chunks is stored once, at the offset
// it would have in sequential generation; the chunk's undefined symbols resolve
// to the definitions merged so far.
fn (mut g Gen) merge_gen_chunk(chunk Arm64GenChunk) {
	text_base := g.text_offset()
	mut symbol_map := []int{len: chunk.macho.symbols.len}
	mut next_string := 0
	for index, symbol in chunk.macho.symbols {
		if symbol.sect == 0 {
			symbol_map[index] = g.macho.add_undefined(symbol.name)
		} else if symbol.sect == 2 {
			symbol_map[index] = g.string_symbol(chunk.strings[next_string])
			next_string++
		} else {
			value := if symbol.sect == 1 { symbol.value + u64(text_base) } else { symbol.value }
			symbol_map[index] = g.macho.add_symbol(symbol.name, value, symbol.type_ & n_ext != 0,
				symbol.sect)
		}
	}
	for name, pairs in chunk.macho.unwind_saved_pairs {
		g.macho.unwind_saved_pairs[name] = pairs
	}
	for relocation in chunk.macho.relocs {
		g.macho.add_reloc(relocation.addr + text_base, symbol_map[relocation.sym_idx], relocation.type_,
			relocation.pcrel)
	}
	g.text_words << chunk.text_words
}
