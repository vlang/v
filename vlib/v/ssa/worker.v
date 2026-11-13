module ssa

import os
import v.flat
import v.types
import v.workers

// Parallel construction of function bodies.
//
// Registering types, globals and signatures stays serial, and every body is then
// built independently, the way Go's compileFunctions (cmd/compile/internal/gc/
// compile.go) hands prepared functions to its backend workers once the phases
// that are not safe to run concurrently are over. Each worker owns a Module and a
// Builder seeded with what registration produced, like Odin's per-module lbModule
// (src/llvm_backend.cpp, lb_generate_procedures_worker_proc). Values, instructions
// and blocks are private to the function that made them, so a function's output
// is one contiguous range of its worker's tables and depends on nothing another
// function did.
//
// Zig's InternPool (src/InternPool.zig, Local and Shard) can publish ids that name
// the thread that made them because they never reach the output. Here ids decide
// how the binary is laid out, so the ranges are instead renumbered after the
// workers finish, walking the functions in source order. Everything a worker
// created that outlives one function (types, externs, stubs, globals) is interned
// the first time that walk meets it, which leaves the result independent of how
// the functions were scheduled.

fn C.atomic_fetch_add_u32(voidptr, u32) u32

// FunctionJob is one source function body to build.
struct FunctionJob {
	node_index  int
	module_name string
	func_id     int
mut:
	weight int // AST nodes in the body; bigger functions are started first
}

// FunctionFragment locates one built function in its worker's tables.
struct FunctionFragment {
mut:
	worker   int = -1
	value_lo int
	value_hi int
	instr_lo int
	instr_hi int
	block_lo int
	block_hi int
	func_lo  int
	func_hi  int
	error_lo int
	error_hi int
}

// DeferredInterfaceStub is a dispatch stub a worker declared. Its body names
// every implementation of the interface, so it is generated once, by the merge.
struct DeferredInterfaceStub {
	name        string
	base_id     flat.NodeId
	method      string
	module_name string
}

enum LazyTypeKind {
	sum_type
	interface_box
	generic_struct
	multi_return
	option
	wide_integer
}

// LazyTypeRequest remembers how a worker materialized a named type, so the merge
// can ask the main builder for the same type instead of copying the worker's.
struct LazyTypeRequest {
	kind         LazyTypeKind
	name_id      u32
	name         string
	module_name  string
	multi_return types.MultiReturn
	base_typ     TypeID
	is_result    bool
}

@[heap]
struct FunctionWorkerShared {
	main  &Builder
	jobs  []FunctionJob
	order []int
mut:
	fragments []FunctionFragment
	next_job  u32
}

@[heap]
struct FunctionWorker {
	index int
	tc    &types.TypeChecker
mut:
	shared &FunctionWorkerShared = unsafe { nil }
	b      &Builder              = unsafe { nil }
	// Merge state: worker-local id -> final id. Zero (or -1 for functions and
	// globals, whose id 0 is valid) means not interned yet.
	type_remap        []TypeID
	func_remap        []int
	global_remap      []int
	has_deferred_stub []bool
	seed_types        int
	seed_funcs        int
	seed_globals      int
	seed_values       int
	seed_blocks       int
	seed_asm          int
}

// new_worker_module creates the private Module of one function worker. It starts
// from the registered types, functions and globals; the seed tables are small
// because every body is still to come.
fn (m &Module) new_worker_module() &Module {
	mut wts := TypeStore{
		types: []Type{cap: m.type_store.types.len + 1024}
		cache: m.type_store.cache.clone()
	}
	for t in m.type_store.types {
		wts.types << t
	}
	mut w := &Module{
		name:               m.name
		target:             m.target
		track_uses:         false
		type_store:         wts
		funcs:              []Function{cap: m.funcs.len + 64}
		globals:            []GlobalVar{cap: m.globals.len + 16}
		values:             []Value{cap: m.values.len + 64 * 1024}
		instrs:             []Instruction{cap: m.instrs.len + 64 * 1024}
		blocks:             []BasicBlock{cap: m.blocks.len + 8 * 1024}
		asm_specs:          []InlineAsmSpec{cap: m.asm_specs.len}
		export_c_names:     m.export_c_names.clone()
		func_index:         m.func_index.clone()
		const_cache:        unsafe { m.const_cache }
		is_function_worker: true
		seed_value_end:     m.values.len
		c_struct_names:     unsafe { m.c_struct_names }
		c_typedef_structs:  unsafe { m.c_typedef_structs }
		c_extern_symbols:   unsafe { m.c_extern_symbols }
		export_aliases:     unsafe { m.export_aliases }
		noinline_fns:       unsafe { m.noinline_fns }
	}
	for f in m.funcs {
		w.funcs << f
	}
	for g in m.globals {
		w.globals << g
	}
	for v in m.values {
		w.values << v
	}
	for instr in m.instrs {
		w.instrs << instr
	}
	for block in m.blocks {
		w.blocks << block
	}
	for spec in m.asm_specs {
		w.asm_specs << spec
	}
	return w
}

// fork_function_worker copies the builder for one worker. Tables that bodies only
// read stay shared; the ones a body may extend get private copies.
fn (main &Builder) fork_function_worker(tc &types.TypeChecker) &Builder {
	mut w := &Builder{}
	unsafe { vmemcpy(w, main, int(sizeof(Builder))) }
	w.m = main.m.new_worker_module()
	w.tc = tc
	w.is_worker = true
	w.seed_fn_ids = unsafe { main.fn_ids }
	w.spawn_counter = 0
	w.deferred_stubs = map[int]DeferredInterfaceStub{}
	w.type_requests = map[int]LazyTypeRequest{}
	w.fn_ids = main.fn_ids.clone()
	w.fn_types = main.fn_types.clone()
	w.c_fn_ids = main.c_fn_ids.clone()
	w.c_fn_types = main.c_fn_types.clone()
	w.runtime_fn_ids = main.runtime_fn_ids.clone()
	w.runtime_fn_types = main.runtime_fn_types.clone()
	w.struct_types = main.struct_types.clone()
	w.multi_return_types = main.multi_return_types.clone()
	w.multi_return_field_types = main.multi_return_field_types.clone()
	w.ssa_type_names = main.ssa_type_names.clone()
	w.interface_types = main.interface_types.clone()
	w.interface_type_names = main.interface_type_names.clone()
	w.sum_types_registering = main.sum_types_registering.clone()
	w.struct_embeds = main.struct_embeds.clone()
	w.option_types = main.option_types.clone()
	w.option_types_by_payload = main.option_types_by_payload.clone()
	w.var_templates = main.var_templates.clone()
	w.call_candidates = map[string]string{}
	w.sum_by_ssa_type = map[TypeID]u32{}
	w.sum_by_ssa_type_sums = -1
	w.sum_by_ssa_type_structs = -1
	mut field_types := main.struct_field_types.clone()
	w.struct_field_types = flat.TypeTextBindings{
		texts: &flat.TypeTextTable{}
		ids:   field_types.ids
		base:  field_types.base
	}
	// Per-function state: no storage may be shared with the main builder.
	w.vars = VarTable{}
	w.var_type_names = flat.TypeTextBindings{
		texts: &flat.TypeTextTable{}
	}
	w.shadowed_bindings = []ShadowedBinding{}
	w.mut_pointer_params = map[string]TypeID{}
	w.mut_param_names = map[string]bool{}
	w.sum_guard_variants = map[ValueID]u32{}
	w.label_blocks = map[string]BlockID{}
	w.break_labels = map[string]BlockID{}
	w.continue_labels = map[string]BlockID{}
	w.defer_body_ids = []flat.NodeId{}
	w.defer_active_slots = map[flat.NodeId]ValueID{}
	w.loop_targets = []LoopTargets{}
	w.break_targets = []BlockID{}
	w.continue_targets = []BlockID{}
	return w
}

// function_worker_count is the number of threads that build bodies; zero selects
// the serial path. A pool of any size, even none, takes the worker path, so the
// output never depends on the machine (compare Go's `-c=1`).
fn (b &Builder) function_worker_count(job_count int) int {
	if b.m.track_uses || job_count == 0 {
		return 0
	}
	mut limit := 1
	$if !v3_no_parallel ? {
		if !isnil(b.a.worker_pool) {
			limit = b.a.worker_pool.size() + 1
		}
	}
	requested := os.getenv('V3_SSA_JOBS').int()
	if requested > 0 && requested < limit {
		limit = requested
	}
	return if limit < job_count { limit } else { job_count }
}

// function_job_order returns the order to start jobs in: longest first, as
// compileFunctions sorts its queue so the slowest functions do not end up alone
// at the end. Output does not depend on it, which V3_SSA_SHUFFLE exercises by
// randomizing the order, as compileFunctions does under the race detector.
fn function_job_order(jobs []FunctionJob) []int {
	mut order := []int{len: jobs.len, init: index}
	seed := os.getenv('V3_SSA_SHUFFLE').u64()
	if seed != 0 {
		mut state := seed
		for i := order.len - 1; i > 0; i-- {
			state = state * 6364136223846793005 + 1442695040888963407
			j := int((state >> 33) % u64(i + 1))
			order[i], order[j] = order[j], order[i]
		}
		return order
	}
	order.sort_with_compare(fn [jobs] (a &int, b &int) int {
		if jobs[*a].weight != jobs[*b].weight {
			return if jobs[*a].weight > jobs[*b].weight { -1 } else { 1 }
		}
		return if *a < *b { -1 } else { 1 }
	})
	return order
}

// run_function_jobs builds the bodies of `jobs`, which are in source order.
fn (mut b Builder) run_function_jobs(mut jobs []FunctionJob) {
	thread_count := b.function_worker_count(jobs.len)
	if thread_count == 0 {
		for job in jobs {
			b.build_function(b.a.nodes[job.node_index], job.module_name)
		}
		return
	}
	for i in 0 .. jobs.len {
		next_node := if i + 1 < jobs.len { jobs[i + 1].node_index } else { b.a.nodes.len }
		jobs[i].weight = next_node - jobs[i].node_index
	}
	b.prepare_worker_var_templates(jobs)
	mut shared_state := &FunctionWorkerShared{
		main:      unsafe { b }
		jobs:      jobs
		order:     function_job_order(jobs)
		fragments: []FunctionFragment{len: jobs.len}
	}
	mut pool_workers := []&FunctionWorker{cap: thread_count}
	mut tasks := []workers.Task{cap: thread_count}
	for i in 0 .. thread_count {
		worker_tc := if isnil(b.tc) {
			unsafe { &types.TypeChecker(nil) }
		} else {
			b.tc.fork_for_parallel_codegen()
		}
		mut worker := &FunctionWorker{
			index:  i
			tc:     worker_tc
			shared: shared_state
		}
		pool_workers << worker
		tasks << workers.Task{
			run:        run_function_worker
			arg:        voidptr(worker)
			force_sync: i == 0
		}
	}
	if thread_count == 1 {
		run_function_worker(tasks[0].arg)
	} else {
		mut pool := unsafe { b.a.worker_pool }
		pool.run(tasks)
	}
	b.merge_function_workers(jobs, shared_state.fragments, mut pool_workers)
}

fn run_function_worker(arg voidptr) voidptr {
	mut worker := unsafe { &FunctionWorker(arg) }
	mut shared_state := worker.shared
	worker.b = shared_state.main.fork_function_worker(worker.tc)
	worker.seed_types = worker.b.m.type_store.types.len
	worker.seed_funcs = worker.b.m.funcs.len
	worker.seed_globals = worker.b.m.globals.len
	worker.seed_values = worker.b.m.values.len
	worker.seed_blocks = worker.b.m.blocks.len
	worker.seed_asm = worker.b.m.asm_specs.len
	worker.b.seed_types = worker.seed_types
	for {
		next := int(C.atomic_fetch_add_u32(voidptr(&shared_state.next_job), 1))
		if next >= shared_state.order.len {
			break
		}
		job_index := shared_state.order[next]
		shared_state.fragments[job_index] = worker.build_job(shared_state.jobs[job_index])
	}
	return unsafe { nil }
}

fn (mut worker FunctionWorker) build_job(job FunctionJob) FunctionFragment {
	mut b := worker.b
	mut m := b.m
	mut fragment := FunctionFragment{
		worker:   worker.index
		value_lo: m.values.len
		instr_lo: m.instrs.len
		block_lo: m.blocks.len
		func_lo:  m.funcs.len
		error_lo: m.errors.len
	}
	m.fragment_value_start = m.values.len
	m.function_consts.clear()
	b.spawn_counter = 0
	b.build_function(b.a.nodes[job.node_index], job.module_name)
	if m.target.ptr_size == 8 {
		// Per function work that only touches the function's own range belongs
		// in the worker instead of a serial pass over the whole program.
		b.materialize_function_string_literals(m.funcs[job.func_id])
		for func_index in fragment.func_lo .. m.funcs.len {
			b.materialize_function_string_literals(m.funcs[func_index])
		}
	}
	fragment.value_hi = m.values.len
	fragment.instr_hi = m.instrs.len
	fragment.block_hi = m.blocks.len
	fragment.func_hi = m.funcs.len
	fragment.error_hi = m.errors.len
	return fragment
}

// prepare_worker_var_templates builds the module scopes while the program is
// still serial; workers share them read-only.
fn (mut b Builder) prepare_worker_var_templates(jobs []FunctionJob) {
	saved_module := b.cur_module
	for job in jobs {
		if job.module_name !in b.var_templates {
			b.cur_module = job.module_name
			b.module_var_template()
		}
	}
	b.cur_module = saved_module
}

// note_lazy_type records how a worker created a named type.
fn (mut b Builder) note_lazy_type(typ TypeID, request LazyTypeRequest) {
	if b.is_worker && typ >= b.seed_types && typ !in b.type_requests {
		b.type_requests[typ] = request
	}
}

// merge_function_workers renumbers each function's range into the main module.
// The walk is in source order and interns before it copies, so symbols appear in
// the order the program first uses them, whichever worker built what.
fn (mut b Builder) merge_function_workers(jobs []FunctionJob, fragments []FunctionFragment, mut pool_workers []&FunctionWorker) {
	for mut worker in pool_workers {
		if isnil(worker.b) {
			continue
		}
		worker.type_remap = []TypeID{len: worker.b.m.type_store.types.len}
		worker.func_remap = []int{len: worker.b.m.funcs.len, init: -1}
		worker.global_remap = []int{len: worker.b.m.globals.len, init: -1}
		worker.has_deferred_stub = []bool{len: worker.b.m.funcs.len}
		for func_id, _ in worker.b.deferred_stubs {
			worker.has_deferred_stub[func_id] = true
		}
	}
	mut anonymous_types := map[string]TypeID{}
	saved_module := b.cur_module
	for fragment in fragments {
		mut worker := pool_workers[fragment.worker]
		b.intern_fragment_symbols(mut worker, fragment, mut anonymous_types)
	}
	b.cur_module = saved_module
	// Everything that can append to the main tables has run: lay the fragments
	// out and fill them concurrently.
	mut plans := []FragmentPlan{len: jobs.len}
	mut values_end := b.m.values.len
	mut instrs_end := b.m.instrs.len
	mut blocks_end := b.m.blocks.len
	for i, fragment in fragments {
		plans[i] = FragmentPlan{
			value_off: values_end - fragment.value_lo
			instr_off: instrs_end - fragment.instr_lo
			block_off: blocks_end - fragment.block_lo
		}
		values_end += fragment.value_hi - fragment.value_lo
		instrs_end += fragment.instr_hi - fragment.instr_lo
		blocks_end += fragment.block_hi - fragment.block_lo
	}
	unsafe {
		b.m.values.grow_len(values_end - b.m.values.len)
		b.m.instrs.grow_len(instrs_end - b.m.instrs.len)
		b.m.blocks.grow_len(blocks_end - b.m.blocks.len)
	}
	b.materialized_functions = []bool{len: b.m.funcs.len}
	mut copy_state := &FragmentCopyState{
		main:         unsafe { b }
		jobs:         jobs
		fragments:    fragments
		plans:        plans
		pool_workers: pool_workers
	}
	thread_count := pool_workers.len
	if thread_count == 1 {
		run_fragment_copy(voidptr(copy_state))
	} else {
		mut tasks := []workers.Task{cap: thread_count}
		for i in 0 .. thread_count {
			tasks << workers.Task{
				run:        run_fragment_copy
				arg:        voidptr(copy_state)
				force_sync: i == 0
			}
		}
		mut pool := unsafe { b.a.worker_pool }
		pool.run(tasks)
	}
	for i, fragment in fragments {
		worker := pool_workers[fragment.worker]
		for error_index in fragment.error_lo .. fragment.error_hi {
			b.m.errors << worker.b.m.errors[error_index]
		}
		b.materialized_functions[jobs[i].func_id] = true
		for func_index in fragment.func_lo .. fragment.func_hi {
			if worker.b.m.funcs[func_index].blocks.len > 0 {
				b.materialized_functions[worker.func_remap[func_index]] = true
			}
		}
	}
	// Shadow bindings are additive and independent of function order.
	mut added_shadows := map[string]string{}
	for worker in pool_workers {
		if !isnil(worker.b) {
			for name, shadow in worker.b.m.export_c_names {
				if name !in b.m.export_c_names {
					added_shadows[name] = shadow
				}
			}
		}
	}
	mut shadow_names := added_shadows.keys()
	shadow_names.sort()
	for name in shadow_names {
		b.m.export_c_names[name] = added_shadows[name]
	}
}

struct FragmentPlan {
	value_off int
	instr_off int
	block_off int
}

@[heap]
struct FragmentCopyState {
	main         &Builder
	jobs         []FunctionJob
	fragments    []FunctionFragment
	plans        []FragmentPlan
	pool_workers []&FunctionWorker
mut:
	next u32
}

const fragment_copy_chunk = 128

fn run_fragment_copy(arg voidptr) voidptr {
	mut state := unsafe { &FragmentCopyState(arg) }
	mut main := unsafe { &Builder(state.main) }
	for {
		start := int(C.atomic_fetch_add_u32(voidptr(&state.next), fragment_copy_chunk))
		if start >= state.jobs.len {
			break
		}
		end := if start + fragment_copy_chunk < state.jobs.len {
			start + fragment_copy_chunk
		} else {
			state.jobs.len
		}
		for i in start .. end {
			fragment := state.fragments[i]
			mut worker := state.pool_workers[fragment.worker]
			main.copy_fragment(mut worker, fragment, state.jobs[i], state.plans[i])
		}
	}
	return unsafe { nil }
}

// intern_fragment_symbols resolves everything the fragment names outside itself.
// It runs before the fragment's final position is chosen, because generating an
// interface stub appends to the main tables.
@[direct_array_access]
fn (mut b Builder) intern_fragment_symbols(mut worker FunctionWorker, fragment FunctionFragment, mut anonymous_types map[string]TypeID) {
	values := worker.b.m.values
	instrs := worker.b.m.instrs
	for i in fragment.value_lo .. fragment.value_hi {
		value := values[i]
		if value.typ >= worker.seed_types {
			b.intern_worker_type(mut worker, value.typ, mut anonymous_types)
		}
		if value.kind == .func_ref && (value.index >= worker.seed_funcs
			|| worker.has_deferred_stub[value.index]) {
			b.intern_worker_function(mut worker, value.index, mut anonymous_types)
		} else if value.kind == .global && value.index >= worker.seed_globals {
			b.intern_worker_global(mut worker, value.index, mut anonymous_types)
		}
	}
	mut worker_module := worker.b.m
	for i in fragment.instr_lo .. fragment.instr_hi {
		if instrs[i].typ >= worker.seed_types {
			b.intern_worker_type(mut worker, instrs[i].typ, mut anonymous_types)
		}
		if instrs[i].asm_id > worker.seed_asm {
			worker_module.instrs[i].asm_id = b.m.add_inline_asm_spec(worker_module.asm_specs[instrs[i].asm_id - 1])
		}
	}
	// Synthetic functions with a body live in this fragment even when nothing
	// here references them.
	for func_index in fragment.func_lo .. fragment.func_hi {
		if worker.b.m.funcs[func_index].blocks.len > 0 {
			b.intern_worker_function(mut worker, func_index, mut anonymous_types)
		}
	}
}

// intern_worker_type maps a worker's type to the main store. A named type is
// asked of the main builder, the way a worker asked its own; anything else is
// interned by structure over already-final ids.
fn (mut b Builder) intern_worker_type(mut worker FunctionWorker, local TypeID, mut anonymous_types map[string]TypeID) TypeID {
	if local < worker.seed_types {
		return local
	}
	remapped := worker.type_remap[local]
	if remapped != 0 {
		return remapped
	}
	mut final := TypeID(0)
	if request := worker.b.type_requests[local] {
		b.cur_module = request.module_name
		final = match request.kind {
			.sum_type {
				b.register_checker_sum_type(request.name_id)
			}
			.interface_box {
				b.register_checker_interface(request.name_id) or { TypeID(0) }
			}
			.generic_struct {
				b.register_checker_generic_struct(request.name) or { TypeID(0) }
			}
			.multi_return {
				b.register_multi_return_type(request.multi_return)
			}
			.option {
				payload := b.intern_worker_type(mut worker, request.base_typ, mut anonymous_types)
				b.option_type_id_with_payload(request.name_id, payload, request.is_result)
			}
			.wide_integer {
				b.wide_integer_type()
			}
		}
		if final == 0 {
			panic('ssa: main builder could not materialize worker type ${request.kind} ${request.name}')
		}
	} else {
		final = b.intern_structural_type(mut worker, worker.b.m.type_store.types[local], mut
			anonymous_types)
	}
	worker.type_remap[local] = final
	return final
}

fn (mut b Builder) intern_structural_type(mut worker FunctionWorker, t Type, mut anonymous_types map[string]TypeID) TypeID {
	match t.kind {
		.int_t {
			return if t.is_unsigned {
				b.m.type_store.get_uint(t.width)
			} else {
				b.m.type_store.get_int(t.width)
			}
		}
		.float_t {
			return b.m.type_store.get_float(t.width)
		}
		.ptr_t {
			return b.m.type_store.get_ptr(b.intern_worker_type(mut worker, t.elem_type, mut anonymous_types))
		}
		.array_t {
			return b.m.type_store.get_array(b.intern_worker_type(mut worker, t.elem_type, mut anonymous_types),
				t.len)
		}
		else {}
	}
	mut fields := []TypeID{cap: t.fields.len}
	for field in t.fields {
		fields << b.intern_worker_type(mut worker, field, mut anonymous_types)
	}
	mut params := []TypeID{cap: t.params.len}
	for param in t.params {
		params << b.intern_worker_type(mut worker, param, mut anonymous_types)
	}
	elem_type := b.intern_worker_type(mut worker, t.elem_type, mut anonymous_types)
	ret_type := b.intern_worker_type(mut worker, t.ret_type, mut anonymous_types)
	key := '${int(t.kind)}|${t.width}|${t.is_unsigned}|${elem_type}|${t.len}|${fields}|${t.field_names}|${params}|${ret_type}|${t.is_c_struct}|${t.is_union}|${t.is_packed}|${t.alignment}|${t.size}|${t.field_offsets}'
	if existing := anonymous_types[key] {
		return existing
	}
	id := b.m.type_store.register(Type{
		kind:          t.kind
		width:         t.width
		is_unsigned:   t.is_unsigned
		elem_type:     elem_type
		len:           t.len
		fields:        fields
		field_names:   t.field_names
		params:        params
		ret_type:      ret_type
		is_c_struct:   t.is_c_struct
		is_union:      t.is_union
		is_packed:     t.is_packed
		alignment:     t.alignment
		size:          t.size
		field_offsets: t.field_offsets
	})
	anonymous_types[key] = id
	return id
}

fn (mut b Builder) intern_worker_function(mut worker FunctionWorker, local int, mut anonymous_types map[string]TypeID) int {
	if local < worker.seed_funcs && !worker.has_deferred_stub[local] {
		return local
	}
	if worker.func_remap[local] >= 0 {
		return worker.func_remap[local]
	}
	source := worker.b.m.funcs[local]
	if stub := worker.b.deferred_stubs[local] {
		b.cur_module = stub.module_name
		if final := b.register_interface_method_stub(stub.name, stub.base_id, stub.method) {
			worker.func_remap[local] = final
			return final
		}
	}
	if local < worker.seed_funcs {
		worker.func_remap[local] = local
		return local
	}
	final := b.m.new_function(source.name, b.intern_worker_type(mut worker, source.typ, mut
		anonymous_types))
	mut f := b.m.funcs[final]
	f.typ = b.intern_worker_type(mut worker, source.typ, mut anonymous_types)
	f.is_c_extern = source.is_c_extern
	f.is_variadic = source.is_variadic
	f.variadic_start = source.variadic_start
	f.is_prototype = source.is_prototype
	f.linkage = source.linkage
	f.call_conv = source.call_conv
	b.m.funcs[final] = f
	worker.func_remap[local] = final
	return final
}

fn (mut b Builder) intern_worker_global(mut worker FunctionWorker, local int, mut anonymous_types map[string]TypeID) int {
	if local < worker.seed_globals {
		return local
	}
	if worker.global_remap[local] >= 0 {
		return worker.global_remap[local]
	}
	source := worker.b.m.globals[local]
	final_type := b.intern_worker_type(mut worker, source.typ, mut anonymous_types)
	if final := b.merged_global_ids[source.name] {
		worker.global_remap[local] = final
		return final
	}
	mut global := source
	global.typ = final_type
	final := b.m.globals.len
	b.m.globals << global
	b.merged_global_ids[source.name] = final
	// Later serial code looks private constants up by name.
	if source.linkage != .external {
		b.global_vars[source.name] = b.m.add_value(.global, b.m.type_store.get_ptr(final_type), source.name,
			final)
	}
	worker.global_remap[local] = final
	return final
}

// copy_fragment fills one function's reserved range, rewriting ids in place: each
// operand list belongs to exactly one instruction, so nothing needs copying.
@[direct_array_access]
fn (mut b Builder) copy_fragment(mut worker FunctionWorker, fragment FunctionFragment, job FunctionJob, plan FragmentPlan) {
	mut m := b.m
	wm := worker.b.m
	value_off := plan.value_off
	instr_off := plan.instr_off
	block_off := plan.block_off
	seed_values := worker.seed_values
	seed_blocks := worker.seed_blocks
	seed_types := worker.seed_types
	type_remap := worker.type_remap
	for i in fragment.value_lo .. fragment.value_hi {
		mut value := wm.values[i]
		value.id = i + value_off
		if value.typ >= seed_types {
			value.typ = type_remap[value.typ]
		}
		match value.kind {
			.instruction {
				value.index += instr_off
			}
			.basic_block {
				value.index += block_off
			}
			.func_ref {
				if value.index >= worker.seed_funcs {
					value.index = worker.func_remap[value.index]
				}
			}
			.global {
				if value.index >= worker.seed_globals {
					value.index = worker.global_remap[value.index]
				}
			}
			else {}
		}
		m.values[i + value_off] = value
	}
	for i in fragment.instr_lo .. fragment.instr_hi {
		mut instr := wm.instrs[i]
		for operand in 0 .. instr.operands.len {
			id := instr.operands[operand]
			if instr.is_block_operand(operand) {
				if id >= seed_blocks {
					instr.operands[operand] = id + block_off
				}
			} else if id >= seed_values {
				instr.operands[operand] = id + value_off
			}
		}
		if instr.block >= seed_blocks {
			instr.block += block_off
		}
		if instr.typ >= seed_types {
			instr.typ = type_remap[instr.typ]
		}
		m.instrs[i + instr_off] = instr
	}
	for i in fragment.block_lo .. fragment.block_hi {
		mut block := wm.blocks[i]
		block.id += block_off
		if block.val_id >= seed_values {
			block.val_id += value_off
		}
		if block.parent >= worker.seed_funcs {
			block.parent = worker.func_remap[block.parent]
		}
		for j in 0 .. block.instrs.len {
			if block.instrs[j] >= seed_values {
				block.instrs[j] += value_off
			}
		}
		for j in 0 .. block.preds.len {
			if block.preds[j] >= seed_blocks {
				block.preds[j] += block_off
			}
		}
		for j in 0 .. block.succs.len {
			if block.succs[j] >= seed_blocks {
				block.succs[j] += block_off
			}
		}
		m.blocks[i + block_off] = block
	}
	b.install_built_function(wm.funcs[job.func_id], job.func_id, value_off, block_off, seed_values,
		seed_blocks)
	for func_index in fragment.func_lo .. fragment.func_hi {
		source := wm.funcs[func_index]
		if source.blocks.len > 0 {
			b.install_built_function(source, worker.func_remap[func_index], value_off, block_off,
				seed_values, seed_blocks)
		}
	}
}

@[direct_array_access]
fn (mut b Builder) install_built_function(source Function, final int, value_off int, block_off int, seed_values int, seed_blocks int) {
	mut f := b.m.funcs[final]
	f.blocks = []BlockID{cap: source.blocks.len}
	for block in source.blocks {
		f.blocks << if block >= seed_blocks { block + block_off } else { block }
	}
	f.params = []ValueID{cap: source.params.len}
	for param in source.params {
		f.params << if param >= seed_values { param + value_off } else { param }
	}
	f.is_prototype = source.is_prototype
	b.m.funcs[final] = f
}
