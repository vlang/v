module types

import os
import time
import v.flat
import v.workers

fn C.atomic_load_u32(voidptr) u32

fn C.atomic_store_u32(voidptr, u32)

// pass2_stream_publish_step is the number of top-level declarations after
// which pass 2 publishes what it recorded to the lanes that follow it.
const pass2_stream_publish_step = 64

// pass2_stream_start_timeout is how long pass 2 waits, where it reads the
// tables of the lanes that follow it, for a lane that the pool has not
// started (nanoseconds).
const pass2_stream_start_timeout = u64(250_000_000)

// Pass2Stream lets pool lanes replay the registrations of pass 2 while the
// collecting thread still records them, instead of in a batch after it: the
// lanes then end with pass 2, and nothing waits for the slowest table.
//
// The collecting thread appends to the registration lists, whose buffers
// have room reserved and do not move, and publishes their lengths now and
// then. A published entry is never written again. A lane (follow_pass2_stream)
// replays the published entries of its list into its own tables, which
// nothing else reads or writes until the lanes are joined. `finished` ends
// the lanes once they have replayed everything.
//
// A list that runs out of room would move its buffer. The collecting thread
// then sets `aborted` and waits until no lane is `busy`: a lane announces
// itself busy before it looks at `aborted` again, so it either sees the abort
// or has finished its entries before the wait ends, and never reads a list
// after it. What the lanes did not replay is replayed after pass 2.
@[heap]
struct Pass2Stream {
mut:
	// The published length of each registration list (pass2_list_*), the end
	// of pass 2 and the abort (atomic).
	published [pass2_lists]u32
	finished  u32
	aborted   u32
	// By group: whether its lane runs, whether it replays entries right now,
	// and how many entries of its list it has replayed (atomic).
	started [pass2_ancillary_groups]u32
	busy    [pass2_ancillary_groups]u32
	done    [pass2_ancillary_groups]u32
	// The lists' buffers.
	data [pass2_lists]voidptr
	// The collecting thread's own: the declarations since it last published,
	// and whether it stopped publishing.
	step    int
	stopped bool
	syncs   int
	// The lane of the receiver method index added a generic receiver method,
	// which invalidates the receiver patterns memoized in the type cache: the
	// collecting thread clears them, since the lanes leave that cache alone
	// (atomic).
	clear_receiver_patterns u32
	// What the lanes that take the functions' names from the preparations
	// built, to install once they are joined.
	visible_mutations &VisibleMutationCache = unsafe { nil }
	c_names           map[string]string
}

struct Pass2StreamArgs {
	tc    voidptr
	group int
	preps []Pass2FnPrep
	a     &flat.FlatAst = unsafe { nil }
	fast  bool
mut:
	public_c_structs &map[u32]bool = unsafe { nil }
	fn_ns            u64
	struct_ns        u64
}

// pass2_stream_wait lets a waiting thread spin for a moment and then sleep.
@[inline]
fn pass2_stream_wait(mut idle &int) {
	unsafe {
		(*idle)++
		if *idle < 2048 {
			C.cpu_relax()
		} else {
			time.sleep(20 * time.microsecond)
		}
	}
}

// follow_pass2_stream replays, on a pool lane, the registrations of the list
// of `group` as pass 2 publishes them.
fn (mut tc TypeChecker) follow_pass2_stream(group int, expected int) {
	mut stream := tc.pass2_stream
	list := pass2_group_list(group)
	mut begun := false
	mut done := 0
	mut idle := 0
	C.atomic_store_u32(&stream.started[group], 1)
	for {
		if C.atomic_load_u32(&stream.aborted) != 0 {
			break
		}
		// Read before the length: pass 2 publishes its last length first.
		finished := C.atomic_load_u32(&stream.finished) != 0
		published := int(C.atomic_load_u32(&stream.published[list]))
		if published > done {
			C.atomic_store_u32(&stream.busy[group], 1)
			if C.atomic_load_u32(&stream.aborted) != 0 {
				C.atomic_store_u32(&stream.busy[group], 0)
				break
			}
			if !begun {
				// Sizing a table writes it too.
				begun = true
				tc.begin_pass2_ancillary_group(group, expected)
			}
			tc.replay_pass2_registrations(group, stream.data[list], done, published)
			done = published
			C.atomic_store_u32(&stream.done[group], u32(done))
			C.atomic_store_u32(&stream.busy[group], 0)
			idle = 0
			continue
		}
		if finished {
			break
		}
		pass2_stream_wait(mut &idle)
	}
}

fn pass2_stream_thread(arg voidptr) voidptr {
	mut a := unsafe { &Pass2StreamArgs(arg) }
	mut tc := unsafe { &TypeChecker(a.tc) }
	mut sw := time.new_stopwatch()
	defer {
		tc.timing_step(mut sw, '  p2 lane ${a.group}')
	}
	mut stream := tc.pass2_stream
	if a.group == pass2_ancillary_groups {
		tc.collect_deprecated_symbols()
	} else if a.group == 6 {
		// A private index: lookups of the collecting thread may add to the
		// checker's own meanwhile.
		mut index := new_visible_mutation_cache()
		tc.pass2_visible_mutations_from_preps(a.preps, mut index)
		stream.visible_mutations = index
	} else if a.group == 10 {
		// A private table, for the same reason.
		stream.c_names = tc.pass2_c_names_from_preps(a.preps)
	} else {
		// A declaration registers about two names; one method in four has a
		// mutable receiver.
		expected := if a.group == 4 { a.preps.len / 4 } else { a.preps.len * 2 }
		tc.follow_pass2_stream(a.group, expected)
	}
	return unsafe { nil }
}

// pass2_serial_stream_thread is the collecting thread's task of the batch:
// pass 2 itself, which then ends the stream.
fn pass2_serial_stream_thread(arg voidptr) voidptr {
	mut a := unsafe { &Pass2StreamArgs(arg) }
	mut tc := unsafe { &TypeChecker(a.tc) }
	mut public_c_structs := unsafe { a.public_c_structs }
	a.fn_ns, a.struct_ns = tc.collect_pass2(a.a, tc.top_level_idx, a.preps, a.fast, mut
		*public_c_structs)
	mut stream := tc.pass2_stream
	if !stream.stopped {
		tc.pass2_stream_publish()
	}
	C.atomic_store_u32(&stream.finished, 1)
	return unsafe { nil }
}

// pass2_stream_usable reports whether pass 2 can run with lanes that follow
// its registrations: there is a lane for every table, and the preparations
// have the names of every function.
fn (tc &TypeChecker) pass2_stream_usable(preps []Pass2FnPrep) bool {
	pool := checker_worker_pool(tc.a)
	return tc.defer_fn_ancillary && tc.defer_fn_params && tc.defer_fn_names && !isnil(pool)
		&& pool.size() > pass2_ancillary_groups && preps.len == tc.top_level_idx.len
		&& os.getenv('V3_NO_PASS2_STREAM') == ''
}

// collect_pass2_streamed is collect_pass2 over every top-level declaration,
// with pool lanes that fill the deferred tables meanwhile (see Pass2Stream)
// and collect the deprecated symbols. It leaves in the registration lists
// what the lanes did not replay, for finish_pass2_ancillary_registrations.
fn (mut tc TypeChecker) collect_pass2_streamed(a &flat.FlatAst, preps []Pass2FnPrep, fast_pass2_registration bool, public_c_structs &map[u32]bool) (u64, u64) {
	pool := checker_worker_pool(a)
	tc.disable_call_flag_bloom()
	mut stream := &Pass2Stream{}
	for list in 0 .. pass2_lists {
		stream.data[list] = tc.pass2_list_data(list)
	}
	tc.pass2_stream = stream
	mut args := []Pass2StreamArgs{cap: pass2_ancillary_groups + 2}
	for group in 0 .. pass2_ancillary_groups + 1 {
		args << Pass2StreamArgs{
			tc:    voidptr(tc)
			group: group
			preps: preps
		}
	}
	args << Pass2StreamArgs{
		tc:               voidptr(tc)
		preps:            preps
		a:                a
		fast:             fast_pass2_registration
		public_c_structs: public_c_structs
	}
	mut tasks := []workers.Task{cap: args.len}
	for i in 0 .. args.len {
		tasks << workers.Task{
			run:        if i == args.len - 1 {
				pass2_serial_stream_thread
			} else {
				pass2_stream_thread
			}
			arg:        unsafe { voidptr(&args[i]) }
			force_sync: i == args.len - 1
		}
	}
	pool.run(tasks)
	tc.pass2_stream = unsafe { nil }
	if tc.verbose {
		eprintln('  [ttime]         p2 lane waits   ${stream.syncs:7}')
	}
	if stream.clear_receiver_patterns != 0 {
		tc.clear_generic_receiver_pattern_cache()
	}
	// The lanes are joined: what they replayed, for the batch after pass 2.
	tc.pass2_streamed = true
	for group in 0 .. pass2_ancillary_groups {
		tc.pass2_replayed[group] = int(stream.done[group])
	}
	tc.install_pass2_names(preps, mut stream)
	return args[args.len - 1].fn_ns, args[args.len - 1].struct_ns
}

// install_pass2_names makes the tables that the lanes built from the
// functions' prepared names the checker's own.
fn (mut tc TypeChecker) install_pass2_names(preps []Pass2FnPrep, mut stream Pass2Stream) {
	if !isnil(tc.visible_mutation_cache) && !isnil(stream.visible_mutations) {
		mut cache := tc.visible_mutation_cache
		if cache.global_decls.len == 0 && cache.decls.len == 0 {
			cache.global_decls = stream.visible_mutations.global_decls.move()
			cache.decls = stream.visible_mutations.decls.move()
		} else {
			// A lookup during pass 2 memoized a declaration, which the index
			// keeps: register the declarations on top of it, as a lane after
			// pass 2 would.
			tc.pass2_visible_mutations_from_preps(preps, mut cache)
		}
	}
	if !isnil(tc.type_cache) {
		// The few names that pass 2 lowered itself meanwhile.
		for name, c_name in tc.type_cache.c_name_entries {
			stream.c_names[name] = c_name
		}
		tc.type_cache.c_name_entries = stream.c_names.move()
	}
}

// pass2_stream_publish lets the lanes replay everything recorded so far.
fn (mut tc TypeChecker) pass2_stream_publish() {
	mut stream := tc.pass2_stream
	for list in 0 .. pass2_lists {
		C.atomic_store_u32(&stream.published[list], u32(tc.pass2_list_len(list)))
	}
}

// pass2_stream_step is pass 2's call before each top-level declaration while
// lanes follow it: it publishes now and then, and stops the lanes before a
// list would outgrow its buffer.
fn (mut tc TypeChecker) pass2_stream_step(node &flat.Node) {
	mut stream := tc.pass2_stream
	if stream.stopped {
		return
	}
	if node.kind in [.fn_decl, .c_fn_decl, .interface_decl] {
		// What one declaration records at most: a function its name, C name
		// and `_str` alias, twice with its unqualified name, and two file
		// notes; an interface a registration of each method.
		methods := if node.kind == .interface_decl { int(node.children_count) } else { 0 }
		if tc.fn_ancillary_registrations.cap - tc.fn_ancillary_registrations.len < 8 + methods
			|| tc.fn_c_variadic_registrations.cap - tc.fn_c_variadic_registrations.len < 4
			|| tc.fn_mut_receiver_registrations.cap - tc.fn_mut_receiver_registrations.len < 2 + methods
			|| tc.fn_ret_text_registrations.cap - tc.fn_ret_text_registrations.len < 4 + methods {
			tc.pass2_stream_abort()
			return
		}
	}
	stream.step++
	if stream.step >= pass2_stream_publish_step {
		stream.step = 0
		tc.pass2_stream_publish()
	}
}

// pass2_stream_abort makes the lanes leave the registration lists alone from
// now on, and returns once none of them reads one.
fn (mut tc TypeChecker) pass2_stream_abort() {
	mut stream := tc.pass2_stream
	stream.stopped = true
	if tc.verbose {
		eprintln('  [ttime]         p2 lanes stopped at ${tc.fn_ancillary_registrations.len} of ${tc.fn_ancillary_registrations.cap} registrations')
	}
	C.atomic_store_u32(&stream.aborted, 1)
	mut idle := 0
	for group in 0 .. pass2_ancillary_groups {
		for C.atomic_load_u32(&stream.busy[group]) != 0 {
			pass2_stream_wait(mut &idle)
		}
	}
}

// pass2_init_reads_no_fn_table reports whether the type of the initializer
// `id` of a global is found without a table of functions: it is made of
// literals, casts, addresses and struct, array and map initializers alone, so
// that nothing in it names, calls or overloads a function. Pass 2 resolves
// such an initializer without waiting for the lanes that fill those tables.
fn pass2_init_reads_no_fn_table(a &flat.FlatAst, id flat.NodeId, depth int) bool {
	if int(id) < 0 || int(id) >= a.nodes.len || depth > 64 {
		return false
	}
	node := a.node(id)
	match node.kind {
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .nil_literal,
		.none_expr, .enum_val, .struct_init, .field_init, .array_literal, .array_init, .map_init,
		.cast_expr, .paren, .block, .expr_stmt {
		}
		.prefix {
			if node.op != .amp {
				return false
			}
		}
		else {
			return false
		}
	}
	for i in 0 .. node.children_count {
		if !pass2_init_reads_no_fn_table(a, a.child(node, i), depth + 1) {
			return false
		}
	}
	return true
}

// pass2_stream_sync returns once the tables of the lanes hold every
// registration recorded so far, and no lane writes one until pass 2 records
// another: what pass 2 resolves next may read those tables.
fn (mut tc TypeChecker) pass2_stream_sync() {
	mut stream := tc.pass2_stream
	stream.syncs++
	if !stream.stopped {
		tc.pass2_stream_publish()
		// A lane that runs catches up in a moment, and an idle pool starts
		// every lane within a few milliseconds even on a loaded host. A lane
		// that has not started long after that waits for a pool that is busy
		// with other work: the collecting thread then goes on without the lanes.
		deadline := time.sys_mono_now() + pass2_stream_start_timeout
		mut idle := 0
		for {
			mut caught_up := true
			mut all_started := true
			for group in 0 .. pass2_ancillary_groups {
				list := pass2_group_list(group)
				if list < 0 {
					continue
				}
				if int(C.atomic_load_u32(&stream.done[group])) < tc.pass2_list_len(list) {
					caught_up = false
					if C.atomic_load_u32(&stream.started[group]) == 0 {
						all_started = false
					}
				}
			}
			if caught_up {
				tc.pass2_stream_clear_receiver_patterns()
				return
			}
			if !all_started && time.sys_mono_now() > deadline {
				break
			}
			pass2_stream_wait(mut &idle)
		}
		tc.pass2_stream_abort()
	}
	// The lanes stopped: the collecting thread replays the rest itself.
	for group in 0 .. pass2_ancillary_groups {
		list := pass2_group_list(group)
		if list < 0 {
			continue
		}
		done := int(C.atomic_load_u32(&stream.done[group]))
		tc.replay_pass2_registrations(group, tc.pass2_list_data(list), done, tc.pass2_list_len(list))
		C.atomic_store_u32(&stream.done[group], u32(tc.pass2_list_len(list)))
	}
	tc.pass2_stream_clear_receiver_patterns()
}

// pass2_stream_clear_receiver_patterns clears the type cache's receiver
// patterns on the collecting thread, while no lane replays, if the receiver
// method index gained a generic receiver method since it last did.
fn (mut tc TypeChecker) pass2_stream_clear_receiver_patterns() {
	mut stream := tc.pass2_stream
	if C.atomic_load_u32(&stream.clear_receiver_patterns) != 0 {
		C.atomic_store_u32(&stream.clear_receiver_patterns, 0)
		tc.clear_generic_receiver_pattern_cache()
	}
}

// pass2_stream_note_generic_receiver is the receiver method index lane's note
// that it added a generic receiver method.
fn (tc &TypeChecker) pass2_stream_note_generic_receiver() {
	mut stream := tc.pass2_stream
	C.atomic_store_u32(&stream.clear_receiver_patterns, 1)
}
