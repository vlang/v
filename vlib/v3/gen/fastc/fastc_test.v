module fastc

import os
import strings
import v3.cmdexec
import v3.pref
import v3.scanner
import v3.token

fn test_parse_resolve_memo_round_trip() {
	text := 'D\t/vlib/builtin\t100\t200\t300\t400\t2\t/vlib/builtin/a.v\t/vlib/builtin/b.v\n' + 'D\t/vlib/plain\n' + 'E\tmain.v\t/abs/main.v\t10\t20\t30\t40\t/abs\t1\t/abs/other.v\n' + 'F\t/vlib/builtin/a.v\t5\t6\t7\t8\t41\n' + 'M\tos\t/abs/main.v\n' + 'T\t1234\n' + 'B\ttoken\n'
	memo := fastc_parse_resolve_memo(text)
	assert memo.dirs == ['/vlib/builtin', '/vlib/plain']
	assert memo.dir_stamps.len == 2
	assert memo.dir_stamps[0] == FastcFileStamp{
		size: 100
		mtime: 200
		ctime: 300
		inode: 400
	}
	assert memo.dir_files[0] == ['/vlib/builtin/a.v', '/vlib/builtin/b.v']
	assert memo.dir_stamps[1].mtime == 0
	assert memo.dir_files[1].len == 0
	assert memo.entry_paths == ['main.v']
	assert memo.entry_real_paths == ['/abs/main.v']
	assert memo.entry_stamps[0].inode == 40
	assert memo.entry_vmod_roots == ['/abs']
	assert memo.entry_files[0] == ['/abs/other.v']
	assert memo.files == ['/vlib/builtin/a.v']
	assert memo.stamps[0] == FastcFileStamp{
		size: 5
		mtime: 6
		ctime: 7
		inode: 8
	}
	assert memo.offsets == [41]
	assert memo.lookup_modules == ['os']
	assert memo.lookup_sources == ['/abs/main.v']
	assert memo.written == 1234
	assert memo.blob_token == 'token'
}

fn test_parse_resolve_memo_rejects_short_listings() {
	// A directory line whose file count does not match its fields records no
	// listing, so the directory is listed again.
	memo := fastc_parse_resolve_memo('D\t/vlib/x\t1\t2\t3\t4\t3\t/vlib/x/a.v\n')
	assert memo.dirs == ['/vlib/x']
	assert memo.dir_stamps[0].mtime == 0
	assert memo.dir_files[0].len == 0
}

fn test_fastc_index_of_and_replace() {
	text := 'abc_call(x) + abc_call(y) - ab'
	assert fastc_index_of(text, 'abc_call(', 0) == text.index_after_('abc_call(', 0)
	assert fastc_index_of(text, 'abc_call(', 1) == text.index_after_('abc_call(', 1)
	assert fastc_index_of(text, 'zzz', 0) == -1
	assert fastc_index_of(text, '', 0) == -1
	assert fastc_index_of(text, 'ab', text.len - 2) == text.len - 2
	assert fastc_index_of(text, 'abx', text.len - 2) == -1
	assert fastc_contains(text, '- ab')
	assert !fastc_contains(text, 'abcd')
	assert fastc_replace(text, 'abc_call(', 'f(') == text.replace('abc_call(', 'f(')
	assert fastc_replace('aaa', 'aa', 'b') == 'aaa'.replace('aa', 'b')
	assert fastc_replace(text, 'zzz', 'q') == text
}

fn test_contains_method_marker() {
	// A leading occurrence of the name is not a call; the scan must go on to
	// a later `.name(` or `->name(`.
	assert fastc_contains_method_marker('foo + ptr.foo(1)', 'foo')
	assert fastc_contains_method_marker('foo + ptr->foo(1)', 'foo')
	assert fastc_contains_method_marker('foo(1) + x.foo(2)', 'foo')
	assert fastc_contains_method_marker('bar.foo()', 'foo')
	assert fastc_contains_method_marker('a->foo()', 'foo')
	assert !fastc_contains_method_marker('foo(1)', 'foo')
	assert !fastc_contains_method_marker('foo + foo', 'foo')
	assert !fastc_contains_method_marker('x.foobar(1)', 'foo')
	assert !fastc_contains_method_marker('x.foo', 'foo')
	assert !fastc_contains_method_marker('xfoo(1)', 'foo')
	assert !fastc_contains_method_marker('', 'foo')
	assert !fastc_contains_method_marker('x.foo(1)', '')
}

fn test_selfhost_shared_keyword_local_is_preserved() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Box {
mut:
	value int
}

fn consume(shared value Box) {}

fn id(shared int) int {
	return shared
}

fn pair(shared, other int) int {
	return shared + other
}

fn use_immutable_shared() Box {
	shared := Box{}
	return shared
}

fn xor_shared() int {
	shared := 1
	return shared ^ 2
}

fn unwrap_shared(value ?int) int {
	shared := value
	return shared or { 0 }
}

fn contains_shared() bool {
	shared := 1
	return shared in [1]
}

fn same_line_shared() int { shared := 1; return shared }

fn condition_shared() int {
	shared := true
	if shared {
		return 1
	}
	return 0
}

fn propagate_shared(value ?int) ?int {
	shared := value
	return shared?
}

fn sum_shared(values []int) int {
	mut total := 0
	for shared in values {
		total += shared
	}
	return total
}

fn parallel_shared() int {
	x, shared := 1, 2
	return x + shared
}

fn following_line_shared() int {
	shared := 3
	x := shared
	println(x)
	return x
}

fn nested_comment_shared() int {
	shared := 1
	return shared /* outer /* inner */ tail */ + 1
}

fn multiline_comment_shared() int {
	shared := 1
	return shared /* first
	second */ + 1
}

fn following_keyword_shared() int {
	shared := 3
	x := shared
	if true {
		println(x)
	}
	return x
}

fn multiline_shared_operator(enabled bool) bool {
	shared := true
	return shared
		&& enabled
}

fn main() {
	_ = id(1)
	_ = pair(1, 2)
	_ = use_immutable_shared()
	_ = xor_shared()
	_ = unwrap_shared(1)
	_ = contains_shared()
	_ = same_line_shared()
	_ = condition_shared()
	_ = propagate_shared(1)
	_ = sum_shared([1, 2])
	_ = parallel_shared()
	_ = following_line_shared()
	_ = nested_comment_shared()
	_ = multiline_comment_shared()
	_ = following_keyword_shared()
	_ = multiline_shared_operator(true)
	unsafe {
		mut shared := Box{}
		mut type := Box{}
		value := Box{}
		shared.value = 1
		println(shared.value)
		consume(shared shared)
		consume(shared (shared))
		consume(shared /* ownership */ value)
		consume(shared
			value)
		consume(shared // ownership
			value)
		consume(shared
			shared)
		consume(shared
			type)
	}
}
', 'shared_keyword_local.v', prefs) or { panic(err) }
	assert c_source.contains('shared.value=1;'), c_source
	assert c_source.contains('i64 id(i64 shared);'), c_source
	assert c_source.contains('i64 id(i64 shared) {'), c_source
	assert c_source.contains('println(shared.value)'), c_source
	assert c_source.contains('consume(&(shared));'), c_source
	assert c_source.contains('consume(&((shared)));'), c_source
	assert c_source.contains('consume(&(value));'), c_source
	assert c_source.count('consume(&(value));') == 3, c_source
	assert c_source.count('consume(&(shared));') == 2, c_source
	assert c_source.contains('consume(&(type));'), c_source
	assert c_source.contains('__typeof__(((Box){})) shared = ((Box){});'), c_source
	assert c_source.contains('return shared;'), c_source
	assert c_source.contains('i64 pair(i64 shared, i64 other)'), c_source
	assert c_source.contains('return shared+other;'), c_source
	assert c_source.contains('return shared^2;'), c_source
	assert !c_source.contains('return &shared'), c_source
	assert c_source.contains('Option __v_fastc_option_0 = (shared);'), c_source
	assert c_source.contains('__v_fastc_membership_item = (shared);'), c_source
	assert c_source.count('return shared;') == 3, c_source
	assert c_source.contains('if (shared)'), c_source
	assert c_source.contains('total+=shared;'), c_source
	assert c_source.contains('return x+shared;'), c_source
	assert c_source.count('return shared+1;') == 2, c_source
	assert c_source.count('println(x);') == 2, c_source
	assert c_source.contains('return ((shared)&&(enabled));'), c_source
	assert !c_source.contains('(&shared)'), c_source
}

fn test_selfhost_shared_pointer_local_modifier_is_not_addressed_again() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn consume_mut(mut value int) {}

fn consume_shared(shared value int) {}

fn main() {
	unsafe {
		mut value := 1
		mut shared := &value
		consume_mut(mut shared)
		consume_shared(shared shared)
	}
}
', 'shared_pointer_local_modifier.v', prefs) or { panic(err) }
	assert c_source.contains('consume_mut(shared);'), c_source
	assert c_source.contains('consume_shared(shared);'), c_source
	assert !c_source.contains('consume_mut(&(shared));'), c_source
	assert !c_source.contains('consume_shared(&(shared));'), c_source
}

fn test_selfhost_grouped_keyword_parameter_names_are_collected() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut functions := map[string]FastcFunctionSignature{}
	collect_function_signatures('fn pair(shared, type int) int {}', 'grouped_keyword_parameter_names.v', FastcSourceHeader{ module_name: 'main' }, prefs, []int{}, map[string]bool{}, map[string]string{}, map[string]bool{}, mut functions) or { panic(err) }
	signature := functions['pair'] or { panic('missing pair signature') }
	assert signature.parameter_types == ['int', 'int']
}

fn test_selfhost_generic_calls_named_shared_are_monomorphized() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Worker {}

fn shared[T](value T) T {
	return value
}

fn (worker Worker) shared[T](value T) T {
	_ = worker
	return value
}

fn main() {
	worker := Worker{}
	_ := shared[int](1)
	_ := worker.shared[int](2)
}
', 'selfhost_generic_shared_calls.v', prefs) or { panic(err) }
	assert c_source.contains('shared_mono_int'), c_source
	assert c_source.contains('Worker_shared_mono_int'), c_source
	assert !c_source.contains('shared[int]'), c_source
}

fn test_selfhost_implicit_generic_method_named_shared_is_monomorphized() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Worker {}

fn (worker Worker) shared[T](value T) T {
	_ = worker
	return value
}

fn main() {
	worker := Worker{}
	_ := worker.shared(2)
}
', 'selfhost_implicit_generic_shared_method.v', prefs) or { panic(err) }
	assert c_source.contains('Worker_shared_mono_int'), c_source
	assert c_source.contains('Worker_shared_mono_int(worker,2)'), c_source
}

fn test_selfhost_implicit_generic_named_shared_parameter_is_monomorphized() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Worker {}

fn id[T](shared T) T {
	return shared
}

fn (worker Worker) id[T](shared T) T {
	_ = worker
	return shared
}

fn main() {
	worker := Worker{}
	_ := id(1)
	_ := worker.id(2)
}
', 'selfhost_implicit_generic_named_shared_parameter.v', prefs) or { panic(err) }
	assert c_source.contains('id_mono_int(1)'), c_source
	assert c_source.contains('Worker_id_mono_int(worker,2)'), c_source
}

fn test_selfhost_generic_call_from_contextual_shared_parameter_is_monomorphized() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn id[T](value T) T {
	return value
}

fn forward(shared int) int {
	return id(shared)
}

fn main() {
	_ := forward(1)
}
', 'selfhost_generic_call_from_contextual_shared_parameter.v', prefs) or { panic(err) }
	assert c_source.contains('id_mono_int(shared)'), c_source
}

fn test_selfhost_multiline_result_propagation_after_shared_identifier() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn maybe_value() !int {
	return 7
}

fn propagate() !int {
	shared := maybe_value()
	return shared
	!
}

fn main() {
	_ := propagate() or { 0 }
}
', 'selfhost_multiline_shared_result_propagation.v', prefs) or { panic(err) }
	assert c_source.contains('Option __v_fastc_option_propagate = (shared);'), c_source
	assert !c_source.contains('shared!'), c_source
}

fn test_selfhost_mutable_shared_loop_binding_is_dereferenced() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn main() {
	mut values := [1, 2]
	for mut shared in values {
		println(shared + 1)
	}
}
', 'selfhost_mutable_shared_loop_binding.v', prefs) or { panic(err) }
	assert c_source.contains('println((*(shared))+1)'), c_source
	assert !c_source.contains('println(shared+1)'), c_source
}

fn test_selfhost_static_shared_local_is_contextual_identifier() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn main() {
	static shared := 1
	println(shared)
}
', 'selfhost_static_shared_local.v', prefs) or { panic(err) }
	assert c_source.contains('static ${fastc_platform_int_c_type} shared;'), c_source
	assert c_source.contains('static bool __v_fastc_static_init_'), c_source
	assert c_source.contains('shared = (1);'), c_source
	assert c_source.contains('println(shared)'), c_source
}

fn test_selfhost_mut_static_pointer_survives_later_calls() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {}

fn retained(value &Item) &Item {
	mut static saved := unsafe { &Item(nil) }
	if value != unsafe { nil } {
		saved = value
	}
	return saved
}

fn main() {
	item := &Item{}
	_ = retained(item)
	_ = retained(unsafe { nil })
}
', 'selfhost_mut_static_pointer.v', prefs) or { panic(err) }
	assert c_source.contains('static __typeof__((((Item*)(NULL)))) saved;'), c_source
	assert c_source.contains('static bool __v_fastc_static_init_'), c_source
	assert c_source.contains('saved = (((Item*)(NULL)));'), c_source
}

fn test_selfhost_unsafe_contextual_shared_local_mutation() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Box {
mut:
	value int
}

fn main() {
	shared := Box{}
	unsafe {
		shared.value = 1
	}
	println(shared.value)
}
', 'selfhost_unsafe_contextual_shared_local_mutation.v', prefs) or { panic(err) }
	assert c_source.contains('shared.value=1;'), c_source
}

fn test_selfhost_contextual_shared_struct_field_uses_designated_initializer() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Box {
	shared int
}

fn main() {
	box := Box{shared: 1}
	println(box.shared)
}
', 'selfhost_contextual_shared_struct_field.v', prefs) or { panic(err) }
	assert c_source.contains('(Box){.shared='), c_source
	assert !c_source.contains('(Box){shared='), c_source
}

fn test_selfhost_shared_map_field_keeps_identity_across_struct_copy() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Registry {
	values shared map[string]int
}

fn insert(copy Registry) {
	lock copy.values {
		copy.values["answer"] = 42
	}
}

fn main() {
	registry := Registry{}
	insert(registry)
	rlock registry.values {
		println(registry.values["answer"])
	}
}
', 'selfhost_shared_map_field.v', prefs) or { panic(err) }
	assert c_source.contains('Map_string_int* values;'), c_source
	assert c_source.contains('*(copy.values)'), c_source
	assert c_source.contains('*(registry.values)'), c_source
	assert c_source.contains('(Map_string_int*)v_fastc_interface_box'), c_source
}

fn test_fastc_chunk_bounds_reserve_files_for_later_workers() {
	sources := [
		FastcSourceFile{
			source: 'a'.repeat(90)
		},
		FastcSourceFile{
			source: 'b'.repeat(90)
		},
		FastcSourceFile{
			source: 'c'.repeat(90)
		},
		FastcSourceFile{
			source: 'd'.repeat(300)
		},
	]
	assert fastc_chunk_bounds(sources, 2) == [0, 3, 3, 4]
}

fn test_fastc_file_generation_order_is_largest_first() {
	sources := [
		FastcSourceFile{ source: 'a'.repeat(30) },
		FastcSourceFile{ source: 'b'.repeat(60) },
		FastcSourceFile{ source: 'c'.repeat(40) },
		FastcSourceFile{ source: 'd'.repeat(50) },
	]
	// Work-stealing claims files in this order, so the biggest files start before
	// the shared queue's tail thins out.
	assert fastc_file_generation_order(sources) == [1, 3, 2, 0]
}

fn fastc_test_steal_claimed_indices(queue &FastcGenQueue, limit u32) []u32 {
	mut claimed := []u32{}
	for {
		index := fastc_atomic_fetch_add_u32(&queue.next, 1)
		if index >= limit {
			break
		}
		claimed << index
	}
	return claimed
}

fn test_fastc_gen_queue_claims_every_index_exactly_once() {
	// Several workers drain one shared queue concurrently, exactly as
	// fastc_generate_file_outputs spawns them. The atomic counter must hand each
	// index to exactly one worker — the property the work-stealing scheduler relies
	// on to generate every file once — regardless of how the OS interleaves them.
	limit := u32(4000)
	mut queue := &FastcGenQueue{
		next: 0
	}
	mut workers := []thread []u32{}
	for _ in 0 .. 8 {
		workers << spawn fastc_test_steal_claimed_indices(queue, limit)
	}
	mut claim_counts := []int{len: int(limit)}
	mut total := 0
	for worker in workers {
		for index in worker.wait() {
			assert index < limit
			claim_counts[index]++
			total++
		}
	}
	// Every index claimed exactly once, and nothing beyond the end leaks through.
	assert total == int(limit)
	for count in claim_counts {
		assert count == 1
	}
}

fn test_fastc_prealloc_enabled_reads_user_defines() {
	mut with_prealloc := pref.new_preferences()
	with_prealloc.user_defines = ['prealloc']
	assert fastc_prealloc_enabled(with_prealloc)
	assert !fastc_prealloc_enabled(pref.new_preferences())
}

fn test_fastc_prealloc_arena_root_is_per_thread() {
	mut out := strings.new_builder(256)
	fastc_write_prealloc_tls_global(mut out, 'VMemoryBlock*', 'g_memory_block', false)
	rendered := out.str()
	// The arena root must be per-thread so the parallel per-file generator's
	// workers never share it: a pthread-key slot under bundled TinyCC on macOS
	// (no working thread-local storage), `_Thread_local` everywhere else. It must
	// never be emitted as a plain shared global.
	assert rendered.contains('#define g_memory_block (*(VMemoryBlock* *)v_prealloc_tls_slot())')
	assert rendered.contains('pthread_getspecific(v_prealloc_tls_key)')
	assert rendered.contains('static _Thread_local VMemoryBlock* g_memory_block;')
	assert !rendered.contains('static VMemoryBlock* g_memory_block;')
}

fn test_fastc_split_rewrites_prealloc_tls_global_linkage() {
	definition := 'static _Thread_local VMemoryBlock* g_memory_block;\n'
	assert fastc_extern_declarations(definition, false) == '_Thread_local VMemoryBlock* g_memory_block;\n'
	assert fastc_extern_declarations(definition, true) == 'extern _Thread_local VMemoryBlock* g_memory_block;\n'
}

fn test_fastc_window_ranges_keeps_content_appended_after_empty_file() {
	// An empty final source can still own a non-empty window when generated generic
	// or anonymous-function definitions are appended after that source's body.
	assert fastc_window_ranges([0, 20], 10, 20) == [10, 20]
	assert fastc_window_ranges([0, 10], 10, 10) == []
}

fn test_fastc_emits_explicit_c_extern_prototype() {
	functions := {
		'C.external_api': FastcFunctionSignature{
			parameter_types: ['int', 'voidptr']
			return_type: 'int'
			is_c_extern: true
		}
	}
	assert fastc_c_extern_prototypes(functions) == '#ifndef external_api\nextern int external_api(int, voidptr);\n#endif\n'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct ExternalUsage {
	value u64
}

@[c_extern]
fn C.external_api(pid int, usage &ExternalUsage) int

fn main() {
	mut usage := ExternalUsage{}
	_ = C.external_api(1, &usage)
}
', 'selfhost_c_extern.v', prefs) or { panic(err) }
	assert c_source.contains('#ifndef external_api\nextern int external_api(int, ExternalUsage*);\n#endif'), c_source
}

fn test_fastc_tcc_job_count_respects_parallel_controls() {
	old_vjobs := os.getenv_opt('VJOBS')
	old_disabled := os.getenv_opt('V3_FASTC_NO_PARALLEL')
	defer {
		if value := old_vjobs {
			os.setenv('VJOBS', value, true)
		} else {
			os.unsetenv('VJOBS')
		}
		if value := old_disabled {
			os.setenv('V3_FASTC_NO_PARALLEL', value, true)
		} else {
			os.unsetenv('V3_FASTC_NO_PARALLEL')
		}
	}
	os.setenv('VJOBS', '4', true)
	os.unsetenv('V3_FASTC_NO_PARALLEL')
	mut prefs := pref.new_preferences()
	assert fastc_tcc_job_count(prefs) == 4
	prefs.no_parallel = true
	assert fastc_tcc_job_count(prefs) == 1
	prefs.no_parallel = false
	os.setenv('V3_FASTC_NO_PARALLEL', '1', true)
	assert fastc_tcc_job_count(prefs) == 1
}

fn test_fastc_fragmented_generation_matches_serial_output() {
	large_comment := '// ' + 'x'.repeat(fastc_generation_fragment_size + 1024)
	sources := [
		FastcSourceFile{
			path: 'large.v'
			source: 'module fastc\nfn fastc_fragment_first() {\n${large_comment}\n}\nfn fastc_fragment_second() {}\n'
			header: FastcSourceHeader{
				module_name: 'v3.gen.fastc'
			}
		},
		FastcSourceFile{
			path: 'small_1.v'
			source: 'module fastc\nfn fastc_fragment_small_1() {}\n'
			header: FastcSourceHeader{ module_name: 'v3.gen.fastc' }
		},
		FastcSourceFile{
			path: 'small_2.v'
			source: 'module fastc\nfn fastc_fragment_small_2() {}\n'
			header: FastcSourceHeader{ module_name: 'v3.gen.fastc' }
		},
		FastcSourceFile{
			path: 'small_3.v'
			source: 'module fastc\nfn fastc_fragment_small_3() {}\n'
			header: FastcSourceHeader{ module_name: 'v3.gen.fastc' }
		},
	]
	mut parallel_prefs := pref.new_preferences()
	parallel_prefs.building_v = true
	parallel, _, _ := generate_source_files(sources, map[string]string{}, parallel_prefs) or {
		panic(err)
	}
	mut serial_prefs := pref.new_preferences()
	serial_prefs.building_v = true
	serial_prefs.no_parallel = true
	serial, _, _ := generate_source_files(sources, map[string]string{}, serial_prefs) or {
		panic(err)
	}
	assert parallel == serial
}

fn test_fastc_generation_fragments_keep_top_level_comptime_chain_together() {
	large_comment := '// ' + 'x'.repeat(fastc_generation_fragment_size + 1024)
	source := 'module fastc\n\$if linux {\n\tfn fastc_fragment_comptime_branch() {\n\t\t${large_comment}\n\t}\n} \$else \$if windows {\n\tfn fastc_fragment_comptime_else_if() {}\n} \$else {\n\tfn fastc_fragment_comptime_else() {}\n}\nfn fastc_fragment_after_chain() {}\n'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	fragments := fastc_source_generation_fragments(FastcSourceFile{
		path: 'large_comptime_chain.v'
		source: source
		header: FastcSourceHeader{
			module_name: 'v3.gen.fastc'
		}
	}, prefs)
	assert fragments.len == 2
	assert !fragments[1].source.trim_space().starts_with('\$else')
	assert fragments[1].source.trim_space().starts_with('fn fastc_fragment_after_chain')
}

fn test_fastc_generation_fragments_keep_top_level_initializer_together() {
	large_comment := '// ' + 'x'.repeat(fastc_generation_fragment_size + 1024)
	source := 'module fastc\nstruct FastcFragmentValue {}\n@[inline]\nfn (v FastcFragmentValue) value() int {\n\treturn 1\n}\n${large_comment}\nconst fastc_fragment_result = FastcFragmentValue{}.value()\nfn fastc_fragment_after_initializer() {}\n'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	fragments := fastc_source_generation_fragments(FastcSourceFile{
		path: 'large_top_level_initializer.v'
		source: source
		header: FastcSourceHeader{
			module_name: 'v3.gen.fastc'
		}
	}, prefs)
	assert fragments.len == 2
	assert fragments[0].source.contains('FastcFragmentValue{}.value()')
	assert fragments[1].source.trim_space().starts_with('fn fastc_fragment_after_initializer')
}

fn test_fastc_overlap_workers_honor_serial_preferences() {
	mut prefs := pref.new_preferences()
	prefs.no_parallel = true
	sources := [
		FastcSourceFile{ source: 'module main\nfn a() {}\n' },
		FastcSourceFile{ source: 'module main\nfn b() {}\n' },
		FastcSourceFile{ source: 'module main\nfn c() {}\n' },
		FastcSourceFile{ source: 'module main\nfn d() {}\n' },
	]
	mut pending_references := fastc_start_referenced_function_names(sources, prefs, map[string]FastcFunctionSignature{})
	assert pending_references.workers.len == 0
	assert fastc_wait_referenced_function_names(mut pending_references).len > 0

	mut functions := map[string]FastcFunctionSignature{}
	for name in ['a', 'b', 'c', 'd'] {
		functions[name] = FastcFunctionSignature{}
	}
	mut pending_dispatches := fastc_start_interface_dispatches(map[string]FastcDeclaredTypeKind{}, functions, map[string]bool{}, map[string]bool{}, false, prefs)
	assert pending_dispatches.workers.len == 0
	assert fastc_wait_interface_dispatches(mut pending_dispatches) == ''
}

fn test_fastc_source_scan_flags_respect_identifier_boundaries() {
	flags := fastc_source_scan_flags('module main\nconst answer = 42\n__global count int\n')
	assert flags.has_constants
	assert flags.has_global_declarations
	assert !flags.has_interfaces
	assert !flags.has_comptime_if
	assert !flags.has_type_keywords
	assert !flags.has_generic_fn_syntax
	identifier_flags := fastc_source_scan_flags('module main\nfn key_const() {}\nfn use__global_value() {}\nfn interfaces() {}\nfn structure() {}\n')
	assert !identifier_flags.has_constants
	assert !identifier_flags.has_global_declarations
	assert !identifier_flags.has_interfaces
	assert !identifier_flags.has_type_keywords
}

fn test_fastc_source_scan_flags_detect_declaration_keywords() {
	assert fastc_source_scan_flags('module main\ninterface Shape { area() int }\n').has_interfaces
	assert fastc_source_scan_flags('module main\ninterface Shape {}\n').has_type_keywords
	assert fastc_source_scan_flags('module main\nstruct Point { x int }\n').has_type_keywords
	assert fastc_source_scan_flags('module main\nenum Color { red }\n').has_type_keywords
	assert fastc_source_scan_flags('module main\ntype Id = int\n').has_type_keywords
	assert fastc_source_scan_flags('module main\nunion Bits { a int }\n').has_type_keywords
	dollar := '\$'
	assert fastc_source_scan_flags('module main\n' + dollar + 'if linux {\nfn only_linux() {}\n}\n').has_comptime_if
	assert fastc_source_scan_flags('module main\n' + dollar + ' if linux {\nfn only_linux() {}\n}\n').has_comptime_if
	assert !fastc_source_scan_flags('module main\nfn main() { println("' + dollar + '{1}") }\n').has_comptime_if
	assert fastc_source_scan_flags('module main\nfn pick[T](value T) T { return value }\n').has_generic_fn_syntax
	assert fastc_source_scan_flags('module main\nfn (s Stack) push[T](value T) {}\n').has_generic_fn_syntax
	assert fastc_source_scan_flags('module main\nfn Stack.make[T]() Stack {}\n').has_generic_fn_syntax
	assert !fastc_source_scan_flags('module main\nfn plain(values []int) int { return values[0] }\n').has_generic_fn_syntax
}

fn test_fastc_source_scan_flags_skip_comments_between_tokens() {
	// The scanner treats comments as whitespace, so the byte probes must not
	// let one hide a `$if` or a generic declaration.
	dollar := '\$'
	assert fastc_source_scan_flags('module main\n' + dollar + '/* c */if linux {\nfn only_linux() {}\n}\n').has_comptime_if
	assert fastc_source_scan_flags('module main\n' + dollar + '// c\nif linux {\nfn only_linux() {}\n}\n').has_comptime_if
	assert fastc_source_scan_flags('module main\n' + dollar + '/* outer /* nested */ */ if linux {\n}\n').has_comptime_if
	assert fastc_source_scan_flags('module main\nfn /* c */ pick[T](x T) T { return x }\n').has_generic_fn_syntax
	assert fastc_source_scan_flags('module main\nfn pick /* c */ [T](x T) T { return x }\n').has_generic_fn_syntax
	assert fastc_source_scan_flags('module main\nfn (s /* ) */ Stack) push[T](v T) {}\n').has_generic_fn_syntax
	assert fastc_source_scan_flags('module main\nfn (s Stack) // c\n push[T](v T) {}\n').has_generic_fn_syntax
	assert !fastc_source_scan_flags('module main\nfn plain() { x := 1 // [T]\n }\n').has_generic_fn_syntax
}

fn test_fastc_generic_source_collection_matches_serial_scan() {
	mut prefs := pref.new_preferences()
	sources := [
		FastcSourceFile{
			path: 'first.v'
			source: 'module sample\nfn pick[T](value T) T { return value }\n'
			header: FastcSourceHeader{
				module_name: 'sample'
			}
		},
		FastcSourceFile{
			path: 'second.v'
			source: 'module sample\nfn keep[T](value T) T { return value }\n'
			header: FastcSourceHeader{
				module_name: 'sample'
			}
		},
		FastcSourceFile{
			path: 'plain.v'
			source: 'module sample\nfn plain() {}\n'
			header: FastcSourceHeader{
				module_name: 'sample'
			}
		},
		FastcSourceFile{
			path: 'last.v'
			source: 'module sample\nfn pick[T](other T) T { return other }\n'
			header: FastcSourceHeader{
				module_name: 'sample'
			}
		},
	]
	serial := fastc_collect_generic_method_source_chunk(sources, prefs, 0, sources.len).sources
	mut flagged_sources := sources.clone()
	collected := fastc_collect_generic_method_sources(mut flagged_sources, prefs)
	assert collected.keys().len == serial.keys().len
	for key, expected in serial {
		assert key in collected
		assert collected[key].source == expected.source
	}
}

fn test_selfhost_spawn_nested_wait_statement_and_helper_names() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	foo_start := fastc_spawn_start_name('foo')
	foo_creator := '${foo_start}_c1'
	foo_c1_creator := fastc_spawn_start_name('foo_c1')
	void_waiter := fastc_thread_wait_name(fastc_thread_type_name(''))
	int_waiter := fastc_thread_wait_name(fastc_thread_type_name('int'))
	assert foo_creator != foo_c1_creator
	source := 'module main\n\nfn foo() int {\n\treturn 1\n}\n\nfn foo_c1() int {\n\treturn 2\n}\n\nfn ${foo_start}() int {\n\treturn 0\n}\n\nfn finish() {}\n\nfn main() {\n\tfirst := spawn foo()\n\tmut threads := [first]\n\tthreads << spawn foo_c1()\n\tprintln(threads[0].wait())\n\tprintln(threads[1].wait())\n\tprintln((spawn foo()).wait())\n\tdone := spawn finish()\n\tdone.wait()\n}\n'
	c_source := generate(source, 'selfhost_spawn_regressions.v', prefs) or { panic(err) }
	assert c_source.contains('args->result = foo();'), c_source
	assert c_source.contains('args->result = foo_c1();'), c_source
	assert c_source.contains('${foo_creator}()'), c_source
	assert c_source.contains('${foo_c1_creator}()'), c_source
	assert !c_source.contains('spawn foo_c1'), c_source
	assert c_source.contains('${int_waiter}((${foo_creator}()))'), c_source
	assert c_source.contains('${void_waiter}(done);'), c_source
}

fn test_selfhost_spawn_rejects_disabled_callee_before_arguments() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	_ := generate('module main

@[if fastc_missing_define ?]
fn traced(value int) {}

fn side_effect() int {
	println(99)
	return 1
}

fn main() {
	t := spawn traced(side_effect())
	t.wait()
}
', 'disabled_spawn.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('spawn of disabled function `traced`'), message
}

fn test_selfhost_spawn_accepts_explicit_and_omitted_params_structs() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

@[params]
struct Config {
	value int = default_value()
}

fn default_value() int {
	return 1234
}

fn configured(base int, config Config) int {
	return base + config.value
}

@[params]
struct PointerConfig {
	value int
}

fn configured_pointer(config &PointerConfig) int {
	return config.value
}

fn main() {
	explicit := spawn configured(1, Config{value: 2})
	omitted := spawn configured(4)
	named := spawn configured(5, value: 6)
	pointer := spawn configured_pointer()
	println(explicit.wait())
	println(omitted.wait())
	println(named.wait())
	println(pointer.wait())
}
', 'spawn_params_struct.v', prefs) or { panic(err) }
	assert c_source.contains('args->result = configured(args->arg0, args->arg1);'), c_source
	assert c_source.contains('__v_fastc_struct_default.value=(default_value());'), c_source
	assert c_source.contains('.value=(__v_fastc_struct_field_0)'), c_source
	assert c_source.contains('v_fastc_interface_box(&(PointerConfig){0}, sizeof(PointerConfig))'), c_source
}

fn test_selfhost_spawn_accepts_shared_named_params_field() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

@[params]
struct Config {
	shared int
}

fn configured(config Config) int {
	return config.shared
}

fn main() {
	handle := spawn configured(shared: 1)
	println(handle.wait())
}
', 'spawn_shared_named_params_field.v', prefs) or { panic(err) }
	assert c_source.contains('.shared=(__v_fastc_struct_field_0)'), c_source
}

fn test_generate_and_compile_without_flat_ast() {
	source := 'module main

fn main() {
	mut total := 0
	label := "total="
	for i in 0 .. 3 {
		total += twice(i)
	}
	if true {
		print(label)
		println(total)
	} else {
		println(0)
	}
}

fn twice(value int) int {
	return value * 2
}
'
	prefs := pref.new_preferences()
	c_source := generate(source, 'fastc_test.v', prefs) or { panic(err) }
	assert c_source.contains('i64 total = (0);')
	assert c_source.contains('string label = ("total=");')
	assert c_source.contains('__v_fastc_range_start_0 = (0);')
	assert c_source.contains('__v_fastc_range_end_1 = (3);')
	assert c_source.contains('i64 twice(i64 value);')
	assert c_source.contains('setvbuf(stdout, NULL, _IONBF, 0);')
	assert !c_source.contains('v3.flat')

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'total=6'
}

fn test_declaration_only_entry_synthesizes_main() {
	prefs := pref.new_preferences()
	c_source := generate('module main

pub fn answer() int {
	return 42
}
', 'declaration_only_entry.v', prefs) or { panic(err) }
	assert c_source.contains('int main(void) {'), c_source
	assert c_source.contains('\tsetvbuf(stdout, NULL, _IONBF, 0);\n\treturn 0;'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_declaration_only_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
}

fn test_ordinary_string_interpolation_has_runtime_support() {
	prefs := pref.new_preferences()
	c_source := generate(r"module main

fn greeting(name string) string {
	return '|${name:10}|${name:-10}|${name:10s}|'
}

fn main() {
	println(greeting('FastC'))
	println('|${'界':4}|${'é':4}|${'👩🏽‍💻':4}|')
}
", 'ordinary_string_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('static string builtin__string_plus_many'), c_source
	assert c_source.contains('v_fastc_string_pad(name, 10, false)'), c_source
	assert c_source.contains('v_fastc_string_pad(name, 10, true)'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_interpolation_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '|     FastC|FastC     |     FastC|\n|  界|   é|  👩🏽‍💻|\n'
}

fn test_ordinary_string_concatenation_and_alias_matches() {
	prefs := pref.new_preferences()
	c_source := generate("module main

type Str = string

fn main() {
	value := Str('' + '1')
	println('a' + 'b')
	mut combined := 'a'
	combined += 'b'
	println(combined)
	mut alias_combined := Str('x')
	alias_combined += Str('y')
	println(alias_combined)
	result := match value {
		Str('1') { 'expression' }
		else { 'bad' }
	}
	println(result)
	match value {
		Str('1') { println('matched') }
		else {}
	}
}
", 'ordinary_string_concatenation.v', prefs) or { panic(err) }
	assert c_source.count('builtin__string_plus_many(2, (string[]){') >= 4, c_source
	assert c_source.contains('combined=builtin__string_plus_many(2, (string[]){combined,"b"});'), c_source
	assert c_source.contains('alias_combined=builtin__string_plus_many(2, (string[]){alias_combined,((Str)("y"))});'), c_source
	assert c_source.count('builtin__string_eq(__v_fastc_match_') >= 2, c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_string_concat_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'ab\nab\nxy\nexpression\nmatched\n'

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	selfhost_c := generate("module main

type Str = string

fn main() {
	mut value := Str('a')
	value += Str('b')
}
", 'selfhost_string_alias_compound_assignment.v', selfhost_prefs) or { panic(err) }
	assert selfhost_c.contains('value=builtin__string_plus(value,((Str)(_S("b"))));'), selfhost_c
}

fn test_c_directives_preserve_order_and_resolve_source_paths() {
	prefs := pref.new_preferences()
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_directives_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'src')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_directives' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'src', 'config.h'), '#ifndef FEATURE\n#error "FEATURE must precede config.h"\n#endif\n') or { panic(err) }
	os.write_file(os.join_path(root, 'src', 'local.c'), '') or { panic(err) }
	os.write_file(os.join_path(root, 'vmod_value.h'), '') or { panic(err) }
	os.write_file(os.join_path(root, 'vroot_value.h'), '') or { panic(err) }
	source_path := os.join_path(root, 'src', 'main.v')
	source := 'module main

#define FEATURE 1
#include "@DIR/config.h"
#include "local.c"
#include "@VMODROOT/vmod_value.h"
#insert "@VROOT/vroot_value.h"

fn main() {
	println(42)
}
'
	os.write_file(source_path, source) or { panic(err) }
	c_source := generate(source, source_path, prefs) or { panic(err) }
	define_index := c_source.index('#define FEATURE 1') or { -1 }
	include_index := c_source.index('#include "${os.real_path(os.join_path(root, 'src'))}/config.h"') or {
		-1
	}
	assert define_index >= 0
	assert include_index > define_index
	assert c_source.contains('#include "${os.real_path(os.join_path(root, 'src'))}/local.c"'), c_source

	assert c_source.contains('#include "${os.real_path(root)}/vmod_value.h"'), c_source
	assert c_source.contains('#include "${os.real_path(root)}/vroot_value.h"'), c_source
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '42\n'
}

fn test_c_directive_conditional_scopes_include_generated_code() {
	prefs := pref.new_preferences()
	c_source := generate('module main

struct Holder {
	value int
}

#if 1
fn optional(value Holder) int {
	return value.value
}
#endif

fn main() {
	println(42)
}
', 'conditional_scope.c.v', prefs) or { panic(err) }
	type_index := c_source.index('struct Holder {') or { -1 }
	if_index := c_source.index('#if 1') or { -1 }
	definition_index := c_source.index('i64 optional(Holder value) {') or { -1 }
	endif_index := c_source.index_after('#endif', definition_index) or { -1 }
	assert type_index >= 0
	assert if_index > type_index
	assert definition_index > if_index
	assert endif_index > definition_index

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_conditional_scope_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '42\n'
}

fn test_c_directive_vmodroot_falls_back_to_source_directory() {
	prefs := pref.new_preferences()
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_manifestless_directive_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'header.h'), '') or { panic(err) }
	source_path := os.join_path(root, 'main.v')
	source := 'module main

#include "@VMODROOT/header.h"

fn main() {}
'
	os.write_file(source_path, source) or { panic(err) }
	assert fastc_vmod_root_for_file(source_path) == os.real_path(root)
	c_source := generate(source, source_path, prefs) or { panic(err) }
	assert c_source.contains('#include "${os.real_path(root)}/header.h"'), c_source
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
}

fn test_selfhost_module_pseudo_values_use_the_source_manifest() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_module_pseudo_${os.getpid()}')
	compiler_root := os.join_path(root, 'compiler')
	module_root := os.join_path(root, 'modules', 'dep')
	os.rmdir_all(root) or {}
	os.mkdir_all(compiler_root) or { panic(err) }
	os.mkdir_all(module_root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(compiler_root, 'v.mod'), "Module { name: 'compiler' }\n") or {
		panic(err)
	}
	manifest := "Module { name: 'dep' }\r\n"
	os.write_file(os.join_path(module_root, 'v.mod'), manifest) or { panic(err) }
	source_path := os.join_path(module_root, 'dep.v')
	source := 'module dep

pub fn fastc_module_root() string {
	return @VMODROOT
}

pub fn fastc_module_manifest() string {
	return @VMOD_FILE
}

pub fn fastc_compiler_root() string {
	return @VROOT
}
'
	os.write_file(source_path, source) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.vroot = compiler_root
	c_source := generate(source, source_path, prefs) or { panic(err) }
	module_root_literal := fastc_c_string_value(os.real_path(module_root))
	manifest_literal := fastc_c_string_value(manifest.replace('\r\n', '\n'))
	compiler_root_literal := fastc_c_string_value(compiler_root)
	assert c_source.contains('return _S(${module_root_literal});'), c_source
	assert c_source.contains('return _S(${manifest_literal});'), c_source
	assert c_source.contains('return _S(${compiler_root_literal});'), c_source
}

fn test_selfhost_location_preserves_method_receiver_kind() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct LocationOwner {}

fn (receiver LocationOwner) fastc_instance_location() string {
	return @LOCATION
}

fn LocationOwner.fastc_static_location() string {
	return @LOCATION
}
', 'location_receiver_kind.v', prefs) or { panic(err) }
	assert c_source.contains(', main.LocationOwner{}.fastc_instance_location")'), c_source
	assert c_source.contains(', main.LocationOwner.fastc_static_location (static)")'), c_source
}

fn test_ordinary_primitive_interpolation_has_runtime_support() {
	prefs := pref.new_preferences()
	c_source := generate(r"module main

fn main() {
	value := 7
	negative := -2
	large := u64(42)
	enabled := true
	println('value=${value}; negative=${negative}; large=${large}; enabled=${enabled}')
	hex_value := 15
	println('${hex_value:x}|${hex_value:04x}|${hex_value:X}|${hex_value:04d}|${hex_value:b}|${hex_value:o}')
	println('${8364:c}')
	println('${0x754c:4c}')
	print('${-1:c}x')
	print('${0x110000:c}x')
}
", 'ordinary_primitive_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('v_fastc_signed_str((long long)(value))'), c_source
	assert c_source.contains('v_fastc_signed_str((long long)(negative))'), c_source
	assert c_source.contains('v_fastc_unsigned_str((unsigned long long)(large))'), c_source
	assert c_source.contains('v_fastc_bool_str(enabled)'), c_source
	assert c_source.contains('v_fastc_signed_format((long long)(hex_value), "x")'), c_source
	assert c_source.contains('v_fastc_signed_format((long long)(hex_value), "04x")'), c_source
	assert c_source.contains('v_fastc_signed_format((long long)(8364), "c")'), c_source
	assert c_source.contains('v_fastc_signed_format((long long)(-1), "c")'), c_source
	assert c_source.contains('bool valid_codepoint = magnitude <= 1114111;'), c_source
	assert !c_source.contains('magnitude <= 1114111 ? (unsigned)magnitude : 65533'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_primitive_interpolation_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'value=7; negative=-2; large=42; enabled=true\nf|000f|F|0015|1111|17\n€\n  界\nxx'
}

fn test_primitive_alias_interpolation_uses_the_underlying_type() {
	prefs := pref.new_preferences()
	c_source := generate(r"module main

type Count = int
type Text = string
type Custom = int

fn (value Custom) str() string {
	return 'custom'
}

fn main() {
	println('${Count(1)}|${Text('ok')}|${Custom(2)}')
	println('${Count(15):04x}|${Text('x'):3}')
}
", 'primitive_alias_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('v_fastc_signed_str((long long)(((Count)(1))))'), c_source
	assert c_source.contains('v_fastc_signed_format((long long)(((Count)(15))), "04x")'), c_source
	assert c_source.contains('v_fastc_string_pad(((Text)("x")), 3, false)'), c_source
	assert c_source.contains('Custom_str(((Custom)(2)))'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_alias_interpolation_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '1|ok|custom\n000f|  x\n'
}

fn test_direct_char_interpolation_is_lowered_without_type_validation() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate(r"module main

__global ch char

fn main() {
	println('${ch}')
}
", 'direct_char_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('v_fastc_signed_format((long long)(ch), "c")'), c_source
}

fn test_selfhost_fixed_array_struct_field_interpolation() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Address {
	bytes [4]u8
}

fn (value u8) str() string {
	return "byte"
}

fn format(address Address) string {
	return "\${address.bytes}"
}

fn main() {
	_ := format(Address{})
}
', 'selfhost_fixed_array_field_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('string v_fastc_fixed_array_str_FixedArray_4_FASTC_ARRAY_OF_u8(u8 *it)'), c_source

	assert c_source.contains('v_fastc_fixed_array_str_FixedArray_4_FASTC_ARRAY_OF_u8((u8 *)'), c_source

	assert c_source.contains('u8_str(it[0])'), c_source
}

fn test_selfhost_dynamic_array_struct_field_interpolation() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Versions {
	values []u32
}

fn (value u32) str() string {
	return "version"
}

fn format(versions Versions) string {
	return "\${versions.values}"
}

fn main() {
	_ := format(Versions{})
}
', 'selfhost_dynamic_array_field_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('string v_fastc_array_str_Array_u32(Array_u32 it)'), c_source
	assert c_source.contains('v_fastc_array_str_Array_u32(versions.values)'), c_source
	assert c_source.contains('u32_str((*('), c_source
}

fn test_ordinary_nul_codepoint_interpolation_is_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate(r"module main

fn main() {
	print('${0:c}x')
}
", 'ordinary_nul_codepoint_interpolation.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('NUL code points in `:c` interpolation'), message
}

fn test_ordinary_nonliteral_codepoint_interpolation_is_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate(r"module main

fn main() {
	code := 0
	print('${code:c}x')
}
", 'ordinary_nonliteral_codepoint_interpolation.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('nonliteral `:c` interpolation'), message
}

fn test_i64_codepoint_interpolation_requires_runtime_support() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate(r"module main

fn main() {
	value := i64(65)
	println('${value:c}')
}
", 'i64_codepoint_interpolation.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('nonliteral `:c` interpolation'), message

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	mut selfhost_message := ''
	_ := generate(r"module main

fn main() {
	value := i64(65)
	println('${value:c}')
}
", 'i64_codepoint_interpolation_selfhost.v', selfhost_prefs) or {
		selfhost_message = err.msg()
		''
	}
	assert selfhost_message.contains('interpolation of type `i64`'), selfhost_message
}

fn test_zero_value_strings_print_as_empty_strings() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global name string

fn main() {
	print(name)
	println(name)
	println("done")
}
', 'zero_value_string.v', prefs) or { panic(err) }
	assert c_source.contains('fputs(value ? value : "", stdout)'), c_source
	assert c_source.contains('puts(value ? value : "")'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_zero_string_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '\ndone\n'
}

fn test_ordinary_print_rejects_types_without_runtime_support() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { println(1.5) }\n',
		'module main\nfn main() { value := f32(1.5); print(value) }\n',
	] {
		mut message := ''
		_ := generate(source, 'unsupported_print_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('printing value of type'), message
	}
}

fn test_top_level_statements_emit_main_directly() {
	prefs := pref.new_preferences()
	c_source := generate("println('Hello, World!')\n", 'hello_world.v', prefs) or { panic(err) }
	assert c_source.contains('int main(void) {')
	assert c_source.contains('println("Hello, World!");')
	assert c_source.contains('setvbuf(stdout, NULL, _IONBF, 0);')
}

fn test_unsupported_import_is_rejected() {
	prefs := pref.new_preferences()
	mut failed := false
	_ := generate('module main\nimport os\nfn main() {}\n', 'imports.v', prefs) or {
		failed = true
		''
	}
	assert failed
}

fn test_c_build_directives_are_rejected_instead_of_discarded() {
	mut prefs := pref.new_preferences()
	for building_v in [false, true] {
		prefs.building_v = building_v
		for directive in ['#flag -D FEATURE=1', '#flag -std=gnu11'] {
			mut message := ''
			_ := generate('module main\n${directive}\nfn main() {}\n', 'c_build_directive.v', prefs) or {
				message = err.msg()
				''
			}
			assert message.contains('C build directive `${directive}`'), message
		}
	}

	// `#pkgconfig <lib>` names an external library`s link/cflags; FastC drives its own
	// tcc link line, so it is skipped, not rejected.
	prefs.building_v = false
	pkgconfig_out := generate('module main\n#pkgconfig sqlite3\nfn main() { println(1) }\n', 'pkgconfig_flag.v', prefs) or {
		assert false, 'pkgconfig directive should be skipped: ${err.msg()}'
		''
	}
	assert !pkgconfig_out.contains('pkgconfig'), pkgconfig_out

	// A platform-qualified `#flag <os> ...` that names a different target is inert
	// for this build and must be skipped, not rejected, mirroring how `#include
	// <os>` is target-filtered. (vlib/os uses `#flag windows -lws2_32`.)
	$if !windows {
		prefs.building_v = false
		skipped := generate('module main\n#flag windows -lws2_32\nfn main() { println(1) }\n', 'os_qualified_flag.v', prefs) or {
			assert false, 'os-qualified flag for another target should be skipped: ${err.msg()}'
			''
		}
		assert !skipped.contains('ws2_32'), skipped
	}

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_imported_c_flag_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dependency')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'module main\nimport dependency\nfn main() { dependency.run() }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'dependency', 'dependency.v'), 'module dependency\n#flag -D FEATURE=1\npub fn run() {}\n') or { panic(err) }
	prefs.building_v = false
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('C build directive `#flag -D FEATURE=1`'), message
}

fn test_colliding_import_aliases_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nimport alpha as dep\nimport beta as dep\n',
		'module main\nimport (\nalpha as dep\nbeta as dep\n)\n',
	] {
		mut message := ''
		_ := fastc_scan_source_header(source, 'colliding_import_alias.v', prefs) or {
			message = err.msg()
			FastcSourceHeader{}
		}
		assert message.contains('cannot reuse import alias `dep`'), message
		assert message.contains('`alpha`') && message.contains('`beta`'), message
	}
}

fn test_generate_files_resolves_modules_without_an_ast() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_modules_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'mathutil')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'mathutil', 'mathutil.v')
	os.write_file(main_file, 'module main\nimport mathutil\nfn main() { println(mathutil.twice(21)) }\n') or {
		panic(err)
	}
	os.write_file(module_file, 'module mathutil\npub fn twice(value int) int { return value * 2 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('i64 mathutil__twice(i64 value);')
	assert c_source.contains('println(mathutil__twice(21));'), c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '42'
}

fn test_source_resolver_preserves_aliases_for_scheduled_files() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_scheduled_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	canonical_dir := os.join_path(root, 'canonical')
	os.mkdir_all(canonical_dir) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'legacy')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'module main\nimport canonical\nimport legacy\nfn main() {}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(canonical_dir, 'canonical.v'), 'module canonical\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'legacy', 'alias.v'), "@[alias: '${canonical_dir}'] module legacy\n") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	sources, aliases := fastc_resolve_source_files([main_file], prefs) or { panic(err) }
	assert sources.len == 2
	assert aliases['legacy'] == 'canonical'
}

fn test_source_resolver_preserves_aliases_for_symlinked_module_dirs() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_symlinked_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	canonical_dir := os.join_path(root, 'canonical')
	legacy_dir := os.join_path(root, 'legacy')
	os.mkdir_all(canonical_dir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.symlink(canonical_dir, legacy_dir) or { return }
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'module main\nimport canonical\nimport legacy\nfn main() { canonical.ping(); legacy.ping() }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(canonical_dir, 'canonical.v'), 'module canonical\npub fn ping() {}\n') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	sources, aliases := fastc_resolve_source_files([main_file], prefs) or { panic(err) }
	assert sources.len == 2
	assert aliases['legacy'] == 'canonical'
}

fn test_source_resolver_canonicalizes_building_v_entry_path() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.vroot = os.real_path(@VEXEROOT)
	canonical_entry := os.join_path(prefs.vroot, 'vlib', 'v3', 'v3.v')
	// The regular V3 driver can pass a relative or otherwise non-canonical entry,
	// while entry-module enumeration returns canonical absolute paths. Both spellings
	// must resolve to one source or FastC reports a duplicate `main` during self-host.
	noncanonical_entry := os.dir(canonical_entry) + '/../v3/v3.v'
	sources, _ := fastc_resolve_source_files([noncanonical_entry], prefs) or { panic(err) }
	mut entry_count := 0
	for source in sources {
		if source.path == canonical_entry {
			entry_count++
		}
	}
	assert entry_count == 1
}

fn test_header_discovers_imports_only_from_selected_comptime_branches() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_comptime_imports_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'alpha')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'beta')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'module main

\$if linux {
	import alpha as dep
} \$else {
	import beta as dep
}

fn main() {
	dep.ping()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'alpha', 'alpha.v'), "module alpha

fn init() {
	println('alpha init')
}

fn cleanup() {
	println('alpha cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'beta', 'beta.v'), "module beta

fn init() {
	println('beta init')
}

fn cleanup() {
	println('beta cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	prefs.module_search_paths = [root]
	header := fastc_scan_source_header(os.read_file(main_file) or { panic(err) }, main_file, prefs) or {
		panic(err)
	}
	assert header.import_order == ['alpha']
	assert header.imports['dep'] == 'alpha'
	assert 'beta' !in header.imports.values()
	sources, aliases := fastc_resolve_source_files([main_file], prefs) or { panic(err) }
	mut resolved_modules := []string{}
	for source_file in sources {
		if source_file.header.module_name !in resolved_modules {
			resolved_modules << source_file.header.module_name
		}
	}
	assert resolved_modules == ['main', 'alpha']
	prefs.building_v = true
	c_source, _, _ := generate_source_files(sources, aliases, prefs) or { panic(err) }
	assert c_source.contains('\talpha__init();'), c_source
	assert c_source.contains('\talpha__cleanup();'), c_source
	assert !c_source.contains('beta__init'), c_source
	assert !c_source.contains('beta__cleanup'), c_source
}

fn test_generate_files_rejects_mismatched_imported_module_declarations() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_module_mismatch_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'foo')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'module main\nimport foo\nfn main() { println(foo.answer()) }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'foo', 'foo.v'), 'module bar\npub fn answer() int { return 42 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('declares module `bar` instead of `foo`'), message
}

fn test_module_qualified_alias_cast_can_receive_a_method_call() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	main_source := 'module main
import clock
fn main() {
	d := clock.Duration(1500)
	println(clock.Duration(d - 500).microseconds())
}
'
	clock_source := 'module clock
pub type Duration = i64
pub fn (d Duration) microseconds() i64 { return i64(d) / 1000 }
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'clock.v'
			source: clock_source
			header: fastc_scan_source_header(clock_source, 'clock.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('clock__Duration_microseconds(((clock__Duration)'), c_source
}

fn test_selfhost_module_qualified_pointer_cast() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	main_source := 'module main
import transport
fn convert(pointer voidptr) &transport.Conn {
	return unsafe { &transport.Conn(pointer) }
}
fn main() {}
'
	transport_source := 'module transport
pub struct Conn {}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'transport.v'
			source: transport_source
			header: fastc_scan_source_header(transport_source, 'transport.v', prefs) or {
				panic(err)
			}
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('return ((transport__Conn*)(pointer));'), c_source
	assert !c_source.contains('&((transport__Conn)(pointer))'), c_source
}

fn test_selfhost_double_pointer_cast_assignment() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct State {
mut:
	list &&char
}

fn set_list(mut state State, pointer voidptr) {
	state.list = unsafe { &&char(pointer) }
}

fn main() {}
', 'selfhost_double_pointer_cast_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('state->list=((char**)(pointer))'), c_source
	assert !c_source.contains('state->list=)&&'), c_source
}

fn test_selfhost_static_method_array_literal_argument_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Tool {}

struct RunResult {
	code int
}

fn Tool.run(args []string) RunResult {
	return RunResult{code: args.len}
}

fn main() {
	_ := Tool.run(["one", "two"]).code
}
', 'static_array_argument.v', prefs) or { panic(err) }
	assert c_source.contains('Tool_run(((Array_string)builtin__new_array_from_c_array'), c_source
	assert !c_source.contains('Tool_run(['), c_source
}

fn test_selfhost_static_method_named_shared() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Worker {}

fn Worker.shared() int {
	return 42
}

fn main() {
	_ := Worker.shared()
}
', 'selfhost_static_method_shared.v', prefs) or { panic(err) }
	assert c_source.contains('i64 Worker_shared(void)'), c_source
	assert c_source.contains('Worker_shared()'), c_source
}

fn test_selfhost_primitive_cast_array_literal_argument_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn take(values []u64) {}

fn main() {
	take([u64(0x8000000000000000), u64(1)])
}
', 'selfhost_primitive_cast_array_argument.v', prefs) or { panic(err) }
	assert c_source.contains('0x8000000000000000ULL'), c_source
	assert !c_source.contains('take(['), c_source
}

fn test_selfhost_static_method_named_options_argument_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Kind {
	one
	two
}

struct Options {
	kind Kind
}

struct Tool {}

fn Tool.run(value int, options Options) !int {
	return value + int(options.kind)
}

fn use() !int {
	return Tool.run(1, kind: .two)
}

fn main() {}
', 'selfhost_static_named_options.v', prefs) or { panic(err) }
	assert c_source.contains('Tool_run(1,(Options){.kind='), c_source
	assert c_source.contains('Kind__two'), c_source
	assert !c_source.contains('kind:.two'), c_source
}

fn test_selfhost_method_context_types_array_literal_with_call_first() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Page {}

fn label() string {
	return "title"
}

fn (mut page Page) set_title(parts []string) {}

fn main() {
	mut page := Page{}
	page.set_title([label(), "repo"])
}
', 'method_context_array_argument.v', prefs) or { panic(err) }
	assert c_source.contains('Page_set_title(&(page),((Array_string)builtin__new_array_from_c_array'), c_source
	assert !c_source.contains('Page_set_title(&(page),['), c_source
}

fn test_generate_files_preserves_all_blank_imports() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_blank_imports_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'alpha')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'beta')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, "module main

import alpha as _
import beta as _

fn main() {
	println('main')
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'alpha', 'alpha.v'), "module alpha

fn init() {
	println('alpha init')
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'beta', 'beta.v'), "module beta

fn init() {
	println('beta init')
}
") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	header := fastc_scan_source_header(os.read_file(main_file) or { panic(err) }, main_file, prefs) or {
		panic(err)
	}
	assert header.blank_imports == ['alpha', 'beta']
	assert header.import_order == ['alpha', 'beta']
	assert '_' !in header.imports
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('\talpha__init();'), c_source
	assert c_source.contains('\tbeta__init();'), c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'alpha init\nbeta init\nmain'
}

fn test_generate_files_rejects_private_imported_functions() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_import_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(main_file, 'module main\nimport secrets\nfn main() { println(secrets.secret()) }\n') or { panic(err) }
	os.write_file(module_file, 'module secrets\nfn secret() int { return 42 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private function `secret` from imported module `secrets`'), message

	os.write_file(module_file, 'module secrets\npub fn secret() int { return 42 }\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('println(secrets__secret());'), c_source
}

fn test_generate_files_rejects_private_imported_constants() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_constant_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(main_file, 'module main\nimport secrets\nfn main() { println(secrets.secret) }\n') or {
		panic(err)
	}
	os.write_file(module_file, 'module secrets\nconst secret = 42\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private constant `secret` from imported module `secrets`'), message

	os.write_file(module_file, 'module secrets\npub const secret = 42\n') or { panic(err) }
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('println(secrets__secret);'), c_source
}

fn test_duplicate_constant_declarations_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nconst answer = 1\nconst answer = 2\nfn main() {}\n',
		'module main\nconst (\nanswer = 1\nanswer = 2\n)\nfn main() {}\n',
	] {
		mut message := ''
		_ := generate(source, 'duplicate_constant.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('duplicate constant `answer`'), message
	}
}

fn test_constant_declarations_require_an_assignment_after_the_name() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nconst answer nonsense = 42\nfn main() {}\n', 'invalid_constant_assignment.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('constant `answer` requires `=` or `:=` after its name'), message

	for assignment in ['=', ':='] {
		c_source := generate('module main\nconst answer ${assignment} 42\nfn main() { println(answer) }\n', 'valid_constant_assignment.v', prefs) or { panic(err) }
		assert c_source.contains('main__answer'), c_source
	}
}

fn test_external_c_constant_declarations_are_skipped() {
	prefs := pref.new_preferences()
	// `const C.name type` only records that the C headers provide the symbol
	// (as in vlib/sync/stdatomic). FastC must not register `C` as a constant
	// (which previously reported a duplicate on the second such line) nor reject
	// it for missing an `=`/`:=` after the name. The declaration is simply
	// dropped, while ordinary constants next to it keep working.
	for source in [
		'module main\npub const C.SEEK_SET i32\npub const C.SEEK_CUR i32\nconst answer = 42\nfn main() { println(answer) }\n',
		'module main\nconst (\nC.SEEK_SET i32\nC.SEEK_CUR i32\nanswer = 42\n)\nfn main() { println(answer) }\n',
	] {
		c_source := generate(source, 'external_c_constants.v', prefs) or { panic(err) }
		assert c_source.contains('main__answer'), c_source
		// The external symbols are provided by the C headers, so FastC emits no
		// constant definition for them.
		assert !c_source.contains('SEEK_SET'), c_source
		assert !c_source.contains('SEEK_CUR'), c_source
	}
}

fn test_real_builtin_path_provided_params_struct_named_args() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// Trailing named arguments (`a: 2, b: true`) at the last `@[params]` struct
	// parameter collapse into one struct initializer rather than being counted as
	// separate arguments.
	source := generate("module main\n@[params]\nstruct Opts {\n\ta int\n\tb bool\n}\nfn foo(x int, opts Opts) int {\n\treturn x + opts.a\n}\nfn main() {\n\tr := foo(1, a: 2, b: true)\n\tif r > 0 {\n\t\tprintln('ok')\n\t}\n}\n", 'params_named_args.v', prefs) or { panic(err) }
	assert source.contains('foo(1,({'), source
	assert source.contains('__v_fastc_struct_field_'), source
}

fn test_real_builtin_path_embedded_method_promotion() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// A method defined on an embedded type is promoted through the `__embedded_N`
	// field: a value receiver passes `(d).__embedded_0`, a `mut` receiver passes
	// its address.
	source := generate("module main\nstruct Base {\n\tx int\n}\nfn (b Base) hello() int {\n\treturn b.x + 7\n}\nfn (mut b Base) bump() {\n\tb.x = b.x + 1\n}\nstruct Derived {\n\tBase\n\ty int\n}\nfn main() {\n\tmut d := Derived{\n\t\tBase: Base{\n\t\t\tx: 3\n\t\t}\n\t}\n\td.bump()\n\tif d.hello() > 5 {\n\t\tprintln('ok')\n\t}\n}\n", 'embedded_method.v', prefs) or { panic(err) }
	assert source.contains('Base_hello((d).__embedded_0)'), source
	assert source.contains('Base_bump(&((d).__embedded_0))'), source
	// The `Base:` initializer targets the embedded field, not a `.Base` field.
	assert source.contains('.__embedded_0='), source
}

fn test_selfhost_method_params_struct_named_args() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main\n@[params]\nstruct Options {\n\tafter bool\n}\nstruct Service {}\nfn (mut service Service) use(options Options) {}\nfn (mut service Service) redirect(path string, options Options) {}\nfn main() {\n\tmut service := Service{}\n\tservice.use(after: true)\n\tservice.redirect("/next")\n}\n', 'method_params_struct.v', prefs) or { panic(err) }
	assert source.contains('Service_use(&(service),(Options){'), source
	assert source.contains('.after='), source
	assert source.contains('Service_redirect(&(service),_S("/next"),(Options){0})'), source
}

fn test_selfhost_method_value_uses_method_function_symbol() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main

@[params]
struct Options {
	handler voidptr
}

struct Service {}

fn (mut service Service) handle() {}
fn (mut service Service) use(options Options) {}

fn main() {
	mut service := Service{}
	service.use(handler: service.handle)
}
', 'method_value.v', prefs) or { panic(err) }
	assert source.contains('Service_handle'), source
	assert !source.contains('service.handle'), source
}

fn test_selfhost_selector_at_end_of_binary_expression_is_not_method_value() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main

enum Kind {
	other
	str
}

struct Node {
	kind      Kind
	is_method bool
}

fn accepts_str_method(node Node) bool {
	return node.is_method && node.kind == .str
}

fn same_storage(left string, right string) bool {
	return left.len == right.len && unsafe { left.str == right.str }
}

fn main() {}
', 'selector_after_binary.v', prefs) or { panic(err) }
	assert !source.contains('&builtin__bool_str'), source
	assert source.contains('Kind__str'), source
	assert source.contains('left.str'), source
	assert source.contains('right.str'), source
}

fn test_selfhost_embedded_option_method_recovers_payload_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main

struct Base {}

fn (base Base) get() !string {
	return "ok"
}

struct User {}

struct Derived {
	Base
mut:
	user User
	flag bool
}

fn maybe_user() !User {
	return error("no")
}

fn main() {
	mut value := Derived{}
	value.user = maybe_user() or {
		value.flag = false
		User{}
	}
	text := value.get() or { "" }
	_ := text
}
', 'embedded_option_method.v', prefs) or { panic(err) }
	assert source.contains('*((string *)__v_fastc_option_'), source
	assert !source.contains('*((int *)__v_fastc_option_'), source
}

fn test_real_builtin_path_inferred_enum_keyed_map() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// An inferred map with enum keys — an explicit `Enum.field` first entry then
	// `.field` shorthand, newline-separated — recovers the key enum from the
	// declared enum (a value must not swallow the next line's `.field` key).
	source := generate("module main\nenum Block {\n\thr\n\th\n}\nconst tags = {\n\tBlock.hr: 'a'\n\t.h: 'b'\n}\nfn main() {\n\tif tags.len > 0 {\n\t\tprintln('ok')\n\t}\n}\n", 'enum_keyed_map.v', prefs) or { panic(err) }
	assert source.contains('Block__hr'), source
	assert source.contains('Block__h'), source
	assert source.contains('Map_Block_string'), source
}

fn test_selfhost_inferred_map_enum_value_shorthand_uses_first_value_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate("module main\nenum Header {\n\taccept\n\tcontent_type\n}\nconst headers = {\n\t'accept': Header.accept\n\t'content-type': .content_type\n}\nfn main() {\n\t_ := headers\n}\n", 'enum_value_map.v', prefs) or { panic(err) }
	assert source.contains('Header__accept'), source
	assert source.contains('Header__content_type'), source
	assert source.contains('Map_string_Header'), source
}

fn test_selfhost_map_literal_call_argument_uses_runtime_map() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate("module main\nfn use_headers(headers map[string]string) {}\nfn main() {\n\tuse_headers({\n\t\t'Server': 'veb'\n\t})\n}\n", 'map_literal_argument.v', prefs) or { panic(err) }
	assert source.contains('use_headers(({ map __v_fastc_argument_map = builtin__new_map'), source
	assert source.contains('builtin__map_set(&__v_fastc_argument_map'), source
}

fn test_selfhost_empty_inferred_map_uses_expected_return_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn empty_nested_map() map[string]map[string]string {
	return {}
}

fn main() {
	_ := empty_nested_map()
}
', 'selfhost_empty_inferred_map.v', prefs) or { panic(err) }
	assert c_source.contains('Map_string_Map_string_string'), c_source
	assert c_source.contains('builtin__new_map(sizeof(string), sizeof(map)'), c_source
}

fn test_real_builtin_path_statement_spawn_detaches_the_thread() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// A fire-and-forget `spawn f()` statement detaches the thread rather than
	// being rejected for discarding its handle.
	source := generate("module main\nfn worker(x int) int {\n\treturn x + 1\n}\nfn main() {\n\tspawn worker(5)\n\tprintln('ok')\n}\n", 'detached_spawn.v', prefs) or { panic(err) }
	assert source.contains('pthread_detach'), source
}

fn test_real_builtin_path_typeof_generic_reflection() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// `typeof[T]().idx`/`.name` are compile-time type reflection (used pervasively
	// by vlib/orm). FastC lowers them to the type's canonical builtin index and its
	// name string.
	source := generate('module main\nconst i64_idx = typeof[i64]().idx\nconst str_idx = typeof[string]().idx\nconst int_name = typeof[int]().name\nfn main() {\n\tif i64_idx == 9 && str_idx == 21 {\n\t\tprintln(int_name)\n\t}\n}\n', 'typeof_reflection.v', prefs) or { panic(err) }
	assert source.contains('_S("int")'), source
	// i64 -> 9, string -> 21 (the canonical indices in vlib/v/ast/types.v).
	assert source.contains('main__i64_idx = 9'), source
	assert source.contains('main__str_idx = 21'), source
}

fn test_fastc_vmod_subdirs_parses_the_declared_subdirectories() {
	root := os.join_path(os.vtmp_dir(), 'fastc_vmod_subdirs_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	// The entry module of a real app spans its declared `subdirs` (as gitly's
	// `ssh/`, `repo/`, ... do); FastC reads that list to collect their sources.
	os.write_file(os.join_path(root, 'v.mod'), 'Module {\n\tname: \'x\'\n\tsubdirs: [\'admin\', \'ssh\', "user"]\n}\n') or { panic(err) }
	assert fastc_vmod_subdirs(root) == ['admin', 'ssh', 'user']
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'x'\n}\n") or { panic(err) }
	assert fastc_vmod_subdirs(root) == []string{}
}

fn test_real_builtin_path_map_and_filter_lower_to_array_loops() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// `.map`/`.filter` are compiler-magic array methods: FastC lowers the `it`
	// closure into a C statement expression that iterates the source array and
	// builds a new one with `builtin____new_array` + `builtin__array_push`.
	map_source := generate('module main\nfn main() {\n\tnums := [1, 2, 3]\n\tprintln(nums.map(it * 2).len)\n}\n', 'map_lowering.v', prefs) or { panic(err) }
	assert map_source.contains('builtin____new_array'), map_source
	assert map_source.contains('builtin__array_push'), map_source
	assert map_source.contains('int it = ('), map_source

	filter_source := generate('module main\nfn main() {\n\tnums := [1, 2, 3]\n\tprintln(nums.filter(it > 1).len)\n}\n', 'filter_lowering.v', prefs) or { panic(err) }
	assert filter_source.contains('builtin__array_push'), filter_source
	assert filter_source.contains('int it = ('), filter_source

	enum_source := generate('module main

enum Header {
	cache_control
	content_type
}

fn main() {
	_ := [Header.cache_control, .content_type].map(it.str())
}
', 'enum_literal_map_lowering.v', prefs) or { panic(err) }
	assert enum_source.contains('typedef array Array_Header;'), enum_source
	plain_enum_array_source := generate('module main

enum Space {
	initial
	handshake
}

fn main() {
	spaces := [Space.initial, .handshake]
	_ = spaces
}
', 'plain_enum_array_literal.v', prefs) or { panic(err) }
	assert plain_enum_array_source.contains('typedef array Array_Space;'), plain_enum_array_source
}

fn test_higher_order_lowering_handles_pointer_receivers_callbacks_and_keyword_parameters() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn convert(value int) int {
	return value + 1
}

fn consume(values []int) int {
	return values.len
}

fn apply(mut values []int) int {
	streamed := values.map(|short| short + 1)
	return streamed.len + consume(values.map(convert)) + consume(values.map(|short| short + 2))
}

fn main() {
	mut values := [1, 2]
	_ := apply(mut values)
}
', 'higher_order_nested_regressions.v', prefs) or { panic(err) }
	// Mutable array receivers are pointers in C. Both lowerers iterate a value copy of the
	// header, nested bare callbacks are invoked, and legal V names are escaped for C.
	assert c_source.contains('Array_int __v_fastc_collection_'), c_source
	assert c_source.contains('= (*(values));'), c_source
	assert !c_source.contains('Array_int* __v_fastc_collection_'), c_source
	assert c_source.contains('convert(it)'), c_source
	assert c_source.count('int __v_fastc_keyword_short =') == 2, c_source
	assert !c_source.contains('int short ='), c_source
}

fn test_real_builtin_path_keyword_identifiers_enum_casts_and_dollar_d() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	// FastC's real-`builtin` path (used for ordinary apps, not just self-host)
	// accepts constructs the bootstrap subset rejected: a keyword used as an
	// identifier (enum field `none`), a sized enum backing type with primitive-cast
	// values (`enum ... as u32 { ok = int(0) }`), and `$d('key', default)` compile
	// time defaults lowered to their default value.
	source := "module main

enum Mode {
	none
	manual
}

enum Code as u32 {
	ok  = int(0)
	err = int(1)
}

fn main() {
	m := Mode.none
	c := Code.ok
	limit := int(\$d('limit_bytes', 8192))
	println(int(m) + int(c) + limit)
}
"
	c_source := generate(source, 'real_builtin_keyword_identifiers.v', prefs) or { panic(err) }
	assert c_source.contains('Mode__none'), c_source
	assert c_source.contains('Code__ok'), c_source
	assert c_source.contains('8192'), c_source
}

fn test_duplicate_type_declarations_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\ntype UserId = int\ntype UserId = int\nfn main() {}\n',
		'module main\nstruct Item {}\ntype Item = int\nfn main() {}\n',
	] {
		mut message := ''
		_ := generate(source, 'duplicate_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('duplicate type declaration'), message
	}
}

fn test_global_declarations_require_enable_globals_or_module_attribute() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\n__global answer = 42\nfn main() {}\n', 'plain_global.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('use `v -enable-globals ...` to enable globals'), message

	attributed_source := generate('@[has_globals]\nmodule main\n__global answer = 42\nfn main() {}\n', 'attributed_global.v', prefs) or { panic(err) }
	assert attributed_source.contains('static int answer;'), attributed_source

	mut enabled_prefs := pref.new_preferences()
	enabled_prefs.enable_globals = true
	enabled_source := generate('module main\n__global answer = 42\nfn main() {}\n', 'enabled_global.v', enabled_prefs) or { panic(err) }
	assert enabled_source.contains('static int answer;'), enabled_source
}

fn test_duplicate_global_declarations_are_rejected() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	mut message := ''
	_ := generate('module main\n__global answer = 1\n__global answer = 2\nfn main() {}\n', 'duplicate_global.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('duplicate global `answer`'), message
}

fn test_generate_files_rejects_private_imported_globals() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_global_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(main_file, 'module main\nimport secrets\nfn main() { println(secrets.secret) }\n') or {
		panic(err)
	}
	os.write_file(module_file, 'module secrets\n__global secret = 42\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private global `secret` from imported module `secrets`'), message
	os.write_file(main_file, 'module main\nimport secrets\nconst copied = secrets.secret\nfn main() { println(copied) }\n') or {
		panic(err)
	}
	message = ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private global `secret` from imported module `secrets`'), message

	os.write_file(module_file, 'module secrets\npub __global secret = 42\n') or { panic(err) }
	os.write_file(main_file, 'module main\nimport secrets\nfn main() { println(secrets.secret) }\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('println(secrets__secret);'), c_source
}

fn test_generate_files_rejects_private_imported_types() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_types_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(module_file, 'module secrets

struct SecretStruct {}
enum SecretEnum { value }
interface SecretInterface {}
union SecretUnion { value int }
type SecretAlias = int
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	for type_name in ['SecretStruct', 'SecretEnum', 'SecretInterface', 'SecretUnion', 'SecretAlias'] {
		os.write_file(main_file, 'module main\nimport secrets\nfn consume(value secrets.${type_name}) {}\nfn main() {}\n') or {
			panic(err)
		}
		mut message := ''
		_ := generate_files([main_file], prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('private type `${type_name}` from imported module `secrets`'), message
	}

	os.write_file(module_file, 'module secrets\npub struct SecretStruct {}\n') or { panic(err) }
	os.write_file(main_file, 'module main\nimport secrets\nfn consume(value secrets.SecretStruct) {}\nfn main() {}\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('void consume(secrets__SecretStruct value);'), c_source
}

fn test_generate_files_restricts_unqualified_imported_type_lookup() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_type_scope_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'widgets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'widgets', 'widgets.v')
	os.write_file(module_file, 'module widgets\npub struct Widget {}\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]

	os.write_file(main_file, 'module main\nimport widgets\nfn consume(value Widget) {}\nfn main() {}\n') or {
		panic(err)
	}
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('undeclared type `Widget`'), message

	os.write_file(main_file, 'module main\nimport widgets { Widget }\nfn consume(value Widget) {}\nfn main() {}\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('void consume(widgets__Widget value);'), c_source

	os.write_file(module_file, 'module widgets\nstruct Widget {}\n') or { panic(err) }
	message = ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private type `Widget` from imported module `widgets`'), message

	os.write_file(main_file, 'module main\nimport widgets { Widget }\nfn main() { _ := Widget{} }\n') or {
		panic(err)
	}
	message = ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('unresolved name `Widget`'), message
}

fn test_selfhost_struct_field_defaults_are_preserved() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn default_retries() int {
	return 3
}

struct Config {
	retries int = default_retries()
}

fn main() {
	config := Config{}
	println(config.retries)
}
', 'struct_field_default.v', prefs) or { panic(err) }
	assert c_source.contains('int default_retries(void)'), c_source
	assert c_source.contains('__v_fastc_struct_default.retries=(default_retries());'), c_source
}

fn test_required_struct_fields_must_be_initialized() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	_ := generate('module main

struct Config {
	name string @[required]
}

fn main() {
	Config{}
}
', 'missing_required_struct_field.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('field `Config.name` must be initialized'), message

	generate('module main

struct Config {
	name string @[required]
}

fn main() {
	config := Config{name: "set"}
	println(config.name)
}
', 'initialized_required_struct_field.v', prefs) or { panic(err) }
}

fn test_generate_files_rejects_private_imported_struct_fields() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_fields_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'records')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'records', 'records.v')
	module_source := 'module records

pub struct Settings {
	secret int
pub:
	visible int
}

pub fn make() Settings {
	return Settings{secret: 1, visible: 2}
}
'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.module_search_paths = [root]
	for source in [
		'module main\nimport records\nfn main() { value := records.make(); println(value.secret) }\n',
		'module main\nimport records\nfn main() { value := records.Settings{secret: 1}; println(value.visible) }\n',
	] {
		mut message := ''
		if _, _, _ := generate_source_files([
			FastcSourceFile{
				path: main_file
				source: source
				header: fastc_scan_source_header(source, main_file, prefs) or { panic(err) }
			},
			FastcSourceFile{
				path: module_file
				source: module_source
				header: fastc_scan_source_header(module_source, module_file, prefs) or {
					panic(err)
				}
			},
		], map[string]string{}, prefs) {
			assert false, 'private field access unexpectedly compiled'
		} else {
			message = err.msg()
		}
		assert message.contains('private field `Settings.secret` from imported module `records`'), message
	}

	valid_source := 'module main\nimport records\nfn main() { value := records.Settings{visible: 2}; println(value.visible) }\n'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: main_file
			source: valid_source
			header: fastc_scan_source_header(valid_source, main_file, prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: module_file
			source: module_source
			header: fastc_scan_source_header(module_source, module_file, prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('.visible=(__v_fastc_struct_field_0)'), c_source
}

fn test_imported_public_field_mutability_is_preserved() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_field_mutability_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'settings')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'settings', 'settings.v')
	module_source := 'module settings

pub struct Config {
pub:
	read_only int
pub mut:
	writable int
}
'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	invalid_source := 'module main

import settings

fn main() {
	mut config := settings.Config{}
	config.read_only = 2
}
'
	mut message := ''
	if _, _, _ := generate_source_files([
		FastcSourceFile{
			path: main_file
			source: invalid_source
			header: fastc_scan_source_header(invalid_source, main_file, prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: module_file
			source: module_source
			header: fastc_scan_source_header(module_source, module_file, prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) {
		assert false, 'immutable field mutation unexpectedly compiled'
	} else {
		message = err.msg()
	}
	assert message.contains('mutation of immutable field `Config.read_only`'), message

	valid_source := 'module main

import settings

fn main() {
	mut config := settings.Config{}
	config.writable = 2
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: main_file
			source: valid_source
			header: fastc_scan_source_header(valid_source, main_file, prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: module_file
			source: module_source
			header: fastc_scan_source_header(module_source, module_file, prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('config.writable=2;'), c_source
}

fn test_struct_literal_fields_are_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for source, expected in {
		'module main\nstruct Config { value int }\nfn main() { config := Config{value: 1, value: 2}; println(config.value) }\n':                         'duplicate field `Config.value` in struct literal'
		'module main\nstruct Config { value int }\nfn main() { base := Config{}; config := Config{value: 2, ...base}; println(config.value) }\n':        'struct update expression must be first'
		'module main\nstruct Config { value int }\nfn main() { base := Config{}; config := Config{...base, ...base}; println(config.value) }\n':         'duplicate struct update expression'
		'module main\nstruct Config { values [2]int }\nfn main() { config := Config{values: [1]!}; println(config.values) }\n':                          'expects 2 elements, got 1'
		'module main\nconst size = 2\nstruct Config { values [size]int }\nfn main() { config := Config{values: [1, 2, 3]!}; println(config.values) }\n': 'expects 2 elements, got 3'
	} {
		mut message := ''
		_ := generate(source, 'invalid_struct_literal.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains(expected), message
	}

	c_source := generate('module main

struct Config {
	enabled bool
	value int
}

fn main() {
	enabled := true
	config := Config{enabled: enabled, value: 2}
	println(config.value)
}
', 'valid_struct_literal.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_struct_field_0 = (enabled);'), c_source
	assert c_source.contains('__v_fastc_struct_field_1 = (2);'), c_source
	assert c_source.contains('.enabled=(__v_fastc_struct_field_0)'), c_source
	assert c_source.contains('.value=(__v_fastc_struct_field_1)'), c_source

	update_source := generate('module main

struct Config {
	value int
}

fn main() {
	base := Config{}
	config := Config{...base, value: 2}
	println(config.value)
}
', 'valid_struct_update.v', prefs) or { panic(err) }
	assert update_source.contains('__v_fastc_struct_update'), update_source

	pointer_update_source := generate('module main

struct Config {
	value int
}

fn clone_config(base &Config) &Config {
	return &Config{...base, value: 2}
}

fn main() {
	base := &Config{}
	_ := clone_config(base)
}
', 'valid_pointer_struct_update.v', prefs) or { panic(err) }
	assert pointer_update_source.contains('Config __v_fastc_struct_update = *(base);'), pointer_update_source
	assert pointer_update_source.contains('v_fastc_interface_box(&__v_fastc_struct_update, sizeof(Config))'), pointer_update_source
}

fn test_selfhost_struct_field_initializers_preserve_source_order() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := 'module main

struct Pair {
	first int
	second int
}

fn next(value int) int {
	println(value)
	return value
}

fn main() {
	pair := Pair{second: next(2), first: next(1)}
	println(pair.first)
}
'
	c_source := generate(source, 'struct_field_initializer_order.v', prefs) or { panic(err) }
	second_initializer := c_source.index('__v_fastc_struct_field_0 = (next(2));') or { -1 }
	first_initializer := c_source.index('__v_fastc_struct_field_1 = (next(1));') or { -1 }
	assert second_initializer >= 0, c_source
	assert first_initializer > second_initializer, c_source
	assert c_source.contains('(Pair){.first=(__v_fastc_struct_field_1),.second=(__v_fastc_struct_field_0)}'), c_source
}

fn test_embedded_struct_fields_use_storage_paths() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Child {
	mut:
	count int
}

struct Leaf {
	mut:
	number int
}

struct Inner {
	Leaf

	mut:
	value int
	child Child
}

struct Outer {
	Inner
}

fn main() {
	mut outer := Outer{value: 3, number: 7}
	outer.child.count = 5
	println(outer.value)
	println(outer.child.count)
	println(outer.number)
	outer.value = 4
	outer.child.count = 6
	outer.number = 8
	println(outer.value)
	println(outer.child.count)
	println(outer.number)
}
', 'embedded_struct_fields.v', prefs) or { panic(err) }
	assert c_source.contains('.__embedded_0.value=(__v_fastc_struct_field_0)'), c_source
	assert c_source.contains('.__embedded_0.__embedded_0.number=(__v_fastc_struct_field_1)'), c_source
	assert c_source.contains('outer.__embedded_0.value'), c_source
	assert c_source.contains('outer.__embedded_0.child.count'), c_source
	assert c_source.contains('outer.__embedded_0.__embedded_0.number'), c_source
}

fn test_interface_dispatch_keeps_parameter_shape_and_mutability() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	type_mismatch_source := generate('module main

interface Worker {
	work(value int) int
}

struct Wrong {}

fn (wrong Wrong) work(value string) bool { return false }

fn main() {
	_ := Worker(Wrong{})
}
', 'interface_type_mismatch.v', prefs) or { panic(err) }
	assert type_mismatch_source.len > 0

	mut mutability_message := ''
	_ := generate('module main

interface Worker {
	work(mut value int) int
}

struct Wrong {}

fn (wrong Wrong) work(value int) int { return value }

fn main() {
	_ := Worker(Wrong{})
}
', 'interface_mutability_mismatch.v', prefs) or {
		mutability_message = err.msg()
		''
	}
	assert mutability_message.contains('incompatible mutability'), mutability_message

	c_source := generate('module main

interface Worker {
	work(value int) int
	update(mut value int)
}

struct Good {}

fn (good Good) work(value int) int {
	return value
}

fn (good Good) update(mut value int) {
	value = 2
}

fn main() {
	worker := Worker(Good{})
	println(worker.work(1))
	mut value := 1
	worker.update(mut value)
}
', 'valid_interface_implementation.v', prefs) or { panic(err) }
	assert c_source.contains('case __v_typeid_Good:'), c_source
	assert c_source.contains('void Worker_update(Worker value, int* arg1)'), c_source
}

fn test_selfhost_interface_preserves_type_only_shared_parameter() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Box {}

interface User {
	use(shared Box)
}

struct Impl {}

fn (value Impl) use(shared box Box) {}

fn main() {
	_ := User(Impl{})
}
', 'selfhost_interface_type_only_shared_parameter.v', prefs) or { panic(err) }
	assert c_source.contains('void User_use(User value, Box* arg1)'), c_source
}

fn test_selfhost_unused_interface_dispatches_do_not_reference_pruned_methods() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Reader {
	read() !int
}

struct File {}

fn (file File) read() !int {
	return 1
}

fn main() {}
', 'unused_interface_dispatch.v', prefs) or { panic(err) }
	assert c_source.contains('Option Reader_read('), c_source
	assert !c_source.contains('Option File_read('), c_source
}

fn test_selfhost_constant_interface_cast_uses_interface_value() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

interface Failure {
	message() string
}

struct Message {
	text string
}

fn (message Message) message() string {
	return message.text
}

const sentinel = Failure(&Message{text: 'error'})

fn main() {
	println(sentinel.message())
}
", 'constant_interface_cast.v', prefs) or { panic(err) }
	assert c_source.contains('main__sentinel = (Failure){._object=(void*)('), c_source
	assert c_source.contains('._typ=__v_typeid_Message, ._methods=NULL};'), c_source
	assert !c_source.contains('main__sentinel = ((Failure)('), c_source
}

fn test_interface_receiver_mutability_is_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for source in [
		'module main

interface Worker {
	work()
}

struct Wrong {}

fn (mut wrong Wrong) work() {}

fn main() {
	_ := Worker(Wrong{})
}
',
		'module main

interface Worker {
mut:
	work()
}

struct Wrong {}

fn (wrong Wrong) work() {}

fn main() {
	_ := Worker(Wrong{})
}
',
	] {
		mut message := ''
		_ := generate(source, 'invalid_interface_receiver_mutability.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('incompatible mutability for interface `Worker` method `work`'), message
	}

	mut immutable_message := ''
	_ := generate('module main

interface Worker {
mut:
	work()
}

struct Good {}

fn (mut good Good) work() {}

fn main() {
	worker := Worker(Good{})
	worker.work()
}
', 'immutable_mutable_interface_receiver.v', prefs) or {
		immutable_message = err.msg()
		''
	}
	assert immutable_message.contains('mutating method `work` receiver `worker` is immutable'), immutable_message

	c_source := generate('module main

interface Worker {
mut:
	work()
}

struct Good {}

fn (mut good Good) work() {}

fn main() {
	mut worker := Worker(Good{})
	worker.work()
}
', 'mutable_interface_receiver.v', prefs) or { panic(err) }
	assert c_source.contains('void Worker_work(Worker value)'), c_source
	assert c_source.contains('Worker_work(worker);'), c_source
}

fn test_interface_cast_validates_required_fields() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut missing_message := ''
	_ := generate('module main

interface Named {
	name string
}

struct Empty {}

fn main() {
	_ := Named(Empty{})
}
', 'interface_missing_field.v', prefs) or {
		missing_message = err.msg()
		''
	}
	assert missing_message.contains('does not implement interface `Named` field `name`'), missing_message

	type_source := generate('module main

interface Named {
	name string
}

struct Wrong {
	name bool
}

fn main() {
	_ := Named(Wrong{})
}
', 'interface_wrong_field_type.v', prefs) or { panic(err) }
	assert type_source.contains('Named'), type_source

	mut mutability_message := ''
	_ := generate('module main

interface Named {
mut:
	name string
}

struct Wrong {
	name string
}

fn main() {
	_ := Named(Wrong{})
}
', 'interface_immutable_field.v', prefs) or {
		mutability_message = err.msg()
		''
	}
	assert mutability_message.contains('field `name` must be mutable'), mutability_message

	c_source := generate('module main

interface Named {
	name string
mut:
	count int
}

struct Good {
	name string
mut:
	count int
}

fn main() {
	_ := Named(Good{})
}
', 'valid_interface_fields.v', prefs) or { panic(err) }
	assert c_source.contains('struct Named { void *_object; u32 _typ; void *_methods; };'), c_source
}

fn test_disabled_function_attributes_emit_empty_stubs() {
	mut prefs := pref.new_preferences()
	prefs.user_defines = []
	c_source := generate('module main

@[if fastc_missing_define ?]
fn traced(value int) {
	println("must not run")
}

fn side_effect() int {
	println(99)
	return 1
}

fn main() {
	traced(side_effect())
	println(0)
}
', 'disabled_function_attribute.v', prefs) or { panic(err) }
	assert c_source.contains('void traced(int value) {\n}')
	assert !c_source.contains('must not run')
	assert !c_source.contains('traced(side_effect())')
	assert c_source.contains('((void)0);')

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_disabled_call_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '0\n'

	module_dir := os.join_path(root, 'tracing')
	os.mkdir_all(module_dir) or { panic(err) }
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(module_dir, 'tracing.v')
	os.write_file(main_file, 'module main

import tracing

fn side_effect() int {
	println(99)
	return 1
}

fn main() {
	tracing.trace(side_effect())
	println(0)
}
') or {
		panic(err)
	}
	os.write_file(module_file, 'module tracing

@[if fastc_missing_define ?]
pub fn trace(value int) {}
') or {
		panic(err)
	}
	prefs.module_search_paths = [root]
	imported_source := generate_files([main_file], prefs) or { panic(err) }
	assert !imported_source.contains('tracing__trace(side_effect())')
	os.write_file(c_file, imported_source) or { panic(err) }
	imported_compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert imported_compile_result.exit_code == 0, imported_compile_result.output
	imported_run_result := cmdexec.run(bin_file, [])
	assert imported_run_result.exit_code == 0, imported_run_result.output
	assert imported_run_result.output == '0\n'

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	method_source := generate('module main

struct Tracer {}

@[if fastc_missing_define ?]
fn (tracer Tracer) trace(value int) {}

fn side_effect() int {
	return 1
}

fn run(tracer Tracer) {
	tracer.trace(side_effect())
}

fn main() {}
', 'disabled_method_attribute.v', selfhost_prefs) or { panic(err) }
	assert !method_source.contains('Tracer_trace(tracer,side_effect())')
	assert method_source.contains('((void)0);')
}

fn test_compound_function_attributes_evaluate_the_complete_condition() {
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

@[if linux && windows]
fn impossible() {
	println("disabled compound condition")
}

@[if linux || windows]
fn supported() {
	println("enabled compound condition")
}

fn main() {
	impossible()
	supported()
}
', 'compound_function_attribute.v', prefs) or { panic(err) }
	assert c_source.contains('void impossible(void) {\n}')
	assert !c_source.contains('disabled compound condition')
	assert c_source.contains('enabled compound condition')
}

fn test_disabled_type_attributes_skip_collection_and_emission() {
	prefs := pref.new_preferences()
	c_source := generate('module main

@[if fastc_missing_define ?]
struct DisabledStruct {
	bad MissingDisabledType
}

@[if fastc_missing_define ?]
union DisabledUnion {
	bad MissingDisabledType
}

@[if fastc_missing_define ?]
enum DisabledEnum {
	value
}

@[if fastc_missing_define ?]
interface DisabledInterface {
	bad(value MissingDisabledType)
}

@[if fastc_missing_define ?]
type DisabledAlias = MissingDisabledType

fn main() {
	println(7)
}
', 'disabled_type_attribute.v', prefs) or { panic(err) }
	for disabled_name in ['DisabledStruct', 'DisabledUnion', 'DisabledEnum', 'DisabledInterface',
		'DisabledAlias', 'MissingDisabledType'] {
		assert !c_source.contains(disabled_name), c_source
	}
}

fn test_selected_top_level_comptime_function_signatures_are_collected() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

$if windows {
	fn platform() string {
		return "wrong"
	}
} $else $if linux {
	fn platform() int {
		return 42
	}
} $else {
	fn platform() bool {
		return false
	}
}

fn main() {
	println(platform())
}
', 'top_level_comptime_function.v', prefs) or { panic(err) }
	assert c_source.contains('int platform(void)'), c_source
	assert c_source.contains('println(platform());'), c_source
	assert !c_source.contains('return "wrong";'), c_source
}

fn test_selected_top_level_comptime_types_are_collected_and_emitted() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

$if windows {
	struct Choice {
		wrong bool
	}
} $else $if linux {
	struct Choice {
		value int
	}

	enum Mode {
		selected
	}

	type ChoiceId = int

	union Payload {
		number int
	}

	interface Named {
		name() string
	}
}

fn main() {
	choice := Choice{
		value: 42
	}
	println(choice.value)
}
', 'top_level_comptime_types.v', prefs) or { panic(err) }
	assert c_source.contains('struct Choice {\n\tint value;'), c_source
	assert !c_source.contains('bool wrong;'), c_source
	assert c_source.contains('#define Mode__selected ((Mode)0)'), c_source
	assert c_source.contains('typedef int ChoiceId;'), c_source
	assert c_source.contains('union Payload {\n\tint number;'), c_source
	assert c_source.contains('struct Named { void *_object; u32 _typ; void *_methods; };'), c_source
	assert c_source.contains('Named_name(Named value) {'), c_source
	assert c_source.contains('__typeof__((({ __typeof__((42)) __v_fastc_struct_field_0 = (42); (Choice){.value=(__v_fastc_struct_field_0)}; }))) choice'), c_source
}

fn test_selected_top_level_comptime_constants_are_collected_and_emitted() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

$if windows {
	const answer = "wrong"
} $else $if linux {
	const answer = 42
} $else {
	const answer = false
}

fn main() {
	println(answer)
}
', 'top_level_comptime_constant.v', prefs) or { panic(err) }
	assert c_source.contains('#define main__answer (42)'), c_source
	assert c_source.contains('println(main__answer);'), c_source
	assert !c_source.contains('wrong'), c_source
}

fn test_selected_top_level_comptime_globals_are_collected_and_emitted() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

$if windows {
	__global state = "wrong"
} $else $if linux {
	__global state = 42
} $else {
	__global state = false
}

fn main() {
	println(state)
}
', 'top_level_comptime_global.v', prefs) or { panic(err) }
	assert c_source.contains('static int state;'), c_source
	assert c_source.contains('\tstate = 42;'), c_source
	assert c_source.contains('println(state);'), c_source
	assert !c_source.contains('wrong'), c_source
}

fn test_initialized_global_value_is_emitted() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global answer = 42

fn main() {
	println(answer)
}
', 'initialized_global.v', prefs) or { panic(err) }
	assert c_source.contains('static int answer;'), c_source
	assert c_source.contains('\tanswer = 42;'), c_source
	assert c_source.contains('v_fastc_init_globals();'), c_source
}

fn test_script_main_initializes_globals_before_statements() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global answer = 42

fn init() {
	answer = 43
}

println(answer)
', 'initialized_script_global.v', prefs) or { panic(err) }
	main_source := c_source.all_after('int main(void) {')
	startup_source := c_source.all_after('static void v_fastc_init_globals(void) {')
	initializer := startup_source.index('answer = 42;') or { -1 }
	module_initializer := startup_source.index('\n\tinit();') or { -1 }
	startup_call := main_source.index('v_fastc_init_globals();') or { -1 }
	statement := main_source.index('println(answer);') or { -1 }
	assert initializer >= 0, c_source
	assert module_initializer > initializer, c_source
	assert startup_call >= 0, c_source
	assert statement > startup_call, c_source
}

fn test_runtime_constants_are_materialized_exactly_once() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_runtime_constants_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global calls int

const value = next()
const unused = next()

fn next() int {
	calls++
	return calls
}

fn main() {
	println(value)
	println(value)
	println(calls)
}
', 'runtime_constants.v', prefs) or { panic(err) }
	assert c_source.contains('static int main__value;'), c_source
	assert c_source.contains('static int main__unused;'), c_source
	assert !c_source.contains('#define main__value'), c_source
	assert c_source.count('main__value = next();') == 1, c_source
	assert c_source.count('main__unused = next();') == 1, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '1\n1\n2'
}

fn test_runtime_constant_initializers_follow_module_dependencies() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_constant_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	dep_file := os.join_path(root, 'dep', 'dep.v')
	os.write_file(main_file, 'module main

import dep

const copied = dep.original

fn main() {
	println(copied)
}
') or {
		panic(err)
	}
	os.write_file(dep_file, 'module dep

pub const original = next()

pub fn next() int {
	return 42
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	dependency_initializer := c_source.index('dep__original = dep__next();') or { -1 }
	importer_initializer := c_source.index('copied = dep__original;') or { -1 }
	assert dependency_initializer >= 0, c_source
	assert importer_initializer > dependency_initializer, c_source
}

fn test_imported_global_initializers_run_before_importer_globals() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_global_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	dep_file := os.join_path(root, 'dep', 'dep.v')
	os.write_file(main_file, 'module main

import dep

__global copied = dep.current()

fn main() {
	println(copied)
}
') or {
		panic(err)
	}
	os.write_file(dep_file, 'module dep

__global current_value = 42

pub fn current() int {
	return current_value
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	dependency_initializer := c_source.index('dep__current_value = 42;') or { -1 }
	importer_initializer := c_source.index('copied = dep__current();') or { -1 }
	assert dependency_initializer >= 0, c_source
	assert importer_initializer > dependency_initializer, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '42'
}

fn test_module_initializers_run_in_dependency_order_before_main() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_module_init_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	dep_file := os.join_path(root, 'dep', 'dep.v')
	os.write_file(main_file, 'module main

import dep

__global observed = 0

const copied = dep.value()

fn init() {
	observed = copied + 1
}

fn main() {
	println(observed)
}
') or {
		panic(err)
	}
	os.write_file(dep_file, 'module dep

__global state = 1

fn init() {
	state = 41
}

pub fn value() int {
	return state
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	main_source := c_source.all_after('int main(void) {')
	startup_source := c_source.all_after('static void v_fastc_init_globals(void) {')
	dependency_initializer := startup_source.index('dep__state = 1;') or { -1 }
	dependency_init := startup_source.index('\tdep__init();') or { -1 }
	importer_initializer := startup_source.index('main__copied = dep__value();') or { -1 }
	entry_global_initializer := startup_source.index('observed = 0;') or { -1 }
	entry_init := startup_source.index('\n\tinit();') or { -1 }
	startup_call := main_source.index('v_fastc_init_globals();') or { -1 }
	main_statement := main_source.index('println(observed);') or { -1 }
	assert dependency_initializer >= 0, c_source
	assert dependency_init > dependency_initializer, c_source
	assert importer_initializer > dependency_init, c_source
	assert entry_global_initializer > importer_initializer, c_source
	assert entry_init > entry_global_initializer, c_source
	assert startup_call >= 0, c_source
	assert main_statement > startup_call, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '42'
}

fn test_module_cleanup_hooks_run_in_reverse_order_on_main_return() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_module_cleanup_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	dep_file := os.join_path(root, 'dep', 'dep.v')
	os.write_file(main_file, "module main

import dep

fn init() {
	println('main init')
}

fn cleanup() {
	println('main cleanup')
}

fn main() {
	dep.ping()
	defer {
		println('main defer')
	}
	println('main')
	if true {
		return
	}
	println('unreachable')
}
") or {
		panic(err)
	}
	os.write_file(dep_file, "module dep

fn init() {
	println('dep init')
}

fn cleanup() {
	println('dep cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	cleanup_source := c_source.all_after('static void v_fastc_cleanup_modules(void) {')
	main_cleanup := cleanup_source.index('\n\tcleanup();') or { -1 }
	dependency_cleanup := cleanup_source.index('\n\tdep__cleanup();') or { -1 }
	assert main_cleanup >= 0, c_source
	assert dependency_cleanup > main_cleanup, c_source
	main_source := c_source.all_after('int main(void) {')
	startup_call := main_source.index('v_fastc_init_globals();') or { -1 }
	cleanup_registration := main_source.index('atexit(v_fastc_cleanup_modules);') or { -1 }
	early_return := main_source.index('return 0;') or { -1 }
	assert startup_call >= 0, c_source
	assert cleanup_registration > startup_call, c_source
	assert early_return > cleanup_registration, c_source
	assert !main_source.contains('v_fastc_cleanup_modules();'), c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'dep init\nmain init\nmain\nmain defer\nmain cleanup\ndep cleanup'
}

fn test_module_cleanup_runs_when_user_code_calls_exit() {
	prefs := pref.new_preferences()
	c_source := generate("module main

fn C.exit(code int)

fn cleanup() {
	println('cleanup')
}

fn main() {
	println('main')
	C.exit(0)
	println('unreachable')
}
", 'cleanup_after_exit.c.v', prefs) or { panic(err) }
	main_source := c_source.all_after('int main(void) {')
	registration := main_source.index('atexit(v_fastc_cleanup_modules);') or { -1 }
	exit_call := main_source.index('exit(0);') or { -1 }
	assert registration >= 0, c_source
	assert exit_call > registration, c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_cleanup_exit_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'main\ncleanup\n'
}

fn test_module_lifecycle_preserves_source_import_order() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_import_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'zed')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'alpha')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, "module main

import zed
import alpha

fn main() {
	zed.ping()
	alpha.ping()
	println('main')
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'zed', 'zed.v'), "module zed

fn init() {
	println('zed init')
}

fn cleanup() {
	println('zed cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'alpha', 'alpha.v'), "module alpha

fn init() {
	println('alpha init')
}

fn cleanup() {
	println('alpha cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	header := fastc_scan_source_header(os.read_file(main_file) or { panic(err) }, main_file, prefs) or {
		panic(err)
	}
	assert header.import_order == ['zed', 'alpha']
	c_source := generate_files([main_file], prefs) or { panic(err) }
	startup_source := c_source.all_after('static void v_fastc_init_globals(void) {')
	zed_init := startup_source.index('\tzed__init();') or { -1 }
	alpha_init := startup_source.index('\talpha__init();') or { -1 }
	assert zed_init >= 0, c_source
	assert alpha_init > zed_init, c_source
	cleanup_source := c_source.all_after('static void v_fastc_cleanup_modules(void) {')
	alpha_cleanup := cleanup_source.index('\talpha__cleanup();') or { -1 }
	zed_cleanup := cleanup_source.index('\tzed__cleanup();') or { -1 }
	assert alpha_cleanup >= 0, c_source
	assert zed_cleanup > alpha_cleanup, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'zed init\nalpha init\nmain\nalpha cleanup\nzed cleanup'
}

fn test_module_initializer_arity_is_validated_but_return_type_is_not() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn init(value int) {}\nfn main() {}\n', 'invalid_module_init.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('module `init` with parameters'), message
	generate('module main\nfn init() int { return 1 }\nfn main() {}\n', 'value_returning_module_init.v', prefs) or { panic(err) }
}

fn test_module_cleanup_arity_is_validated_but_return_type_is_not() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn cleanup(value int) {}\nfn main() {}\n', 'invalid_module_cleanup.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('module `cleanup` with parameters'), message
	generate('module main\nfn cleanup() int { return 1 }\nfn main() {}\n', 'value_returning_module_cleanup.v', prefs) or { panic(err) }
}

fn test_negative_enum_discriminants_are_preserved() {
	prefs := pref.new_preferences()
	c_source := generate('module main

enum Foo {
	a = 1
	d = -10
	e
}

fn main() {}
', 'negative_enum_discriminant.v', prefs) or { panic(err) }
	assert c_source.contains('#define Foo__d ((Foo)-10)'), c_source
	assert c_source.contains('#define Foo__e ((Foo)-9)'), c_source
}

fn test_duplicate_enum_fields_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

enum Item {
	value
	value
}

fn main() {}
', 'duplicate_enum_field.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('duplicate enum field `Item.value`'), message
}

fn test_flag_enum_custom_values_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

@[flag]
enum Permissions {
	read = 4
}

fn main() {}
', 'flag_enum_custom_value.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('custom value for flag enum field `Permissions.read`'), message
}

fn test_flag_mutating_methods_require_mutable_receivers() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut ordinary_message := ''
	_ := generate('module main

enum Color {
	red
	green
}

fn main() {
	mut color := Color.red
	color.set(.green)
}
', 'ordinary_enum_flag_method.v', prefs) or {
		ordinary_message = err.msg()
		''
	}
	assert ordinary_message.contains('unresolved method call'), ordinary_message

	for source, receiver_name in {
		'module main\n@[flag]\nenum Permissions { read write }\nfn main() { flags := Permissions.read; flags.set(.write) }\n':                                                   'flags'
		'module main\n@[flag]\nenum Permissions { read write }\nstruct Holder { permissions Permissions }\nfn main() { holder := Holder{}; holder.permissions.clear(.read) }\n': 'holder'
	} {
		mut message := ''
		_ := generate(source, 'immutable_flag_receiver.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('receiver `${receiver_name}` is immutable'), message
	}
	module_source := 'module settings

@[flag]
pub enum Permissions {
	read
	write
}

pub struct Config {
pub:
	permissions Permissions
}
'
	main_source := 'module main

import settings

fn main() {
	mut config := settings.Config{}
	config.permissions.set(.write)
}
'
	mut field_message := ''
	if _, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'immutable_flag_field.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'immutable_flag_field.v', prefs) or {
				panic(err)
			}
		},
		FastcSourceFile{
			path: 'settings.v'
			source: module_source
			header: fastc_scan_source_header(module_source, 'settings.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) {
		assert false, 'immutable flag receiver unexpectedly compiled'
	} else {
		field_message = err.msg()
	}
	assert field_message.contains('receiver field `Config.permissions` is not `pub mut`'), field_message

	c_source := generate('module main

@[flag]
enum Permissions {
	read
	write
}

fn main() {
	mut flags := Permissions.read
	flags.set(.write)
	flags.clear(.read)
}
', 'mutable_flag_receiver.v', prefs) or { panic(err) }
	assert c_source.contains('flags) |= (Permissions__write)'), c_source
	assert c_source.contains('flags) &= ~(Permissions__read)'), c_source
}

fn test_flag_method_argument_types_are_not_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for call in ['flags.set(1)', 'println(flags.has(Other.write))', 'flags.clear(Other.write)'] {
		c_source := generate('module main\n\n@[flag]\nenum Permissions {\n\tread\n\twrite\n}\n\n@[flag]\nenum Other {\n\tread\n\twrite\n}\n\nfn main() {\n\tmut flags := Permissions.read\n\t${call}\n}\n', 'flag_enum_argument.v', prefs) or { panic(err) }
		assert c_source.contains('flags'), c_source
	}
}

fn test_large_flag_enums_use_unsigned_64_bit_values() {
	prefs := pref.new_preferences()
	c_source := generate(r"module main

@[flag]
enum PawnsBoard as u64 {
	a8 b8 c8 d8 e8 f8 g8 h8
	a7 b7 c7 d7 e7 f7 g7 h7
	a6 b6 c6 d6 e6 f6 g6 h6
	a5 b5 c5 d5 e5 f5 g5 h5
	a4 b4 c4 d4 e4 f4 g4 h4
	a3 b3 c3 d3 e3 f3 g3 h3
	a2 b2 c2 d2 e2 f2 g2 h2
	a1 b1 c1 d1 e1 f1 g1 h1
}

fn main() {
	println(u64(PawnsBoard.h1))
	println('${PawnsBoard.h1:020x}')
}
", 'flag_enum_64.v', prefs) or { panic(err) }
	assert c_source.contains('typedef u64 PawnsBoard;'), c_source
	assert c_source.contains('#define PawnsBoard__h1 ((PawnsBoard)(((u64)1) << (63)))'), c_source
	assert c_source.contains('v_fastc_unsigned_format((unsigned long long)(PawnsBoard__h1), "020x")'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_flag_enum_64_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '9223372036854775808\n00008000000000000000\n'
}

fn test_mutable_receiver_methods_auto_address_mutable_values() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	declarations := 'module main

struct Holder {
	value int
}

fn (mut holder Holder) reset() {}
'
	mut message := ''
	_ := generate(declarations + '
fn main() {
	holder := Holder{}
	holder.reset()
}
', 'immutable_method_receiver.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutating method `reset` receiver `holder` is immutable'), message

	c_source := generate(declarations + '
fn main() {
	mut holder := Holder{}
	holder.reset()
}
', 'mutable_method_receiver.v', prefs) or { panic(err) }
	assert c_source.contains('void Holder_reset(Holder* holder)'), c_source
	assert c_source.contains('Holder_reset(&(holder))'), c_source
}

fn test_symbolic_enum_discriminants_and_printing_are_preserved() {
	prefs := pref.new_preferences()
	c_source := generate('module main

const base = 10

enum Color {
	red = base
	green
	blue = base + 1 << 1
}

fn main() {
	println(int(Color.red))
	println(int(Color.blue))
	print(Color.green)
	println(Color.red)
}
', 'symbolic_enum_discriminant.v', prefs) or { panic(err) }
	assert c_source.contains('#define Color__red ((Color)main__base)'), c_source
	assert c_source.contains('#define Color__green ((Color)(main__base + 1))'), c_source
	assert c_source.contains('v_fastc_print_enum_Color(Color__green, false);'), c_source
	assert c_source.contains('v_fastc_print_enum_Color(Color__red, true);'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_symbolic_enum_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '10\n12\ngreenred\n'

	ordinary_interpolation_source := generate(r"module main

enum Color {
	red
	green = 15
}

fn main() {
	println('${Color.red}')
	println('|${Color.red:5}|${Color.red:-5}|')
	println('${Color.green:04x}')
}
", 'ordinary_enum_interpolation.v', prefs) or { panic(err) }
	assert ordinary_interpolation_source.contains('static string v_fastc_enum_str_Color(Color value)'), ordinary_interpolation_source
	assert ordinary_interpolation_source.contains('v_fastc_enum_str_Color(Color__red)'), ordinary_interpolation_source
	assert ordinary_interpolation_source.contains('v_fastc_string_pad(v_fastc_enum_str_Color(Color__red), 5, false)'), ordinary_interpolation_source
	assert ordinary_interpolation_source.contains('v_fastc_string_pad(v_fastc_enum_str_Color(Color__red), 5, true)'), ordinary_interpolation_source
	assert ordinary_interpolation_source.contains('v_fastc_signed_format((long long)(Color__green), "04x")'), ordinary_interpolation_source
	os.write_file(c_file, ordinary_interpolation_source) or { panic(err) }
	interpolation_compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert interpolation_compile_result.exit_code == 0, interpolation_compile_result.output
	interpolation_run_result := cmdexec.run(bin_file, [])
	assert interpolation_run_result.exit_code == 0, interpolation_run_result.output
	assert interpolation_run_result.output == 'red\n|  red|red  |\n000f\n'

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	interpolation_source := generate(r"module main

enum Color {
	red
	green
}

@[flag]
enum Permissions {
	read
	write
}

fn color_label(color Color) string {
	return '${color}'
}

fn color_number(color Color) string {
	return '${color:d}'
}

fn permissions_label(permissions Permissions) string {
	return '${permissions}'
}

fn main() {
	println(color_label(Color.green))
	println(color_number(Color.green))
	println(permissions_label(Permissions.read))
}
", 'enum_interpolation.v', selfhost_prefs) or { panic(err) }
	assert interpolation_source.contains('static string v_fastc_enum_str_Color(Color value)'), interpolation_source
	assert interpolation_source.contains('if (value == Color__green) return _S("green");'), interpolation_source
	assert interpolation_source.contains('return _S("unknown enum value");'), interpolation_source
	assert interpolation_source.contains('v_fastc_enum_str_Color(color)'), interpolation_source
	assert interpolation_source.contains('v_fastc_signed_format((long long)(color), "d")'), interpolation_source
	assert interpolation_source.contains('static string v_fastc_enum_str_Permissions(Permissions value)'), interpolation_source
	assert interpolation_source.contains('_S("Permissions{")'), interpolation_source
	assert interpolation_source.contains('v_fastc_enum_str_Permissions(permissions)'), interpolation_source

	custom_str_source := generate(r"module main

enum Kind {
	assign
}

fn (kind Kind) str() string {
	return '='
}

fn main() {
	println(Kind.assign.str())
	println('${Kind.assign}')
}
", 'enum_custom_str_interpolation.v', selfhost_prefs) or { panic(err) }
	assert custom_str_source.contains('Kind_str(Kind__assign)'), custom_str_source
}

fn test_invalid_enum_printing_matches_interpolation() {
	prefs := pref.new_preferences()
	c_source := generate(r"module main

enum Color {
	red
	green
}

fn main() {
	value := unsafe { Color(99) }
	println(value)
	println('${value}')
}
", 'invalid_enum_print.v', prefs) or { panic(err) }
	assert c_source.contains('else fputs("unknown enum value", stdout);'), c_source
	assert c_source.contains('return _S("unknown enum value");'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_invalid_enum_print_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'unknown enum value\nunknown enum value\n'
}

fn test_enum_alias_member_access_uses_underlying_enum_symbols() {
	prefs := pref.new_preferences()
	c_source := generate('module main

enum MyEnum {
	something
	another
}

type MyEnumAlias = MyEnum

fn main() {
	x := MyEnum.something
	a := MyEnumAlias.something
	println(x == a)
	println(MyEnumAlias.another)
	println(MyEnumAlias(MyEnum.another))
	println(int(MyEnumAlias.another))
}
', 'enum_alias_member.v', prefs) or { panic(err) }
	assert c_source.contains('typedef MyEnum MyEnumAlias;'), c_source
	assert c_source.contains('MyEnum__something'), c_source
	assert c_source.contains('MyEnum__another'), c_source
	assert !c_source.contains('MyEnumAlias__something'), c_source
	assert !c_source.contains('MyEnumAlias__another'), c_source
	assert c_source.count('v_fastc_print_enum_MyEnum') == 3, c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_enum_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'true\nanother\nanother\n1\n'
}

fn test_unresolved_enum_discriminants_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

enum Color {
	red = missing
}

fn main() {}
', 'unresolved_enum_discriminant.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('unresolved enum discriminant name `missing`'), message
}

fn test_select_statements_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	select {
		value := <-messages { println(value) }
		else { println(0) }
	}
}
', 'select_statement.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('fastc parser does not support select statements'), message
}

fn test_unresolved_names_are_rejected_before_c_emission() {
	prefs := pref.new_preferences()
	for source in [
		"module main\nfn main() { puts('hello') }\n",
		'module main\nfn main() { printf("hello") }\n',
		'module main\nfn main() { value := stdout; println(value) }\n',
	] {
		mut message := ''
		_ := generate(source, 'unresolved_name.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('fastc parser does not support unresolved name'), message
	}
}

fn test_declared_names_are_available_without_an_ast() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(later(2))
}

fn later(value int) int {
	return value + 1
}
', 'declared_names.v', prefs) or { panic(err) }
	assert c_source.contains('println(later(2));')
}

fn test_narrow_integer_cast_expressions_are_lowered_without_type_validation() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(u8(255) + u8(1))
}
', 'narrow_cast_expression.v', prefs) or { panic(err) }
	assert c_source.contains('((u8)(255))+((u8)(1))'), c_source
}

fn test_inferred_array_element_types_are_not_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn main() {
	values := [1, true]
}
', 'inferred_array_element_types.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__new_array_from_c_array(2, 2, sizeof(int)'), c_source
}

fn test_undeclared_function_signature_types_are_rejected() {
	prefs := pref.new_preferences()
	for source, undeclared_type in {
		'module main\nfn show(x size_t) { println(1) }\nfn main() { show(1) }\n':        'size_t'
		'module main\nfn value() size_t { return 1 }\nfn main() { println(value()) }\n': 'size_t'
		'module main\nfn consume(x Foo) {}\nfn main() {}\n':                             'Foo'
		'module main\nfn value() ID { return unsafe { nil } }\nfn main() {}\n':          'ID'
	} {
		mut message := ''
		_ := generate(source, 'undeclared_signature_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('undeclared type `${undeclared_type}`'), message
	}
}

fn test_declared_function_call_argument_types_are_not_validated() {
	prefs := pref.new_preferences()
	invalid_c_source := generate('module main

fn show(x bool) {
	println(x)
}

fn main() {
	show(2)
}
', 'invalid_call_argument.v', prefs) or { panic(err) }
	assert invalid_c_source.contains('show(2);'), invalid_c_source

	c_source := generate('module main

fn increment(x int) int {
	return x + 1
}

fn show(x bool) {
	println(x)
}

fn main() {
	value := 2
	flag := true
	println(increment(value))
	show(flag)
}
', 'valid_call_arguments.v', prefs) or { panic(err) }
	assert c_source.contains('println(increment(value));')
	assert c_source.contains('show(flag);')
}

fn test_selfhost_under_arity_calls_require_params_structs() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for source in [
		'module main\nfn f(x int) {}\nfn main() { f() }\n',
		'module main\n@[params]\nstruct Config {}\nfn f(x int, y int, config Config) {}\nfn main() { f(1) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_under_arity_call.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('function `f` call with'), message
	}

	c_source := generate('module main

@[params]
struct Config {
	value int
}

fn with_config(value int, config Config) {}

fn main() {
	with_config(1)
}
', 'valid_omitted_params_struct.v', prefs) or { panic(err) }
	assert c_source.contains('with_config(1,(Config){0});'), c_source
}

fn test_variadic_call_argument_types_are_not_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	invalid_c_source := generate('module main

fn consume(values ...int) {}

fn main() {
	consume(true)
}
', 'invalid_variadic_argument.v', prefs) or { panic(err) }
	assert invalid_c_source.contains('(int[]){((bool)true)}'), invalid_c_source

	c_source := generate('module main

fn consume(values ...int) {}

fn main() {
	consume(1, 2)
}
', 'valid_variadic_arguments.v', prefs) or { panic(err) }
	assert c_source.contains('sizeof(int), (int[]){1,2}'), c_source
}

fn test_selfhost_variadic_struct_named_arguments_are_packed_as_one_element() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Kind {
	text
}

struct Config {
	key   Kind
	value string
}

fn consume(configs ...Config) {}

fn main() {
	consume(
		key: .text
		value: "plain"
	)
}
', 'variadic_named_struct.v', prefs) or { panic(err) }
	assert c_source.contains('sizeof(Config), (Config[]){({'), c_source
	assert c_source.contains('.key=('), c_source
	assert c_source.contains('Kind__text'), c_source
}

fn test_scanner_diagnostics_are_rejected() {
	prefs := pref.new_preferences()
	source := "module main\nfn main() { println('" + r'\_' + "') }\n"
	mut message := ''
	_ := generate(source, 'scanner_diagnostic.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('fastc scanner error'), message
	assert message.contains('`_` unknown escape sequence'), message
}

fn test_condition_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { if 2 { println(1) } }\n',
		'module main\nfn main() { value := 2; for value { break } }\n',
		'module main\nfn main() { for 2 { break } }\n',
		'module main\nfn main() { for i := 0; 2; i++ { break } }\n',
	] {
		generate(source, 'non_boolean_condition.v', prefs) or { panic(err) }
	}

	c_source := generate('module main

fn ready() bool {
	return true
}

fn main() {
	flag := true
	if flag {
		println(1)
	}
	for ready() {
		break
	}
	for i := 0; ready(); i++ {
		break
	}
}
', 'boolean_conditions.v', prefs) or { panic(err) }
	assert c_source.contains('if (flag) {')
	assert c_source.contains('while (ready()) {')
	assert c_source.contains('; ready(); i++) {')

	alias_c_source := generate('module main

type BOOL = bool

fn enabled() BOOL {
	return true
}

fn main() {
	flag := BOOL(true)
	if flag {
		println(1)
	}
	for enabled() {
		break
	}
	for i := 0; BOOL(i < 1); i++ {
		break
	}
}
', 'boolean_alias_conditions.v', prefs) or { panic(err) }
	assert alias_c_source.contains('if (flag) {'), alias_c_source
	assert alias_c_source.contains('while (enabled()) {'), alias_c_source
	assert alias_c_source.contains('; ((BOOL)(i<1)); i++) {'), alias_c_source
}

fn test_comparison_and_logical_operand_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source, expected in {
		'module main\nfn main() { println(1 == true) }\n':          'comparison `==` operands of incompatible types'
		'module main\nfn main() { println(true < false) }\n':       'comparison `<` operands of incompatible types'
		'module main\nfn main() { if true && 1 { println(1) } }\n': 'logical `&&` operands of types'
		'module main\nfn main() { if !1 { println(1) } }\n':        'logical `!` operand of type'
	} {
		mut message := ''
		_ := generate(source, 'invalid_boolean_operands.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', '${expected}: ${message}'
	}

	c_source := generate("module main

type Label = string

fn same(left string, right string) bool {
	return left == right
}

fn same_label(first Label, second Label) bool {
	return first == second
}

fn main() {
	left := 1
	right := 2
	ok := left < right && true
	println(ok)
	println(left < right)
	println(same('same', 'same'))
	println('alpha' < 'beta')
	println('beta' > 'alpha')
	println('alpha' <= 'alpha')
	println('beta' >= 'beta')
	println('alpha' != 'beta')
	println(same_label(Label('same'), Label('same')))
	println(Label('alpha') < Label('beta'))
}
", 'valid_boolean_operands.v', prefs) or { panic(err) }
	assert c_source.contains('left<right'), c_source
	assert c_source.contains('&&'), c_source
	assert c_source.contains('v_fastc_println_bool'), c_source
	assert c_source.contains('static bool builtin__string_eq'), c_source
	assert c_source.contains('builtin__string_eq(left,right)'), c_source
	assert c_source.contains('builtin__string_eq(first,second)'), c_source
	assert c_source.contains('builtin__string_lt("alpha","beta")'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_boolean_operands_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue'
}

fn test_selfhost_unary_not_does_not_capture_following_logical_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

fn rejects(selfhost bool, name string) bool {
	return !selfhost && (name in ['charptr', 'rune'])
}

fn main() {
	println(rejects(true, 'int'))
}
", 'logical_unary_not_precedence.v', prefs) or { panic(err) }
	assert c_source.contains('((!(selfhost))&&('), c_source
	assert !c_source.contains('!(((selfhost)&&('), c_source
}

fn test_struct_equality_is_lowered_field_by_field() {
	prefs := pref.new_preferences()
	c_source := generate('module main

struct Inner {
	value int
}

struct Pair {
	inner Inner
	label string
}

fn equal(left Pair, right Pair) bool {
	return left == right
}

fn different(left Pair, right Pair) bool {
	return left != right
}

fn parenthesized(left Pair, right Pair) bool {
	return !(left == right)
}
', 'struct_equality.v', prefs) or { panic(err) }
	assert c_source.contains('Pair __v_fastc_eq_left = (left);'), c_source
	assert c_source.contains('Pair __v_fastc_eq_right = (right);'), c_source
	assert c_source.contains('((__v_fastc_eq_left).inner).value'), c_source
	assert c_source.contains('builtin__string_eq((__v_fastc_eq_left).label, (__v_fastc_eq_right).label)'), c_source
	assert c_source.contains('!('), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_struct_equality_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
}

fn test_struct_equality_compares_dynamic_array_fields() {
	prefs := pref.new_preferences()
	c_source := generate('module main

struct Values {
	items []int
}

fn equal(left Values, right Values) bool {
	return left == right
}
', 'struct_array_equality.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_array_eq_left.len == __v_fastc_array_eq_right.len'), c_source
	assert c_source.contains('((int *)__v_fastc_array_eq_left.data)'), c_source
}

fn test_literal_membership_evaluates_subject_once() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for operator in ['in', '!in'] {
		c_source := generate('module main\n\nfn next() int {\n\treturn 1\n}\n\nfn main() {\n\tif next() ${operator} [0, 2] {}\n}\n', 'membership_subject_once.v', prefs) or { panic(err) }
		assert c_source.contains('__v_fastc_membership_item = (next());'), c_source
		assert c_source.count('next()') == 1, c_source
	}
}

fn test_array_membership_evaluates_candidate_before_collection() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for operator in ['in', '!in'] {
		c_source := generate('module main\n\nfn candidate() int {\n\treturn 1\n}\n\nfn collection() []int {\n\treturn [1, 2]\n}\n\nfn main() {\n\tif candidate() ${operator} collection() {}\n}\n', 'array_membership_order.v', prefs) or { panic(err) }
		candidate_index := c_source.index('__v_fastc_membership_item = (candidate());') or { -1 }
		collection_index := c_source.index('__v_fastc_membership_collection = (collection());') or {
			-1
		}
		assert candidate_index >= 0, c_source
		assert collection_index > candidate_index, c_source
		assert c_source.count('candidate()') == 1, c_source
		assert c_source.count('__v_fastc_membership_collection = (collection());') == 1, c_source
	}
}

fn fastc_test_expression_token(tok token.Token, lit string) FastcExpressionToken {
	return FastcExpressionToken{
		tok: tok
		lit: lit
	}
}

fn test_literal_membership_materializes_candidates_before_comparison() {
	prefs := pref.new_preferences()
	g := Parser{
		prefs: prefs
		selfhost: true
		s: scanner.new_scanner(prefs, .normal)
		locals: {
			'subject': FastcLocal{
				typ: 'int'
			}
		}
	}
	mut tokens := [
		fastc_test_expression_token(.name, 'subject'),
		fastc_test_expression_token(.key_in, 'in'),
		fastc_test_expression_token(.lsbr, '['),
		fastc_test_expression_token(.name, 'candidate'),
		fastc_test_expression_token(.lpar, '('),
		fastc_test_expression_token(.number, '1'),
		fastc_test_expression_token(.rpar, ')'),
		fastc_test_expression_token(.comma, ','),
		fastc_test_expression_token(.name, 'candidate'),
		fastc_test_expression_token(.lpar, '('),
		fastc_test_expression_token(.number, '2'),
		fastc_test_expression_token(.rpar, ')'),
		fastc_test_expression_token(.rsbr, ']'),
	]
	for operator in [
		fastc_test_expression_token(.key_in, 'in'),
		fastc_test_expression_token(.not_in, '!in'),
	] {
		tokens[1] = operator
		rendered := g.render_special_expression(tokens, '') or {
			panic('membership was not rendered')
		}
		first_assignment := '__v_fastc_membership_candidate_0 = (candidate(1));'
		second_assignment := '__v_fastc_membership_candidate_1 = (candidate(2));'
		comparison := '__v_fastc_membership_subject) == (__v_fastc_membership_candidate_0)'
		first_index := rendered.source.index(first_assignment) or { -1 }
		second_index := rendered.source.index(second_assignment) or { -1 }
		comparison_index := rendered.source.index(comparison) or { -1 }
		assert first_index >= 0, rendered.source
		assert second_index > first_index, rendered.source
		assert comparison_index > second_index, rendered.source
		assert rendered.source.count('candidate(1)') == 1, rendered.source
		assert rendered.source.count('candidate(2)') == 1, rendered.source
	}
}

fn test_membership_temporaries_do_not_collide_with_user_names() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn main() {
	__v_fastc_membership_candidate_0 := 1
	if 1 in [__v_fastc_membership_candidate_0] {}
}
', 'membership_temporary_collision.v', prefs) or { panic(err) }
	assert c_source.contains('int __v_fastc_membership_1_item = (1);'), c_source
	assert c_source.contains('__v_fastc_membership_1_collection'), c_source
	assert !c_source.contains('int __v_fastc_membership_item = (1);'), c_source
}

fn test_selfhost_string_membership_uses_substring_semantics() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

fn substring() string {
	return 'ell'
}

fn value() string {
	return 'hello'
}

fn main() {
	if substring() in value() {}
	if 'xyz' !in 'hello' {}
}
", 'string_membership.v', prefs) or { panic(err) }
	substring_assignment := 'string __v_fastc_membership_substring = (substring());'
	value_assignment := 'string __v_fastc_membership_value = (value());'
	assert c_source.contains('static bool v_fastc_string_contains(string value, string substring)'), c_source
	assert c_source.contains(substring_assignment), c_source
	assert c_source.contains(value_assignment), c_source
	substring_index := c_source.index(substring_assignment) or { -1 }
	value_index := c_source.index(value_assignment) or { -1 }
	assert substring_index < value_index, c_source
	assert c_source.contains('v_fastc_string_contains(__v_fastc_membership_value, __v_fastc_membership_substring)'), c_source
	assert c_source.contains('!(v_fastc_string_contains(__v_fastc_membership_value, __v_fastc_membership_substring))'), c_source
	assert c_source.count('substring()') == 1, c_source
	assert c_source.count('value()') == 1, c_source
	assert !c_source.contains('u8 __v_fastc_membership_item'), c_source
}

fn test_string_alias_membership_uses_string_equality() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

type Str = string

fn main() {
	value := Str('same')
	candidate := Str('same')
	if value in [candidate] {}
	values := [candidate]
	if value in values {}
}
", 'string_alias_membership.v', prefs) or { panic(err) }
	assert c_source.count('builtin__string_eq(__v_fastc_membership_item, ((Str *)__v_fastc_membership_collection.data)') == 2, c_source
}

fn test_mixed_integer_comparisons_preserve_signed_semantics() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(i64(-1) < u64(1))
	println(u64(1) > i64(-1))
	println(i64(-27) < u32(65463356))
	println(u32(8543) > int(-7523))
	println(i64(-89) <= u64(567))
	println(int(-1) != u32(0) - u32(1))
	println(i64(-1) < u64(1) && u64(2) >= i64(2))
	println(!((u64(1) < i64(-1))))
}
', 'mixed_integer_comparisons.v', prefs) or { panic(err) }
	assert c_source.contains('v_fastc_us_gt('), c_source
	assert c_source.contains('v_fastc_us_ne('), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_mixed_integer_comparisons_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue'
}

fn test_match_branch_value_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { x := 1; match x { true { println(1) } else {} } }\n',
		'module main\nfn main() { x := true; match x { 1 { println(1) } else {} } }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_match_branch_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', message
	}

	c_source := generate('module main

fn main() {
	x := 1
	match x {
		0, 1 { println(1) }
		else {}
	}
}
', 'valid_match_branch_types.v', prefs) or { panic(err) }
	assert c_source.contains('if (((__v_fastc_match_'), c_source
	assert c_source.contains('== (0)) || '), c_source
	assert c_source.contains('== (1))'), c_source
}

fn test_selfhost_match_accepts_keyword_enum_members() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Mode {
	none
	manual
}

fn use(mode Mode) int {
	return match mode {
		.manual { 1 }
		.none { 0 }
	}
}

fn main() {
	_ := use(.manual)
}
', 'selfhost_match_keyword_enum.v', prefs) or { panic(err) }
	assert c_source.contains('Mode__manual'), c_source
}

fn test_duplicate_match_cases_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		"module main\nfn main() { n := 1; match n { 1 { println('a') } 1 { println('b') } else {} } }\n",
		'module main\nfn main() { n := 1; value := match n { 1 { 2 } 1 { 3 } else { 4 } }; println(value) }\n',
		'module main\nfn main() { n := 10; match n { 10 {} 0xa {} else {} } }\n',
	] {
		mut message := ''
		_ := generate(source, 'duplicate_match_case.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('duplicate match case'), message
	}
}

fn test_primitive_cast_operand_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { println(bool(2)) }\n',
		"module main\nfn main() { println(int('2')) }\n",
		'module main\nfn main() { println(string(2)) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_primitive_cast.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', message
	}

	c_source := generate('module main

fn main() {
	println(bool(true))
	println(int(true))
	unsafe {
		println(bool(2))
	}
	println(unsafe { bool(0) })
}
', 'valid_primitive_casts.v', prefs) or { panic(err) }
	assert c_source.contains('println(((bool)(2)));'), c_source
	assert c_source.contains('println(((bool)(0)));'), c_source
}

fn test_declared_cast_operand_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source, expected in {
		'module main\ntype MyType = string\nfn main() { println(MyType(5)) }\n':       'alias to `string`'
		"module main\nenum Color { red blue }\nfn main() { println(Color('red')) }\n": 'to enum `Color`'
		'module main\nenum Color { red blue }\nfn main() { println(Color(1)) }\n':     'outside an `unsafe` block'
	} {
		mut message := ''
		_ := generate(source, 'invalid_declared_cast.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', '${expected}: ${message}'
	}

	c_source := generate("module main

type Label = string
type Count = int

enum Color {
	red
	blue
}

fn main() {
	label := Label('ok')
	count := Count(2)
	color := unsafe { Color(1) }
	println(label == Label('ok'))
	println(int(count))
	println(color)
}
", 'valid_declared_casts.v', prefs) or { panic(err) }
	assert c_source.contains('((Label)("ok"))'), c_source
	assert c_source.contains('((Count)(2))'), c_source
	assert c_source.contains('((Color)(1))'), c_source
}

fn test_defer_is_emitted_when_its_lexical_scope_exits() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	if true {
		defer { println(1) }
		println(2)
	}
	println(3)
}
', 'scoped_defer.v', prefs) or { panic(err) }
	print_two := c_source.index('println(2);') or { panic(c_source) }
	deferred_one := c_source.index_after('println(1);', print_two) or { panic(c_source) }
	print_three := c_source.index_after('println(3);', deferred_one) or { panic(c_source) }
	assert print_two < deferred_one
	assert deferred_one < print_three
}

fn test_block_locals_are_released_when_their_scope_exits() {
	prefs := pref.new_preferences()
	c_source := generate("module main

fn main() {
	if true {
		value := 1
		println(value)
		if true {
			nested := 2
			println(nested)
		}
		nested := 'inner'
		println(nested)
	}
	value := 'outer'
	println(value)
}
", 'block_local_scope.v', prefs) or { panic(err) }
	assert c_source.contains('__typeof__((1)) value = (1);'), c_source
	assert c_source.contains('string value = ("outer");'), c_source
}

fn test_return_expression_is_evaluated_before_deferred_blocks() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn value() int {
	mut x := 1
	defer { x = 2 }
	return x
}

fn main() {
	println(value())
}
', 'return_before_defer.v', prefs) or { panic(err) }
	evaluation := c_source.index('__typeof__((x)) __v_fastc_return_') or { panic(c_source) }
	deferred_assignment := c_source.index_after('x=2;', evaluation) or { panic(c_source) }
	returned_temporary := c_source.index_after('return __v_fastc_return_', deferred_assignment) or {
		panic(c_source)
	}
	assert evaluation < deferred_assignment
	assert deferred_assignment < returned_temporary
}

fn test_control_flow_is_rejected_inside_deferred_blocks() {
	prefs := pref.new_preferences()
	for source, expected in {
		'module main\nfn value() int { defer { return 2 } return 1 }\n':     '`return` not allowed inside a `defer` block'
		'module main\nfn main() { for { defer { break } break } }\n':        '`break` is not allowed in defer statements'
		'module main\nfn main() { for { defer { continue } break } }\n':     '`continue` is not allowed in defer statements'
		'module main\nfn main() { defer { goto done } done: println(1) }\n': 'goto is not allowed in defer statements'
		'module main\nfn main() { defer { defer { println(1) } } }\n':       '`defer` blocks cannot be nested'
	} {
		mut message := ''
		_ := generate(source, 'invalid_defer_control_flow.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains(expected), message
	}
}

fn test_mutable_function_parameters_require_mutable_arguments() {
	prefs := pref.new_preferences()
	mut pointer_message := ''
	_ := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	x := 1
	change(&x)
}
', 'immutable_pointer_argument.v', prefs) or {
		pointer_message = err.msg()
		''
	}
	assert pointer_message.contains('requires a mutable argument written with `mut`'), pointer_message

	mut immutable_message := ''
	_ := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	x := 1
	change(mut x)
}
', 'immutable_mut_argument.v', prefs) or {
		immutable_message = err.msg()
		''
	}
	assert immutable_message.contains('mutable argument `x` to function `change` is immutable'), immutable_message

	c_source := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	mut x := 1
	change(mut x)
	println(x)
}
', 'mutable_argument.v', prefs) or { panic(err) }
	assert c_source.contains('void change(int* x)'), c_source
	assert c_source.contains('change(&x);'), c_source
}

fn test_globals_can_be_passed_to_mutable_parameters() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global state = 1

fn update(mut value int) {
	value = 2
}

fn main() {
	update(mut state)
	println(state)
}
', 'mutable_global_argument.v', prefs) or { panic(err) }
	assert c_source.contains('void update(int* value)'), c_source
	assert c_source.contains('update(&state);'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_mut_global_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '2\n'
}

fn test_mutable_arguments_require_mutable_imported_fields() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_mut_argument_field_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'settings')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'settings', 'settings.v')
	module_source := 'module settings

pub struct Config {
pub:
	read_only int
pub mut:
	writable int
}
'
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	os.write_file(module_file, module_source) or { panic(err) }
	os.write_file(main_file, 'module main

import settings

fn change(mut value int) {
	value = 2
}

fn mutate(mut config settings.Config) {
	change(mut config.read_only)
}

fn main() {}
') or {
		panic(err)
	}
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutable argument field `Config.read_only`'), message
	assert message.contains('is not `pub mut` in imported module `settings`'), message

	os.write_file(main_file, 'module main

import settings

fn change(mut value int) {
	value = 2
}

fn mutate(mut config settings.Config) {
	change(mut config.writable)
}

fn main() {}
') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('change(&config.writable);'), c_source
}

fn test_match_expression_requires_else() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	x := 2
	y := match x { 1 { 7 } }
	println(y)
}
', 'non_exhaustive_match_expression.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('non-exhaustive match expression without `else`'), message

	c_source := generate('module main

fn main() {
	x := 2
	y := match x { 1 { 7 } else { 9 } }
	println(y)
}
', 'exhaustive_match_expression.v', prefs) or { panic(err) }
	assert c_source.contains('? (7) : (9)')
}

fn test_match_statement_without_else_does_not_terminate_function() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn value(x int) int {
	match x {
		1 { return 7 }
	}
}

fn main() {
	println(value(1))
}
', 'non_exhaustive_match_statement.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('non-void function `value` that can fall through'), message

	c_source := generate('module main

fn value(x int) int {
	match x {
		1 { return 7 }
		else { return 9 }
	}
}

fn main() {
	println(value(1))
}
', 'exhaustive_match_statement.v', prefs) or { panic(err) }
	assert c_source.contains('else {\n\t\treturn 9;'), c_source
}

fn test_c_reserved_identifiers_are_escaped_consistently() {
	prefs := pref.new_preferences()
	c_source := generate('module main

struct Holder {
	auto int
	v_auto int
}

fn calculate(holder Holder, register int, v_register int) int {
	restrict := register
	v_restrict := v_register
	return holder.auto + holder.v_auto + restrict + v_restrict
}

fn auto() int {
	return 42
}

fn v_auto() int {
	return 24
}

fn main() {
	result := auto()
	v_result := v_auto()
	auto := result
	v_auto := v_result
	println(auto + v_auto)
}
', 'reserved_identifiers.v', prefs) or { panic(err) }
	assert c_source.contains('int __v_fastc_keyword_auto;'), c_source
	assert c_source.contains('int v_auto;'), c_source
	assert c_source.contains('int calculate(Holder holder, int __v_fastc_keyword_register, int v_register)'), c_source
	assert c_source.contains('__typeof__((__v_fastc_keyword_register)) __v_fastc_keyword_restrict = (__v_fastc_keyword_register);'), c_source
	assert c_source.contains('__typeof__((v_register)) v_restrict = (v_register);'), c_source
	assert c_source.contains('return holder.__v_fastc_keyword_auto+holder.v_auto+__v_fastc_keyword_restrict+v_restrict;'), c_source
	assert c_source.contains('int __v_fastc_function_auto(void)'), c_source
	assert c_source.contains('int v_auto(void)'), c_source
	assert c_source.contains(' __v_fastc_keyword_auto = (result);'), c_source
	assert c_source.contains(' v_auto = (v_result);'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_reserved_names_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '66\n'
}

fn test_main_module_libc_function_collisions_are_mangled() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn strlen() int {
	return 42
}

fn printf() int {
	return strlen()
}

fn open() int {
	return printf()
}

fn close() int {
	return open()
}

fn main() {
	println(close())
}
', 'libc_function_collisions.v', prefs) or { panic(err) }
	for name in ['strlen', 'printf', 'open', 'close'] {
		assert c_source.contains('int __v_fastc_function_${name}(void)'), c_source
	}
	assert c_source.contains('return __v_fastc_function_strlen();'), c_source
	assert c_source.contains('return __v_fastc_function_printf();'), c_source
	assert c_source.contains('return __v_fastc_function_open();'), c_source
	assert c_source.contains('println(__v_fastc_function_close());'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_libc_names_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '42\n'
}

fn test_fastc_runtime_function_collisions_are_mangled() {
	prefs := pref.new_preferences()
	c_source := generate("module main

fn v_fastc_bool_str(value bool) string {
	if value {
		return 'user function'
	}
	return 'unexpected'
}

fn main() {
	println(v_fastc_bool_str(true))
}
", 'fastc_runtime_function_collision.v', prefs) or { panic(err) }
	assert c_source.contains('static string v_fastc_bool_str(bool value)'), c_source
	assert c_source.contains('string __v_fastc_function_v_fastc_bool_str(bool value)'), c_source
	assert c_source.contains('println(__v_fastc_function_v_fastc_bool_str(((bool)true)))'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_runtime_names_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'user function\n'
}

fn test_return_expression_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn value() bool { return 2 }\nfn main() { println(value()) }\n',
		'module main\nfn value() int { return true }\nfn main() { println(value()) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_return_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', message
	}

	c_source := generate('module main

fn enabled() bool {
	return true
}

fn value() int {
	return 2
}

fn main() {
	println(enabled())
	println(value())
}
', 'valid_return_types.v', prefs) or { panic(err) }
	assert c_source.contains('return ((bool)true);')
	assert c_source.contains('return 2;')
}

fn test_assignment_value_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { mut enabled := false; enabled = 2; println(enabled) }\n',
		'module main\nfn main() { mut count := 1; count = true; println(count) }\n',
		'module main\nfn main() { mut enabled := false; enabled += 1; println(enabled) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_assignment_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', message
	}

	c_source := generate('module main

fn ready() bool {
	return true
}

fn main() {
	mut enabled := false
	enabled = ready()
	mut count := 1
	count = 2
	count += 3
	println(enabled)
	println(count)
}
', 'valid_assignment_types.v', prefs) or { panic(err) }
	assert c_source.contains('enabled=ready();')
	assert c_source.contains('count=2;')
	assert c_source.contains('count+=3;')
}

fn test_parallel_assignment_targets_are_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for source, expected in {
		'module main\nfn main() { a := 1; b := 2; a, b = b, a }\n':                                                                             'parallel assignment to immutable name `a`'
		'module main\nfn main() { mut enabled := false; mut count := 1; enabled, count = count, enabled }\n':                                   'parallel assignment of type `int` to `enabled` of type `bool`'
		'module main\nfn main() { mut value := 1; value, missing = 2, 3 }\n':                                                                   'parallel assignment to unknown name `missing`'
		'module main\nfn pair() (int, bool) { return 1, true }\nfn main() { mut enabled := false; mut count := 1; enabled, count = pair() }\n': 'parallel assignment of type `int` to `enabled` of type `bool`'
	} {
		mut message := ''
		_ := generate(source, 'invalid_parallel_assignment.v', prefs) or {
			message = err.msg()
			''
		}
		if expected.contains('immutable') || expected.contains('unknown') {
			assert message.contains(expected), message
		} else {
			assert message == '', message
		}
	}

	c_source := generate('module main

fn pair() (int, int) {
	return 3, 4
}

fn main() {
	mut first := 1
	mut second := 2
	first, second = second, first
	first, second = pair()
	println(first)
	println(second)
}
', 'valid_parallel_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('first = __v_fastc_parallel_'), c_source
	assert c_source.contains('second = __v_fastc_parallel_'), c_source
	assert c_source.contains('memcpy(&first, V_FASTC_MULTI_SOURCE(__v_fastc_multi_return_'), c_source
	assert c_source.contains('memcpy(&second, V_FASTC_MULTI_SOURCE(__v_fastc_multi_return_'), c_source
}

fn test_parallel_member_assignment_uses_pointer_backed_multi_return_storage() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Large {
	bytes [40]u8
}

struct Holder {
mut:
	value Large
}

fn pair() (int, Large) {
	return 1, Large{}
}

fn assign(mut holder Holder) {
	_, holder.value = pair()
}

fn main() {
	mut holder := Holder{}
	assign(mut holder)
}
', 'parallel_member_large_multi_return.v', prefs) or { panic(err) }
	assert c_source.contains('memcpy(&holder->value, V_FASTC_MULTI_SOURCE(__v_fastc_multi_return_'), c_source
	assert !c_source.contains('.values[1].data, sizeof(holder->value)'), c_source
}

fn test_selfhost_parallel_assignment_accepts_aggregate_targets() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Holder {
mut:
	values [2]u8
}

fn swap(mut holder Holder) {
	holder.values[0], holder.values[1] = holder.values[1], holder.values[0]
}

fn main() {
	mut holder := Holder{}
	swap(mut holder)
}
', 'parallel_aggregate_assignment.v', prefs) or { panic(err) }
	assert c_source.count('__v_fastc_parallel_') >= 4, c_source
}

fn test_selfhost_parallel_declaration_from_if_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn choose(flag bool) int {
	mut a, mut b := if flag {
		1, 2
	} else {
		3, 4
	}
	a++
	b++
	return a + b
}

fn main() {
	_ := choose(true)
}
', 'selfhost_parallel_if_expression.v', prefs) or { panic(err) }
	assert c_source.contains('MultiReturn __v_fastc_multi_return_'), c_source
	assert c_source.contains('int a = (int){0};'), c_source
	assert c_source.contains('int b = (int){0};'), c_source
}

fn test_selfhost_parallel_option_multireturn_from_nested_receiver() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Data {
	value int
}

struct Worker {}

fn (w Worker) pair() !(int, Data) {
	return 1, Data{ value: 2 }
}

struct Holder {
	worker Worker
}

fn use(holder Holder) int {
	first, data := holder.worker.pair()!
	return first + data.value
}

fn main() {
	_ := use(Holder{})
}
', 'selfhost_nested_receiver_option_multireturn.v', prefs) or { panic(err) }
	assert c_source.contains('Data data = (Data){0};'), c_source
	assert c_source.contains('memcpy(&data, __v_fastc_multi_return_'), c_source
}

fn test_selfhost_option_multireturn_guard_from_nested_mut_receiver() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Worker {}

fn (mut w Worker) pair(flag bool, value int) ?(u64, int) {
	return if flag { u64(1) } else { u64(0) }, value
}

struct Holder {
mut:
	worker &Worker
}

fn use(mut holder Holder) u64 {
	if first, _ := holder.worker.pair(true,
		2)
	{
		return first
	}
	return 0
}

fn main() {
	mut worker := Worker{}
	mut holder := Holder{ worker: &worker }
	_ := use(mut holder)
}
', 'selfhost_nested_mut_receiver_option_multireturn_guard.v', prefs) or { panic(err) }
	assert c_source.contains('u64 first = (u64){0};'), c_source
	assert c_source.contains('memcpy(&first, __v_fastc_multi_return_'), c_source
	assert !c_source.contains(';);'), c_source
}

fn test_selfhost_array_field_index_assignment_keeps_the_field_name() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct File {
mut:
	source_digest []u8
}

fn copy_byte(mut file File, digest []u8, index int) {
	file.source_digest[index] = digest[index]
}

fn main() {
	mut file := File{}
	copy_byte(mut file, []u8{}, 0)
}
', 'array_field_index_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__array_get(file->source_digest, index)'), c_source
	assert !c_source.contains('file->source_('), c_source
}

fn test_selfhost_qualified_constant_fixed_array_length_is_preserved() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	sizes_source := 'module sizes\npub const digest_size = 32\n'
	main_source := 'module main
import sizes

struct File {
mut:
	source_digest [sizes.digest_size]u8
}

fn copy_byte(mut file File, digest []u8, index int) {
	file.source_digest[index] = digest[index]
}

fn main() {
	mut file := File{}
	copy_byte(mut file, []u8{}, 0)
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'sizes/sizes.v'
			source: sizes_source
			header: fastc_scan_source_header(sizes_source, 'sizes/sizes.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('u8 source_digest[sizes__digest_size];'), c_source
	assert c_source.contains('(file->source_digest)[builtin__v_fixed_index(index, sizes__digest_size)]'), c_source
}

fn test_aggregate_lvalue_mutability_is_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	_ := generate('module main

struct Holder {
mut:
	value int
}

fn main() {
	holder := Holder{}
	holder.value = 2
}
', 'immutable_aggregate_root.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutation of immutable or unknown name `holder`'), message

	c_source := generate('module main

struct Holder {
mut:
	value int
}

fn main() {
	mut holder := Holder{}
	holder.value = 2
}
', 'mutable_aggregate_root.v', prefs) or { panic(err) }
	assert c_source.contains('holder.value=2;'), c_source
}

fn test_c_style_loop_initializer_type_is_not_validated() {
	prefs := pref.new_preferences()
	invalid_c_source := generate('module main

fn main() {
	mut enabled := false
	for enabled = 2; enabled; enabled = false {
		println(1)
	}
}
', 'invalid_loop_initializer.v', prefs) or { panic(err) }
	assert invalid_c_source.contains('for (enabled = (2); enabled; enabled=((bool)false)) {'), invalid_c_source

	c_source := generate('module main

fn main() {
	mut enabled := false
	for enabled = true; enabled; enabled = false {}
}
', 'valid_loop_initializer.v', prefs) or { panic(err) }
	assert c_source.contains('for (enabled = (((bool)true)); enabled; enabled=((bool)false)) {'), c_source

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	empty_initializer_source := generate('module main

fn main() {
	mut i := 0
	for ; i < 2; i++ {}
}
', 'empty_loop_initializer.v', selfhost_prefs) or { panic(err) }
	assert empty_initializer_source.contains('for (; i<2; i++) {'), empty_initializer_source
}

fn test_negative_integer_literals_are_lowered_for_unsigned_targets() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn take(x u32) { println(x) }\nfn main() { take(-1) }\n',
		'module main\nfn main() { mut value := u32(0); value = -1; println(value) }\n',
		'module main\nfn value() u32 { return -1 }\nfn main() { println(value()) }\n',
	] {
		mut message := ''
		_ := generate(source, 'negative_unsigned_literal.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', message
	}

	c_source := generate('module main

fn take(x u32) {
	println(x)
}

fn value() u32 {
	return 1
}

fn take_signed(x int) {
	println(x)
}

fn signed_value() int {
	return -1
}

fn main() {
	mut number := u32(0)
	number = 1
	take(1)
	println(value())
	mut signed := 0
	signed = -1
	take_signed(-1)
	println(signed_value())
}
', 'positive_unsigned_literals.v', prefs) or { panic(err) }
	assert c_source.contains('number=1;')
	assert c_source.contains('take(1);')
	assert c_source.contains('return 1;')
	assert c_source.contains('signed=-1;')
	assert c_source.contains('take_signed(-1);')
	assert c_source.contains('return -1;')
}

fn test_main_return_type_is_not_validated() {
	prefs := pref.new_preferences()
	c_source := generate('module main\nfn main() int { return 7 }\n', 'value_returning_main.v', prefs) or { panic(err) }
	assert c_source.contains('int main(void)'), c_source
	assert c_source.contains('return 7;'), c_source
}

fn test_main_must_not_have_parameters() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main(code int) {}\n',
		'module main\nfn main(code int) int { return code }\n',
	] {
		mut message := ''
		_ := generate(source, 'parameterized_main.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('main function with parameters'), message
	}
}

fn test_mutable_iteration_requires_a_mutable_collection() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	_ := generate('module main

fn change(mut value int) {
	value = 3
}

fn main() {
	items := [1, 2]
	for mut item in items {
		change(mut item)
	}
}
', 'immutable_mutable_iteration.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutable iteration over immutable collection `items`'), message

	c_source := generate('module main

fn change(mut value int) {
	value = 3
}

fn main() {
	mut items := [1, 2]
	for mut item in items {
		println(item)
		item = 3
		change(mut item)
	}
}
', 'mutable_iteration.v', prefs) or { panic(err) }
	assert c_source.contains('int *item = &(((int *)'), c_source
	assert c_source.contains('println((*(item)));'), c_source
	assert c_source.contains('(*item)=3;'), c_source
	assert c_source.contains('change(item);'), c_source
}

fn test_map_pointer_iteration_passes_the_map_pointer_directly() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn iterate(source map[string]int) {
	for key, value in source {
		println(key)
		println(value)
	}
	pointer := &source
	for key, value in pointer {
		println(key)
		println(value)
	}
}

fn main() {
	iterate(map[string]int{})
}
', 'map_pointer_iteration.v', prefs) or { panic(err) }
	assert c_source.count('builtin__map_keys((map *)&__v_fastc_map_collection_') == 1, c_source
	assert c_source.count('builtin__map_values((map *)&__v_fastc_map_collection_') == 1, c_source
	assert c_source.count('builtin__map_keys((map *)__v_fastc_map_collection_') == 1, c_source
	assert c_source.count('builtin__map_values((map *)__v_fastc_map_collection_') == 1, c_source
}

fn test_map_pointer_sized_callbacks_follow_target_width() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', 'x86') or { panic(err) }
	source := 'module main

fn main() {
	values := map[usize]int{}
	pointers := map[voidptr]int{}
	println(values.len)
	println(pointers.len)
}
'
	c32 := generate(source, 'map_usize_32.v', prefs) or { panic(err) }
	for key_type in ['usize', 'voidptr'] {
		assert c32.contains('sizeof(${key_type}), sizeof(int), &builtin__map_hash_int_4, &builtin__map_eq_int_4, &builtin__map_clone_int_4'), c32
	}

	prefs.target = pref.target_from('linux', 'arm64') or { panic(err) }
	c64 := generate(source, 'map_usize_64.v', prefs) or { panic(err) }
	for key_type in ['usize', 'voidptr'] {
		assert c64.contains('sizeof(${key_type}), sizeof(int), &builtin__map_hash_int_8, &builtin__map_eq_int_8, &builtin__map_clone_int_8'), c64
	}

	hash_fn, eq_fn, clone_fn, _ := fastc_map_runtime_functions('int*', 32)
	assert hash_fn == 'builtin__map_hash_int_4'
	assert eq_fn == 'builtin__map_eq_int_4'
	assert clone_fn == 'builtin__map_clone_int_4'
	i64_hash_fn, _, _, _ := fastc_map_runtime_functions('i64', 32)
	assert i64_hash_fn == 'builtin__map_hash_int_8'
}

fn test_map_alias_key_callbacks_use_underlying_types() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', 'x86') or { panic(err) }
	c_source := generate("module main

type WideKey = u64
type TextKey = string

enum Kind {
	first
}

type KindKey = Kind

@[flag]
enum Permissions {
	read
}

type PermissionsKey = Permissions

fn main() {
	wide := {WideKey(1): 1}
	text := {TextKey('one'): 1}
	kinds := {KindKey(Kind.first): 1}
	permissions := {PermissionsKey(Permissions.read): 1}
	println(wide.len)
	println(text.len)
	println(kinds.len)
	println(permissions.len)
}
", 'map_alias_callbacks.v', prefs) or { panic(err) }
	assert c_source.contains('sizeof(WideKey), sizeof(int), &builtin__map_hash_int_8, &builtin__map_eq_int_8, &builtin__map_clone_int_8'), c_source

	assert c_source.contains('sizeof(TextKey), sizeof(int), &builtin__map_hash_string, &builtin__map_eq_string, &builtin__map_clone_string'), c_source

	assert c_source.contains('sizeof(KindKey), sizeof(int), &builtin__map_hash_int_4, &builtin__map_eq_int_4, &builtin__map_clone_int_4'), c_source

	assert c_source.contains('sizeof(PermissionsKey), sizeof(int), &builtin__map_hash_int_8, &builtin__map_eq_int_8, &builtin__map_clone_int_8'), c_source
}

fn test_array_pointer_iteration_preserves_element_references() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn take(value &int) {}

fn main() {
	values := [1, 2]
	for value in &values {
		take(value)
	}
	pointer := &values
	for value in pointer {
		take(value)
	}
}
', 'array_pointer_iteration.v', prefs) or { panic(err) }
	assert c_source.count('int *value = &(((int *)__v_fastc_collection_') == 2, c_source
	assert c_source.count('take(value);') == 2, c_source
}

fn test_selfhost_array_and_string_indexing_uses_bounds_checked_helpers() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn checked(mut values []int, text string, nested [][]int, index int) int {
	values[index] = values[index]
	println(text[index])
	return nested[index][index]
}

fn main() {
	mut values := []int{}
	checked(mut values, "x", [][]int{}, 0)
}
', 'checked_indexing.v', prefs) or { panic(err) }
	assert c_source.count('builtin__array_get(*(values), index)') == 2, c_source
	assert c_source.contains('builtin__string_at(text, index)'), c_source
	assert c_source.contains('builtin__array_get(nested, index)'), c_source
	assert c_source.contains('builtin__array_get((*(Array_int *)builtin__array_get(nested, index)), index)'), c_source

	assert !c_source.contains('values.data)[index]'), c_source
	assert !c_source.contains('text.str[index]'), c_source
}

fn test_selfhost_double_pointer_index_preserves_one_pointer_level() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn take(value &u8) {}

fn pass(values &&u8, index int) {
	take(values[index])
}

fn main() {
	pass(&&u8(0), 0)
}
', 'selfhost_double_pointer_index.v', prefs) or { panic(err) }
	assert c_source.contains('take(((values)[index]))'), c_source
	assert !c_source.contains('take(&(((values)[index])))'), c_source
}

fn test_selfhost_selector_assignment_accepts_array_initializer() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Holder {
mut:
	values []u32
}

fn reset(mut holder Holder) {
	holder.values = []u32{len: 8}
}

fn main() {
	mut holder := Holder{}
	reset(mut holder)
}
', 'selector_array_initializer.v', prefs) or { panic(err) }
	assert c_source.contains('holder->values='), c_source
	assert c_source.contains('builtin____new_array('), c_source
}

fn test_selfhost_multiline_sum_type_declaration_is_skipped_completely() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Any = int
	| bool
	| string
	| []Any

fn main() {}
', 'multiline_sum_type.v', prefs) or { panic(err) }
	assert c_source.contains('typedef struct { void *_object; u32 _typ; void *_methods; } Any;'), c_source
}

fn test_selfhost_composite_ordering_moves_one_line_interfaces_before_fields() {
	source := 'struct Holder {\n\tWriter wr;\n};\n\nstruct Writer { void *_object; };\n'
	ordered := fastc_order_c_composite_definitions(source, {
		'Holder': {
			'wr': 'Writer'
		}
	}, {
		fastc_c_declared_type_name(fastc_type_key('main', 'Holder')): true
		fastc_c_declared_type_name(fastc_type_key('main', 'Writer')): true
	}, map[string]string{})
	writer_index := ordered.index('struct Writer {') or { -1 }
	holder_index := ordered.index('struct Holder {') or { -1 }
	assert writer_index >= 0 && writer_index < holder_index, ordered
}

fn test_selfhost_composite_ordering_resolves_by_value_aliases() {
	source := 'typedef Db DbAlias;\nstruct App {\n\tDbAlias db;\n};\n\nstruct Db { int id; };\n'
	ordered := fastc_order_c_composite_definitions(source, {
		'App': {
			'db': 'DbAlias'
		}
	}, {
		'App': true
		'Db':  true
	}, {
		'DbAlias': 'Db'
	})
	db_index := ordered.index('struct Db {') or { -1 }
	app_index := ordered.index('struct App {') or { -1 }
	assert db_index >= 0 && db_index < app_index, ordered
}

fn test_selfhost_type_aliases_are_hoisted_before_composite_fields() {
	source := '#define Redirect__found ((Redirect)(Status__found))\nstruct Request {\n\tRedirectFn on_redirect;\n};\n\ntypedef int (*RedirectFn)(Handle*);\ntypedef struct handle Handle;\n#define Status__found ((Status)302)\n'
	ordered := fastc_hoist_c_type_aliases(source)
	status_index := ordered.index('#define Status__found') or { -1 }
	redirect_index := ordered.index('#define Redirect__found') or { -1 }
	alias_index := ordered.index('typedef int (*RedirectFn)') or { -1 }
	handle_index := ordered.index('typedef struct handle Handle') or { -1 }
	request_index := ordered.index('struct Request {') or { -1 }
	assert redirect_index >= 0 && status_index > redirect_index && status_index < handle_index, ordered
	assert handle_index < alias_index && alias_index < request_index, ordered
}

fn test_c_directive_hoisting_preserves_source_order() {
	source := 'one\n#include <x.h>\ntwo\n# if FLAG\nthree\n#ifdef INNER\nfour\n#endif\n#else\nfive\n#endif\nsix'
	hoisted := fastc_hoist_c_directives(source)
	assert hoisted.directives == '#include <x.h>\n\n'
	assert hoisted.conditional_code == '# if FLAG\nthree\n#ifdef INNER\nfour\n#endif\n#else\nfive\n#endif\n\n'
	assert hoisted.body == 'one\ntwo\nsix\n'
}

fn test_nested_fixed_array_struct_field_uses_native_c_dimensions() {
	typ := fastc_fixed_array_type('4', fastc_fixed_array_type('256', 'u32'))
	assert fastc_fixed_array_field_declaration(typ, 'values')? == 'u32 values[4][256]'
}

fn test_selfhost_comptime_type_condition_accepts_disjunctions() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn fastc_size[T](value T) int {
	$if T is bool || T is u8 || T is i8 {
		return 1
	} $else $if T is $array_fixed {
		return value.len
	} $else $if T is $struct {
		return 2
	} $else {
		return -1
	}
}

fn main() {}
', 'comptime_type_disjunction.v', prefs) or { panic(err) }
	assert c_source.contains('int fastc_size(voidptr value)'), c_source
	assert c_source.contains('return -1;'), c_source
	assert !c_source.contains('return 2;'), c_source
}

fn test_selfhost_typeof_name_interpolation_uses_the_inferred_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

fn fastc_type_name[T](value T) string {
	return '\${typeof(value).name}'
}

fn main() {}
", 'typeof_name_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('return _S("voidptr");'), c_source
}

fn test_selfhost_embedded_field_promoted_in_compound_slice_bound() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Base {
mut:
	pos int
}

struct Scanner {
	Base
	input string
}

fn fastc_slice_two(s Scanner) string {
	return s.input[s.pos..s.pos + 2]
}

fn main() {}
', 'embed_slice_bound.v', prefs) or { panic(err) }
	// The high slice bound `s.pos + 2` is a compound expression, which the pure
	// member-chain renderer rejects; it must still promote the embedded `pos` field
	// like the receiver and low bound do.
	assert c_source.contains('__embedded_0.pos + 2'), c_source
}

fn test_selfhost_embedded_field_promoted_in_plain_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Base {
mut:
	pos int
}

struct Scanner {
	Base
}

fn fastc_getpos(s Scanner) int {
	return s.pos + 2
}

fn main() {}
', 'embed_plain.v', prefs) or { panic(err) }
	// A plain `s.pos` member access (rendered by the main token loop via
	// `expression_token`, not the slice/member-chain renderers) must promote the
	// embedded `pos` field to `s->__embedded_0.pos` rather than the invalid `s->pos`.
	assert c_source.contains('__embedded_0.pos'), c_source
}

fn test_selfhost_embedded_field_promoted_in_nested_call() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Base {
	handle int
}

struct Socket {
	Base
}

struct Conn {
	sock Socket
}

struct Addr {}

fn (addr &Addr) len() int {
	return 1
}

fn C.consume(handle int, length int) int

fn check(result int) ! {}

fn (mut c Conn) write(addr Addr) ! {
	check(C.consume(c.sock.handle, addr.len()))!
}

fn main() {}
', 'embed_nested_call.v', prefs) or { panic(err) }
	assert c_source.contains('consume(c->sock.__embedded_0.handle)'), c_source
	assert !c_source.contains('consume(c->sock.handle)'), c_source
}

fn test_selfhost_member_does_not_resolve_as_same_named_function_value() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct User {}

fn user(name string) User {
	return User{}
}

struct Source {
	name string
	user ?User
}

struct Target {
mut:
	user ?User
}

fn make_source() Source {
	return Source{
		name: maybe_name()!
		user: user("alex")
	}
}

fn maybe_name() !string {
	return "alex"
}

fn copy_user(mut target Target, source Source) {
	target.user = source.user
}

fn main() {}
', 'member_named_like_function.v', prefs) or { panic(err) }
	assert c_source.contains('target->user=source.user'), c_source
	assert !c_source.contains('target->user=&main__user'), c_source
	assert !c_source.contains('.main__user='), c_source
}

fn test_selfhost_nested_member_array_indexes_lower_innermost_first() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Token {
	goto_pc   int
	group_rep int
}

struct Re {
	prog []Token
}

fn group_repetition(re &Re, state int) int {
	return re.prog[re.prog[state].goto_pc].group_rep
}

fn main() {}
', 'nested_member_array_indexes.v', prefs) or { panic(err) }
	assert c_source.count('builtin__array_get') >= 2, c_source
	assert !c_source.contains('re->prog['), c_source
}

fn test_selfhost_implicit_generic_specializes_compound_arguments() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn fastc_abs[T](value T) T {
	return if value < 0 { -value } else { value }
}

fn total(x f64, values []f64) f64 {
	return fastc_abs(x * 2.0) + fastc_abs(values[0])
}

fn main() {}
', 'implicit_generic_compound_arguments.v', prefs) or { panic(err) }
	assert c_source.contains('main__fastc_abs_mono_f64(x*2.0)'), c_source
	assert c_source.contains('main__fastc_abs_mono_f64((*(f64 *)builtin__array_get(values, 0)))'), c_source

	assert !c_source.contains('main__fastc_abs(x*2.0)'), c_source
}

fn test_selfhost_append_map_index_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

fn fastc_append_kinds(m map[string]int) []int {
	mut result := []int{}
	result << m['x']
	return result
}

fn main() {}
", 'append_map_index.v', prefs) or { panic(err) }
	// A map index in an append RHS (`result << m['x']`) must be lowered to a map get,
	// not left as the invalid raw C `m[...]`.
	assert c_source.contains('map_get'), c_source
}

fn test_selfhost_append_to_array_stored_in_map() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

fn add(mut groups map[string][]int, key string, value int) {
	groups[key] << value
}

fn main() {
	mut groups := map[string][]int{}
	add(mut groups, 'x', 1)
}
", 'selfhost_append_to_map_array.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_append_map_target'), c_source
	assert c_source.contains('builtin__map_get_check'), c_source
	assert !c_source.contains('groups[key]'), c_source
}

fn test_selfhost_append_to_nested_array() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn add(mut groups [][]int, index int, value int) {
	groups[index] << value
}

fn main() {
	mut groups := [][]int{len: 1}
	add(mut groups, 0, 42)
}
', 'selfhost_append_to_nested_array.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_append_array_target'), c_source
	assert c_source.contains('builtin__array_get(*(groups), index)'), c_source
	assert c_source.contains('builtin____new_array(0,0,sizeof(int))'), c_source
	assert c_source.contains('((Array_int *)__v_fastc_array_init.data)[__v_fastc_array_index]'), c_source
	assert !c_source.contains('groups[index]'), c_source
}

fn test_selfhost_fixed_array_elements_skip_dynamic_inner_array_initialization() {
	prefs := pref.new_preferences()
	g := Parser{
		prefs: prefs
		s: scanner.new_scanner(prefs, .normal)
		struct_fields: {
			'array': {
				'len': 'int'
			}
		}
	}
	mut tokens := [
		FastcExpressionToken{
			tok: .name
			lit: 'array'
			typ: 'Array_FixedArray_2_int'
		},
		fastc_test_expression_token(.lcbr, '{'),
		fastc_test_expression_token(.name, 'len'),
		fastc_test_expression_token(.colon, ':'),
		fastc_test_expression_token(.number, '1'),
		fastc_test_expression_token(.rcbr, '}'),
	]
	fixed := g.render_struct_literal_expression(tokens) or { panic('fixed array literal was not rendered') }
	assert fixed.source.contains('sizeof(' + 'FixedArray_2_int' + ')'), fixed.source
	assert !fixed.source.contains('builtin____new_array(0,0,sizeof(int))'), fixed.source
	assert !fixed.source.contains('((FixedArray_2_int *)__v_fastc_array_init.data)'), fixed.source

	tokens[0].typ = 'Array_Array_int'
	dynamic := g.render_struct_literal_expression(tokens) or { panic('dynamic array literal was not rendered') }
	assert dynamic.source.contains('builtin____new_array(0,0,sizeof(int))'), dynamic.source
	assert dynamic.source.contains('((Array_int *)__v_fastc_array_init.data)'), dynamic.source
}

fn test_selfhost_append_array_result_to_struct_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct State {
mut:
	values []string
}

fn load_values() ![]string {
	return [\'one\', \'two\']
}

fn add_values(mut state State) ! {
	state.values << load_values()!
}

fn main() {
	mut state := State{}
	add_values(mut state) or { panic(err) }
}
', 'selfhost_append_array_result_to_struct_field.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__array_push_many'), c_source
	assert !c_source.contains('state->values<<'), c_source
}

fn test_selfhost_empty_array_assigned_to_member_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Cfg {
mut:
	fields []string
}

fn fastc_reset(mut c Cfg) {
	c.fields = []
}

fn main() {}
', 'empty_array_field.v', prefs) or { panic(err) }
	// `c.fields = []` (empty array literal assigned to a member target) must lower to a
	// typed empty array, not the invalid raw `c->fields=[]`.
	assert c_source.contains('(Array_string){0}'), c_source
	assert !c_source.contains('fields=[]'), c_source
}

fn test_selfhost_nonempty_array_assigned_to_member_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Cfg {
mut:
	fields []string
}

fn fastc_reset(mut c Cfg, name string) {
	c.fields = [name]
}

fn main() {}
', 'nonempty_array_field.v', prefs) or { panic(err) }
	assert c_source.contains('c->fields=((Array_string)builtin__new_array_from_c_array'), c_source
	assert !c_source.contains('array_get(c->fields='), c_source
}

fn test_selfhost_if_expression_assigned_to_string_field_infers_string() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

struct Cfg {
mut:
	name string
}

fn fastc_pick(mut c Cfg, cond bool) {
	c.name = if cond { 'a' } else { 'b' }
}

fn main() {}
", 'if_assign_string.v', prefs) or { panic(err) }
	// An if-expression assigned to a `string` member field must infer `string` for its
	// branches, not wrap them in an Option (which happened when the member assignment
	// did not set `expected_expression_type` to the field type and a stale `Option`
	// leaked in).
	assert c_source.contains('_S("a")'), c_source
	assert !c_source.contains('__v_fastc_box_value = (_S("a"))'), c_source
}

fn test_selfhost_result_method_or_block_is_a_statement() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Writer {}

interface IError {}

fn panic(err IError) {}

fn (mut writer Writer) write(values []u8) !int {
	return values.len
}

fn pad(mut writer Writer) {
	writer.write([]u8{}) or { panic(err) }
}

fn main() {
	mut writer := Writer{}
	pad(mut writer)
}
', 'result_method_or_statement.v', prefs) or { panic(err) }
	assert c_source.contains('Option __v_fastc_option_'), c_source
}

fn test_selfhost_comptime_if_starts_or_block_statement() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn attempt() ! {}

fn use() {
	attempt() or {
		$if fastc_disabled_trace ? {
			println("trace")
		}
	}
}

fn main() {
	use()
}
', 'selfhost_comptime_if_or_statement.v', prefs) or { panic(err) }
	assert c_source.contains('Option __v_fastc_option_'), c_source
	assert !c_source.contains('_S("trace")'), c_source
}

fn test_selfhost_array_lookup_or_block_checks_bounds() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

fn first(values []string) string {
	return values[0] or { '' }
}

fn main() {
	_ := first([]string{})
}
", 'array_lookup_or_block.v', prefs) or { panic(err) }
	assert c_source.contains('bool __v_fastc_array_missing = __v_fastc_array_index < 0 || __v_fastc_array_index >= __v_fastc_array.len;'), c_source
	assert c_source.contains('(Option){.data=__v_fastc_array_value, .state=__v_fastc_array_missing ? 2 : 0}'), c_source
}

fn test_selfhost_array_slices_use_the_runtime_helper() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn make_values() []int {
	return []int{}
}

fn make_text() string {
	return "abc"
}

fn middle(values []int, start int, end int) []int {
	return values[start..end]
}

fn values_tail() []int {
	return make_values()[1..]
}

fn text_tail() string {
	return make_text()[1..]
}

type Text = string

fn alias_middle(value Text) string {
	return value[1..2]
}

fn alias_tail(value Text) string {
	return value[1..]
}

struct Buffer {
mut:
	bytes []u8
}

fn write_bytes(mut destination []u8, source []u8) int {
	return source.len
}

fn write_tail(mut buffer Buffer, source []u8) int {
	return write_bytes(mut buffer.bytes[1..], source)
}

fn clone_head(mut values []int) []int {
	return values[..1].clone()
}

fn main() {
	values := []int{}
	result := middle(values, 0, 0)
	println(result.len)
	values_tail()
	text_tail()
	alias_middle(Text("abc"))
	alias_tail(Text("abc"))
	mut buffer := Buffer{}
	write_tail(mut buffer, []u8{})
	mut mutable_values := [1]
	clone_head(mut mutable_values)
}
', 'array_slice.v', prefs) or { panic(err) }
	assert c_source.contains('return builtin__array_slice(values, start, end);'), c_source
	assert c_source.contains('__typeof__((make_values())) __v_fastc_slice_receiver = (make_values()); builtin__array_slice(__v_fastc_slice_receiver, 1, __v_fastc_slice_receiver.len);'), c_source
	assert c_source.contains('__typeof__((make_text())) __v_fastc_slice_receiver = (make_text()); builtin__string_substr((__v_fastc_slice_receiver), 1, __v_fastc_slice_receiver.len);'), c_source
	assert c_source.contains('return builtin__string_substr((value), 1, 2);'), c_source
	assert c_source.contains('return builtin__string_substr((value), 1, value.len);'), c_source
	assert !c_source.contains('builtin__array_slice(value, 1'), c_source
	assert !c_source.contains('make_values().len'), c_source
	assert !c_source.contains('make_text().len'), c_source
	assert !c_source.contains('__v_slice.flags |= ArrayFlags__is_slice'), c_source
	assert c_source.contains('__v_fastc_mut_argument = (({ __typeof__((buffer->bytes)) __v_fastc_slice_receiver'), c_source
	assert !c_source.contains('__v_fastc_slice_receiver->len'), c_source
	assert c_source.contains('write_bytes(({ __typeof__(('), c_source
	assert c_source.contains('builtin__array_clone(&(builtin__array_slice(*(values), 0, 1)))'), c_source
}

fn test_range_bound_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { for i in 0.0 .. 2.0 { println(1) } }\n',
		'module main\nfn main() { for i in 0 .. 2.0 { println(1) } }\n',
		'module main\nfn main() { for i in false .. true { println(1) } }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_range_bounds.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', message
	}
}

fn test_range_bound_integer_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { for i in u64(0) .. -1 { println(i) } }\n',
		'module main\nfn main() { for i in i64(0) .. u64(3) { println(i) } }\n',
	] {
		mut message := ''
		_ := generate(source, 'incompatible_range_bounds.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', message
	}

	generate('module main\nfn main() { for i in u64(0) .. 3 { println(i) } }\n', 'compatible_range_bound_literal.v', prefs) or { panic(err) }
}

fn test_literal_range_must_not_be_empty() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { for i in 4 .. 2 { println(i) } }\n',
		'module main\nfn main() { for i in 2 .. 2 { println(i) } }\n',
		'module main\nfn main() { for i in 4 .. -2 { println(i) } }\n',
	] {
		mut message := ''
		_ := generate(source, 'empty_literal_range.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('empty range:'), message
		assert message.contains('will never execute'), message
	}

	c_source := generate('module main\nfn main() { for i in 2 .. 4 { println(i) } }\n', 'valid_literal_range.v', prefs) or { panic(err) }
	assert c_source.contains('for (__typeof__((__v_fastc_range_start_0)) i = (__v_fastc_range_start_0); i < (__v_fastc_range_end_1); i++) {'), c_source
}

fn test_arithmetic_operand_types_are_not_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { println(true + false) }\n',
		'module main\nfn main() { value := true * false; println(value) }\n',
		'module main\nfn main() { mut value := true; value += false; println(value) }\n',
		"module main\nfn main() { mut value := 'abc'; value++; println(value) }\n",
		"module main\nfn main() { mut value := 'abc'; value--; println(value) }\n",
		'module main\nfn main() { mut value := 1; mut pointer := &value; pointer++ }\n',
	] {
		mut message := ''
		_ := generate(source, 'non_numeric_arithmetic.v', prefs) or {
			message = err.msg()
			''
		}
		assert message == '', message
	}

	c_source := generate('module main

fn main() {
	mut value := 1
	mut pointer := &value
	value++
	unsafe {
		pointer--
	}
}
', 'numeric_and_pointer_mutations.v', prefs) or { panic(err) }
	assert c_source.contains('value++;'), c_source
	assert c_source.contains('pointer--;'), c_source
}

fn test_nil_requires_an_unsafe_block() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn show(p &int) { println(*p) }\nfn main() { show(nil) }\n', 'nil_outside_unsafe.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('`nil` outside an `unsafe` block'), message

	c_source := generate('module main\nfn accept(p &int) {}\nfn main() { unsafe { accept(nil) } }\n', 'nil_inside_unsafe.v', prefs) or { panic(err) }
	assert c_source.contains('accept(NULL);')
}

fn test_bitwise_negation_operand_type_is_not_validated() {
	prefs := pref.new_preferences()
	bool_c_source := generate('module main\nfn main() { println(~true) }\n', 'bool_bit_not.v', prefs) or { panic(err) }
	assert bool_c_source.contains('println(~((bool)true));'), bool_c_source

	c_source := generate('module main\nfn main() { println(~1) }\n', 'integer_bit_not.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(~1);')
}

fn test_value_only_expression_statements_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { 1 }\n',
		'module main\nfn main() { true }\n',
		'module main\nfn main() { value := 1; value }\n',
		'module main\nfn main() { int(1) }\n',
	] {
		mut message := ''
		_ := generate(source, 'value_expression_statement.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('value-only expression statement'), message
	}

	c_source := generate('module main\nfn touch() {}\nfn main() { mut count := 0; touch(); count++ }\n', 'valid_expression_statements.v', prefs) or { panic(err) }
	assert c_source.contains('touch();')
	assert c_source.contains('count++;')
}

fn test_selfhost_function_pointer_call_is_a_statement() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn invoke(f fn ()) {
	f()
}

fn callback() {}

fn main() {
	invoke(callback)
}
', 'selfhost_function_pointer_statement.v', prefs) or { panic(err) }
	assert c_source.contains('f();'), c_source
}

fn test_selfhost_function_alias_local_call_is_a_statement() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Create = fn () voidptr
type Destroy = fn (voidptr)

fn symbol() voidptr {
	return unsafe { nil }
}

fn use() {
	create := Create(symbol())
	state := create()
	destroy := Destroy(symbol())
	destroy(state)
}

fn main() {
	use()
}
', 'selfhost_function_alias_local_call.v', prefs) or { panic(err) }
	assert c_source.contains('destroy(state);'), c_source
}

fn test_selfhost_function_alias_preserves_type_only_shared_parameter() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Callback = fn (shared int)

fn main() {}
', 'selfhost_function_alias_shared_parameter.v', prefs) or { panic(err) }
	assert c_source.contains('typedef void (*Callback)(int*);'), c_source
}

fn test_selfhost_static_option_constructor_propagates_payload_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Key {}

fn Key.new() !Key {
	return Key{}
}

fn (key Key) use() {}

fn make() ! {
	key := Key.new()!
	key.use()
}

fn main() {
	make() or { return }
}
', 'selfhost_static_option_constructor.v', prefs) or { panic(err) }
	assert c_source.contains('*((Key *)__v_fastc_option_propagate.data)'), c_source
	assert c_source.contains('Key_use(key);'), c_source
}

fn test_selfhost_static_option_call_with_or_block() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Reader {}

fn Reader.read(path string) !string {
	return path
}

fn load(path string) string {
	return Reader.read(path) or { "" }
}

fn main() {
	_ := load("file")
}
', 'selfhost_static_option_or.v', prefs) or { panic(err) }
	assert c_source.contains('Reader_read(path)'), c_source
	assert !c_source.contains('string_read(Reader'), c_source
}

fn test_selfhost_static_option_call_with_argument_and_propagation() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Key {
	value int
}

fn Key.new(value int) !Key {
	return Key{ value: value }
}

fn make() ! {
	key := Key.new(3)!
	_ = key
}

fn main() {}
', 'selfhost_static_option_argument_propagation.v', prefs) or { panic(err) }
	assert c_source.contains('Key_new(3)'), c_source
	assert !c_source.contains('Key.new(3)'), c_source
}

fn test_selfhost_option_call_contextualizes_array_arguments() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn insert(columns []string, values []string) !int {
	return columns.len + values.len
}

fn use(mut id int, value int) ! {
	id = insert(["id"], [value.str()]) or {
		return err
	}
}

fn main() {
	mut id := 0
	use(mut id, 2) or { return }
}
', 'selfhost_option_array_arguments.v', prefs) or { panic(err) }
	assert c_source.contains('insert(((Array_string)builtin__new_array_from_c_array'), c_source
	assert !c_source.contains('insert([_S('), c_source
}

fn test_selfhost_nested_option_propagation_inside_cast() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn maybe() !u64 {
	return u64(2)
}

fn use() !int {
	value := -int(maybe()!) - 1
	return value
}

fn main() {
	use() or { return }
}
', 'selfhost_nested_propagation_cast.v', prefs) or { panic(err) }
	assert c_source.contains('Option __v_fastc_option_propagate = (maybe())'), c_source
	assert !c_source.contains('maybe()!'), c_source
}

fn test_selfhost_nested_option_propagation_keeps_method_lowering() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Clock {}

struct App {
	db      int
	started i64
}

fn maybe() !int {
	return 1
}

fn now() Clock {
	return Clock{}
}

fn (clock Clock) unix() i64 {
	return 2
}

fn make() !App {
	return App{
		db: maybe()!
		started: now().unix()
	}
}

fn main() {
	make() or { return }
}
', 'selfhost_nested_propagation_method.v', prefs) or { panic(err) }
	assert c_source.contains('Clock_unix(now())'), c_source
	assert !c_source.contains('now().unix()'), c_source
}

fn test_selfhost_outer_propagation_with_propagated_call_argument() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn inner() ![]u8 {
	return [u8(1)]
}

fn outer(_ []u8) ! {}

fn run() ! {
	outer(inner()!)!
}

fn main() {
	run() or { return }
}
', 'selfhost_nested_argument_and_outer_propagation.v', prefs) or { panic(err) }
	assert c_source.contains('Option __v_fastc_option_propagate = (outer('), c_source
	assert !c_source.contains('outer(inner()!)!'), c_source
}

fn test_selfhost_assert_statement_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn check(value int) {
	assert value == 1
	assert value > 0, "positive"
}

fn main() {
	check(1)
}
', 'selfhost_assert_statement.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__panic(_S("assertion failed"))'), c_source
	assert c_source.contains('builtin__panic(_S("positive"))'), c_source
}

fn test_nested_mutations_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { x := 1; println(x++) }\n',
		'module main\nfn main() { mut x := 1; println(x = 2) }\n',
		'module main\nfn main() { mut x := 1; y := x++; println(y) }\n',
	] {
		mut message := ''
		_ := generate(source, 'nested_mutation.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('mutation'), message
		assert message.contains('inside an expression'), message
	}

	c_source := generate('module main\nfn main() { mut x := 1; x++; x += 2; println(x) }\n', 'mutation_statements.v', prefs) or { panic(err) }
	assert c_source.contains('x++;')
	assert c_source.contains('x+=2;')
}

fn test_bare_return_from_main_emits_zero() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn stop() {
	return
}

fn main() {
	if true {
		return
	}
}
', 'bare_return.v', prefs) or { panic(err) }
	assert c_source.contains('void stop(void) {\n\treturn;\n}')
	assert c_source.contains('if (((bool)true)) {\n\t\treturn 0;\n\t}')
}

fn test_non_void_fallthrough_is_rejected_without_return_value_validation() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn value() int {}\nfn main() { println(value()) }\n',
		'module main\nfn value() int { if true { return 1 } }\nfn main() { println(value()) }\n',
		'module main\nfn value() int { return }\nfn main() { println(value()) }\n',
		'module main\nfn value() int { for { break } }\nfn main() {}\n',
		'module main\nfn value(flag bool) int { for { if flag { break } } }\nfn main() {}\n',
	] {
		mut message := ''
		_ := generate(source, 'non_void_fallthrough.v', prefs) or {
			message = err.msg()
			''
		}
		if source.contains('return }') {
			assert message == '', message
		} else {
			assert message.contains('non-void function `value` that can fall through'), message
		}
	}
	c_source := generate('module main

fn value(flag bool) int {
	if flag {
		return 1
	} else {
		return 2
	}
}

fn main() {
	println(value(true))
}
', 'non_void_returns.v', prefs) or { panic(err) }
	assert c_source.contains('return 1;')
	assert c_source.contains('return 2;')
	infinite_source := generate('module main

fn wait_forever() int {
	for {}
}

fn nested_wait() int {
	for {
		for {}
		break
	}
}

fn main() {}
', 'infinite_loop_returns.v', prefs) or { panic(err) }
	assert infinite_source.count('for (;;) {') == 3, infinite_source
}

fn test_integer_range_caches_bounds() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn start() int {
	return 0
}

fn limit() int {
	return 3
}

fn main() {
	for i in start() .. limit() {
		println(i)
	}
}
', 'range_bounds.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_range_start_0 = (start());')
	assert c_source.contains('__v_fastc_range_end_1 = (limit());')
	assert c_source.contains('i < (__v_fastc_range_end_1)')
	assert !c_source.contains('i < (limit())')
}

fn test_decimal_literals_preserve_v_values() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(0_123)
}
', 'literal_values.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(123);')
}

fn test_v_octal_literals_are_translated_to_gnu_c() {
	assert fastc_c_number('0o17')! == '017'
	assert fastc_c_number('0O7_1')! == '071'
	mut oversized_message := ''
	_ := fastc_c_number('0o20000000000') or {
		oversized_message = err.msg()
		''
	}
	assert oversized_message.contains('high-bit nondecimal literals')
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(0o17)
}
', 'octal_literal.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(017);')
}

fn test_hex_string_escape_has_fixed_width_in_c() {
	prefs := pref.new_preferences()
	c_source := generate("module main\nfn main() { println('\\x61ardvark') }\n", 'hex_escape.v', prefs) or { panic(err) }
	assert c_source.contains(r'println("\141ardvark");')
}

fn test_partial_octal_string_escapes_are_reencoded() {
	assert fastc_c_string(r"'\1'")! == r'"\\1"'
	assert fastc_c_string(r"'\12'")! == r'"\\12"'
	assert fastc_c_string(r"'\123'")! == r'"\123"'
}

fn test_string_line_continuations_match_v_unescaping() {
	prefs := pref.new_preferences()
	source := r"module main

fn main() {
	println('left\
	   right')
}
"
	c_source := generate(source, 'continued_string.v', prefs) or { panic(err) }
	assert c_source.contains(r'println("leftright");')
	crlf_literal := "'left\\" + '\r\n' + "\t  right'"
	assert fastc_c_string(crlf_literal)! == '"leftright"'
	assert fastc_c_string(r"'left\nright'")! == r'"left\nright"'
}

fn test_runtime_sensitive_constructs_are_rejected() {
	prefs := pref.new_preferences()
	for source in ['module main

fn main() {
	println("a\\0b")
}
', "module main\nfn main() { println('\\400tail') }\n"] {
		mut nul_failed := false
		_ := generate(source, 'nul_string.v', prefs) or {
			nul_failed = true
			''
		}
		assert nul_failed
	}
	assert fastc_string_contains_nul(r'\400tail', false)
	assert !fastc_string_contains_nul(r'\401tail', false)
	non_nul_octal_c := generate("module main\nfn main() { println('\\401tail') }\n", 'non_nul_octal_string.v', prefs) or { panic(err) }
	assert non_nul_octal_c.contains(r'println("\401tail");')

	mut assert_failed := false
	_ := generate('module main

fn main() {
	assert false
}
', 'assert.v', prefs) or {
		assert_failed = true
		''
	}
	assert assert_failed
}

fn test_expressions_without_safe_lowering_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn show(x int, n int) { println(x << n) }\nfn main() { show(1, 32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x <<= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x >>= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x >>>= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn divide(a int, b int) int { return a / b }\nfn main() { println(divide(1, 0)) }\n',
		'module main\nfn modulo(a int, b int) int { return a % b }\nfn main() { println(modulo(1, 0)) }\n',
		'module main\nfn divide(b int) { mut x := 1; x /= b; println(x) }\nfn main() { divide(0) }\n',
		'module main\nfn modulo(b int) { mut x := 1; x %= b; println(x) }\nfn main() { modulo(0) }\n',
		'module main\nfn main() { println(sizeof(string)) }\n',
		"module main\nfn main() { s := 'abc'; println(s[0]) }\n",
		"module main\nfn main() { println(c'a') }\n",
		'module main\nfn main() { println(`A`) }\n',
		'module main\nfn show(r rune) { println(r) }\nfn main() { show(65) }\n',
		'module main\nfn main() { println(rune(65)) }\n',
		'module main\nfn show(p charptr) { println(p) }\nfn main() { unsafe { show(nil) } }\n',
		'module main\nfn main() { p := charptr(0); println(p) }\n',
		'module main\nfn main() { println(1 ^ 2 + 3) }\n',
		'module main\nfn main() { println(10 & 3 + 1) }\n',
		'module main\nfn main() { println(1 | 2 ^ 3) }\n',
		'module main\nfn main() { println(1 & 2 * 3) }\n',
		'module main\nfn main() { mut x := -2_147_483_648; x--; println(x) }\n',
		'module main\nfn main() { for i := -2_147_483_648; true; i-- { println(i); break } }\n',
		'module main\nfn main() { mut x := -2_147_483_648 - 1; println(x) }\n',
		'module main\nfn main() { x := 2_147_483_649 | 0; println(x) }\n',
		'module main\nfn main() { x := 0xffff_ffff | 0; println(x) }\n',
		'module main\nfn main() { x := 0b11111111111111111111111111111111 | 0; println(x) }\n',
		'module main\nfn main() { mut a := 1; mut b := 2; a, b = b, a; println(a); println(b) }\n',
	] {
		mut failed := false
		_ := generate(source, 'typed_expression.v', prefs) or {
			failed = true
			''
		}
		assert failed
	}

	bool_c := generate('module main\nfn main() { println(true) }\n', 'bool_literal.v', prefs) or {
		panic(err)
	}
	assert bool_c.contains('println(((bool)true));')
	low_hex_c := generate('module main\nfn main() { x := 0x7fff_ffff | 0; println(x) }\n', 'low_hex_literal.v', prefs) or { panic(err) }
	assert low_hex_c.contains('__typeof__((0x7fffffff|0)) x = (0x7fffffff|0);')
	low_binary_c := generate('module main\nfn main() { x := 0b01111111111111111111111111111111 | 0; println(x) }\n', 'low_binary_literal.v', prefs) or { panic(err) }
	assert low_binary_c.contains('__typeof__((0b01111111111111111111111111111111|0))')
	max_int_c := generate('module main\nfn main() { x := 2_147_483_647 - 1; println(x) }\n', 'max_int_expression.v', prefs) or { panic(err) }
	assert max_int_c.contains('__typeof__((2147483647-1)) x = (2147483647-1);')
	call_c := generate('module main\nfn sum(a int, b int) int { return a + b }\nfn main() { println(sum(1, 2)) }\n', 'call_comma.v', prefs) or { panic(err) }
	assert call_c.contains('println(sum(1,2));')
}

fn test_selfhost_unsigned_right_shift_assignment_is_logical_and_guarded() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Holder {
mut:
	value i64
}

fn main() {
	mut direct := i64(-5)
	direct >>>= 1
	mut holder := Holder{value: i64(-5)}
	holder.value >>>= u64(64)
	mut values := [i64(-5)]
	values[0] >>>= 1
	println(direct)
	println(holder.value)
	println(values[0])
}
', 'unsigned_right_shift_assignment.v', prefs) or { panic(err) }
	assert !c_source.contains('>>>='), c_source
	assert c_source.count('u64 __v_fastc_unsigned_shift_count =') == 3, c_source
	assert c_source.count('u64 __v_fastc_unsigned_shift_value =') == 3, c_source
	assert c_source.count('__v_fastc_unsigned_shift_count >= 64') == 3, c_source
	assert c_source.count('__v_fastc_unsigned_shift_value >> __v_fastc_unsigned_shift_count') == 3, c_source
}

fn test_selfhost_main_result_propagation_panics_and_runs_defers() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

fn fail() ! {}

fn main() {
	defer {
		println('cleanup')
	}
	fail()!
}
", 'main_result_propagation.v', prefs) or { panic(err) }
	panic_call := 'builtin__panic_result_not_set(builtin__IError_msg(__v_fastc_option_propagate.err));'
	assert c_source.contains(panic_call), c_source
	assert c_source.contains('if (__v_fastc_option_propagate.state) {'), c_source
	assert c_source.count('println(_S("cleanup"));') == 2, c_source
	assert !c_source.contains('if (__v_fastc_option_propagate.state) { return 1; }'), c_source
	used := fastc_collect_referenced_function_names([], prefs, map[string]FastcFunctionSignature{})
	assert used['panic_result_not_set']
}

fn test_string_alias_for_in_uses_the_underlying_representation() {
	source := "module main

type Text = string

fn main() {
	value := Text('ab')
	for ch in value {
		println(ch)
	}
}
"
	ordinary_prefs := pref.new_preferences()
	ordinary_c := generate(source, 'ordinary_string_alias_iteration.v', ordinary_prefs) or {
		panic(err)
	}
	assert ordinary_c.contains('string __v_fastc_collection_'), ordinary_c
	assert ordinary_c.contains('strlen(__v_fastc_collection_'), ordinary_c
	assert ordinary_c.contains('((const unsigned char *)__v_fastc_collection_'), ordinary_c

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_string_alias_iteration_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, ordinary_c) or { panic(err) }
	tcc := os.join_path(ordinary_prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '97\n98\n'

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	selfhost_c := generate(source, 'selfhost_string_alias_iteration.v', selfhost_prefs) or {
		panic(err)
	}
	assert selfhost_c.contains('__typeof__((value)) __v_fastc_collection_'), selfhost_c
	assert selfhost_c.contains('__v_fastc_collection_0.len'), selfhost_c
	assert selfhost_c.contains('__v_fastc_collection_0.str'), selfhost_c
}

fn test_interface_argument_boxing_at_call_site() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Animal {
	speak() int
}

struct Dog {}

fn (d Dog) speak() int {
	return 7
}

fn make_speak(a Animal) int {
	return a.speak()
}

struct Holder {
	a Animal
}

fn main() {
	d := Dog{}
	r := make_speak(d)
	literal := make_speak(Dog{})
	h := Holder{
		a: Dog{}
	}
	mut cur := Animal(Dog{})
	cur = Dog{}
	println(r + literal + h.a.speak() + cur.speak())
}
', 'interface_argument_boxing.v', prefs) or { panic(err) }
	// A concrete struct used where an interface is expected is boxed with its type
	// id so the generated dispatch can recover the receiver. This is exercised for
	// a call argument (local var and inline literal), a struct-literal field, and
	// assignment to an interface variable.
	assert c_source.contains('._object='), c_source
	assert c_source.contains('._typ=__v_typeid_Dog'), c_source
	assert c_source.contains('case __v_typeid_Dog:'), c_source
	assert c_source.contains('__v_fastc_box_value = (d)'), c_source
	assert c_source.contains('__v_fastc_box_value = ((Dog){})'), c_source
	// Assignment to an interface variable boxes rather than emitting `cur=(Dog){}`.
	assert c_source.contains('cur=(Animal){._object='), c_source
}

fn test_sum_type_construction_match_and_smartcast() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Dog {
	sound int
}

struct Cat {
	sound int
}

type Animal = Cat | Dog

fn describe(a Animal) int {
	match a {
		Dog { return a.sound + 1 }
		Cat { return a.sound + 2 }
	}
	return 0
}

fn main() {
	a := Animal(Dog{ sound: 10 })
	_ := describe(a)
}
', 'sum_type_dispatch.v', prefs) or { panic(err) }
	// A sum type shares the boxed layout with interfaces so construction, match
	// dispatch and smart-casting reuse the interface machinery.
	assert c_source.contains('typedef struct { void *_object; u32 _typ; void *_methods; } Animal;'), c_source
	// Construction boxes the concrete variant with its type id.
	assert c_source.contains('._typ=__v_typeid_Dog'), c_source
	assert c_source.contains('Dog __v_fastc_box_value ='), c_source
	assert !c_source.contains('Dog{sound:'), c_source
	// `match` dispatches on the boxed `_typ` tag rather than comparing structs.
	assert c_source.contains('_typ == __v_typeid_Dog'), c_source
	assert c_source.contains('_typ == __v_typeid_Cat'), c_source
	// The matched branch smart-casts the subject to the concrete variant so field
	// access (`a.sound`) resolves through the uniquely named narrowed temporary.
	assert c_source.contains('Dog __v_fastc_match_cast_'), c_source
	assert c_source.contains('Cat __v_fastc_match_cast_'), c_source
	assert c_source.contains('return __v_fastc_match_cast_'), c_source
	assert !c_source.contains('return a.sound'), c_source
}

fn test_sum_type_primitive_variants() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Value = bool | int

fn kind(v Value) int {
	match v {
		int { return v + 100 }
		bool { return if v { 1 } else { 0 } }
	}
	return -1
}

fn main() {
	a := Value(42)
	b := Value(true)
	_ := kind(a)
	_ := kind(b)
}
', 'sum_type_primitive.v', prefs) or { panic(err) }
	// Primitive scalars get stable type ids in a high range that cannot collide
	// with the sequential declared-type ids.
	assert c_source.contains('#define __v_typeid_int 1073741824'), c_source
	assert c_source.contains('#define __v_typeid_bool '), c_source
	// A primitive is boxed into the sum type with its type id on construction.
	assert c_source.contains('int __v_fastc_box_value = (42)'), c_source
	assert c_source.contains('._typ=__v_typeid_int'), c_source
	// `match` dispatches on the tag and smart-casts to the primitive C type.
	assert c_source.contains('_typ == __v_typeid_int'), c_source
	assert c_source.contains('int v = *(int *)'), c_source
	assert c_source.contains('bool v = *(bool *)'), c_source
}

fn test_sum_type_array_variants() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Value = int | []int

fn kind(v Value) int {
	match v {
		int { return v }
		[]int { return v.len }
	}
	return -1
}

fn main() {
	a := Value(9)
	b := Value([1, 2, 3])
	_ := kind(a)
	_ := kind(b)
}
', 'sum_type_array.v', prefs) or { panic(err) }
	// A composite (`[]int` -> `Array_int`) variant gets a stable type id in the
	// composite range and is registered so it also gets a typedef.
	assert c_source.contains('#define __v_typeid_Array_int '), c_source
	// The array literal is boxed as a real construction, not raw `[1,2,3]`.
	assert c_source.contains('Array_int __v_fastc_box_value = (((Array_int)builtin__new_array_from_c_array'), c_source
	assert c_source.contains('._typ=__v_typeid_Array_int'), c_source
	// `match []int` dispatches on the composite tag and smart-casts to the array.
	assert c_source.contains('_typ == __v_typeid_Array_int'), c_source
	assert c_source.contains('Array_int v = *(Array_int *)'), c_source
}

fn test_sum_type_match_as_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Value = bool | int

fn score(v Value) int {
	return match v {
		int { v + 100 }
		bool { if v { 1 } else { 0 } }
	}
}

fn main() {
	a := Value(5)
	_ := score(a)
}
', 'sum_type_match_expr.v', prefs) or { panic(err) }
	// A `match` used as an expression over a sum type dispatches on the boxed tag
	// (not a struct-vs-type comparison) and smart-casts inside each branch value.
	assert c_source.contains('__v_fastc_match_0._typ == __v_typeid_int'), c_source
	assert c_source.contains('int v = *(int *)__v_fastc_match_0._object'), c_source
	assert c_source.contains('bool v = *(bool *)__v_fastc_match_0._object'), c_source
	assert !c_source.contains('(__v_fastc_match_0) == (int)'), c_source
}

fn test_sum_type_map_and_nested_variants() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Node {}

type Value = int | map[string]int | []Node

fn kind(v Value) int {
	match v {
		int { return 1 }
		map[string]int { return v.len }
		[]Node { return v.len }
	}
	return -1
}

fn main() {
	m := map[string]int{}
	a := Value(m)
	_ := kind(a)
}
', 'sum_type_map.v', prefs) or { panic(err) }
	// A `map[K]V` variant becomes the composite `Map_<k>_<v>` and a `[]Struct`
	// variant `Array_<Struct>`; both dispatch on their composite type id and
	// smart-cast to the composite C type.
	assert c_source.contains('_typ == __v_typeid_Map_string_int'), c_source
	assert c_source.contains('Map_string_int v = *(Map_string_int *)'), c_source
	assert c_source.contains('_typ == __v_typeid_Array_Node'), c_source
	assert c_source.contains('Array_Node v = *(Array_Node *)'), c_source
}

fn test_sum_type_recursive_array_variant_append_boxes_one_element() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Value = int | []Value

fn main() {
	mut dst := []Value{}
	src := [Value(1)]
	dst << src
	_ := dst
}
', 'sum_type_recursive_append.v', prefs) or { panic(err) }
	// `[]Value` is a variant of `Value`, so `dst << src` boxes the whole `src` array as
	// one `Value` element instead of copying its elements. The append must use the
	// single-element push and box `src` with the composite `Array_Value` type id.
	assert !c_source.contains('builtin__array_push_many'), c_source
	assert c_source.contains('__v_typeid_Array_Value'), c_source
	assert c_source.contains('builtin__array_push('), c_source
}

fn test_sum_type_nonrecursive_array_append_is_push_many() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Plain = int | string

fn main() {
	mut dst := []Plain{}
	src := [Plain(1)]
	dst << src
	_ := dst
}
', 'sum_type_nonrecursive_append.v', prefs) or { panic(err) }
	// `Plain` has no array variant, so `[]Plain << []Plain` stays a push-many append
	// that copies each element, matching the main C backend.
	assert c_source.contains('builtin__array_push_many'), c_source
}

fn test_sum_type_smartcast_variant_append_is_boxed() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	value int
}

type Node = Item | string

fn retain_items(nodes []Node) []Node {
	mut result := []Node{}
	for node in nodes {
		if node is Item {
			result << node
		}
	}
	return result
}

fn main() {}
', 'sum_type_smartcast_append.v', prefs) or { panic(err) }
	assert c_source.contains('main__Node __v_fastc_push_value_'), c_source
	assert c_source.contains('__v_typeid_main__Item'), c_source
	assert c_source.contains('builtin__array_push((array *)&result'), c_source
}

fn test_mut_sum_type_smartcast_mutates_boxed_payload() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
mut:
	value int
}

type Node = Item | string

fn update(node Node) {
	mut current := node
	if mut current is Item {
		current.value = 2
	}
}

fn main() {}
', 'mut_sum_type_smartcast.v', prefs) or { panic(err) }
	assert c_source.contains('Item* __v_fastc_if_cast_'), c_source
	assert c_source.contains('->value=2'), c_source
}

fn test_condition_loop_smartcast_subject_is_evaluated_once() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct CallExpr {}
struct NameExpr {}
type Expr = CallExpr | NameExpr

struct Argument {
	expr Expr
}

fn next_index() int {
	return 0
}

fn scan(args []Argument) {
	for args[next_index()].expr is CallExpr {
		break
	}
}

fn main() {
	scan([Argument{ expr: CallExpr{} }])
}
', 'condition_loop_smartcast_once.v', prefs) or { panic(err) }
	// The rewritten type-test conjunct owns evaluation of the subject. Its temporary must not
	// evaluate the indexed expression again in the per-iteration prelude.
	assert c_source.contains('Expr __v_fastc_smartcast_subject_'), c_source
	assert c_source.contains('= (Expr){0};'), c_source
	assert c_source.count('next_index()') == 1, c_source
}

fn test_mut_sum_type_smartcast_passes_original_box_to_mut_sum_type_parameter() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
mut:
	value int
}

type Node = Item | string

fn inspect(mut node Node) {
	if mut node is Item {
		node.value++
		inspect(mut node)
	}
}

fn main() {}
', 'mut_sum_type_smartcast_argument.v', prefs) or { panic(err) }
	assert c_source.contains('inspect(__v_fastc_smartcast_subject_'), c_source
	assert !c_source.contains('inspect(__v_fastc_if_cast_'), c_source
}

fn test_mut_match_sum_type_smartcast_passes_original_box_to_mut_parameter() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
mut:
	value int
}

type Node = Item | string

fn inspect(mut node Node) {
	match mut node {
		Item {
			node.value++
			inspect(mut node)
		}
		else {}
	}
}

fn main() {}
', 'mut_match_sum_type_smartcast_argument.v', prefs) or { panic(err) }
	assert c_source.contains('inspect(node)'), c_source
	assert !c_source.contains('inspect(__v_fastc_match_cast_'), c_source
}

fn test_selfhost_string_array_index_uses_value_equality() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn locate(names []string, wanted string) (int, int) {
	return names.index(wanted), names.last_index(wanted)
}

fn main() {}
', 'string_array_index.v', prefs) or { panic(err) }
	assert c_source.count('builtin__string_eq(__v_fastc_index_item') == 2, c_source
	assert c_source.contains('int __v_fastc_index_cursor = 0;'), c_source
	assert c_source.contains('int __v_fastc_index_cursor = __v_fastc_index_collection.len - 1;'), c_source
}

fn test_mut_match_member_smartcast_reads_current_boxed_payload() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct First {
	value int
}

struct Second {
	value int
}

type Info = First | Second

struct Holder {
mut:
	info Info
}

fn refresh(mut holder Holder) {
	holder.info = First{ value: 2 }
}

fn value(mut holder Holder) int {
	match mut holder.info {
		First {
			refresh(mut holder)
			return holder.info.value
		}
		else {
			return 0
		}
	}
}

fn main() {}
', 'mut_match_current_payload.v', prefs) or { panic(err) }
	assert c_source.contains('((First *)__v_fastc_match_'), c_source
	assert c_source.contains('->_object)->value'), c_source
}

fn test_reference_to_boxed_interface_has_heap_lifetime() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Value {}

struct Item {}

fn boxed_pointer() &Value {
	return &Value(Item{})
}

fn main() {}
', 'boxed_interface_pointer.v', prefs) or { panic(err) }
	assert c_source.contains('(Value *)v_fastc_interface_box(&__v_fastc_iface_ref, sizeof(Value))'), c_source
	assert !c_source.contains('&__v_fastc_iface_ref;'), c_source
}

fn test_boxed_address_payload_has_heap_lifetime() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	value int
}

struct Other {}

type Value = Item | Other

fn boxed(value Item) Value {
	return Value(&value)
}

fn conditionally_boxed(value Item, flag bool) &Value {
	return if flag { &Value(Other{}) } else { &Value(&value) }
}

fn main() {}
', 'boxed_address_payload.v', prefs) or { panic(err) }
	assert c_source.contains('v_fastc_interface_box((const void*)(&(value)), sizeof(Item))'), c_source
	assert c_source.contains('v_fastc_interface_box((const void*)(&value), sizeof(Item))'), c_source
}

fn test_generic_monomorphization() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn box[T](x T) T {
	return x
}

fn main() {
	a := box(5)
	b := box("hi")
	c := box[int](9)
	_ := a
	_ := b
	_ := c
}
', 'generic_mono.v', prefs) or { panic(err) }
	// A single-type-parameter generic is monomorphized into one concrete copy per
	// instantiation (int inferred from a literal, string from a literal, int from
	// an explicit type argument), and every call is rewritten to the mangled name.
	assert c_source.contains('int box_mono_int(int x)'), c_source
	assert c_source.contains('string box_mono_string(string x)'), c_source
	assert c_source.contains('box_mono_int(5)'), c_source
	assert c_source.contains('box_mono_string(_S("hi"))'), c_source
	assert c_source.contains('box_mono_int(9)'), c_source
	// The generic template itself leaves no `box(` definition behind.
	assert !c_source.contains(' box(voidptr'), c_source
}

fn test_selfhost_imported_explicit_generic_call_uses_erased_signature() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	main_source := 'module main
import library

struct App {}
struct Context {}

fn main() {
	mut app := App{}
	library.run[App, Context](mut app, value: 3)!
}
'
	library_source := 'module library

@[params]
pub struct Config {
	value int
}

pub fn run[A, B](mut app A, config Config) ! {
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'library.v'
			source: library_source
			header: fastc_scan_source_header(library_source, 'library.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('library__run('), c_source
	assert c_source.contains('.value='), c_source
	assert !c_source.contains('run[App'), c_source
}

fn test_generic_monomorphization_variable_args() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn identity[T](x T) T {
	return x
}

fn use_param(n int) int {
	return identity(n)
}

fn main() {
	m := 7
	r := identity(m)
	s := identity("hi")
	_ := use_param(3)
	_ := r
	_ := s
}
', 'generic_var_args.v', prefs) or { panic(err) }
	// Concrete type inferred from a function parameter (`n int`), a local declared
	// from a literal (`m := 7`), and a literal argument.
	assert c_source.contains('int identity_mono_int(int x)'), c_source
	assert c_source.contains('string identity_mono_string(string x)'), c_source
	assert c_source.contains('identity_mono_int(n)'), c_source
	assert c_source.contains('identity_mono_int(m)'), c_source
	assert c_source.contains('identity_mono_string(_S("hi"))'), c_source
}

fn test_generic_struct_monomorphization() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Box[T] {
	value T
	tag   string
}

fn main() {
	a := Box[int]{ value: 5, tag: "n" }
	b := Box[string]{ value: "hi", tag: "s" }
	_ := a
	_ := b
}
', 'generic_struct.v', prefs) or { panic(err) }
	// A generic struct is monomorphized into one concrete struct per instantiation
	// with the type parameter substituted in its fields, and each `S[T]` reference
	// is rewritten to the mangled name.
	assert c_source.contains('struct Box_mono_int {'), c_source
	assert c_source.contains('struct Box_mono_string {'), c_source
	assert c_source.contains('int value;'), c_source
	assert c_source.contains('string value;'), c_source
	assert c_source.contains('(Box_mono_int){'), c_source
	assert c_source.contains('(Box_mono_string){'), c_source
}

fn test_generic_struct_method_monomorphization() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Wrapper[T] {
	val T
}

fn (w Wrapper[T]) get() T {
	return w.val
}

fn main() {
	wi := Wrapper[int]{ val: 21 }
	ws := Wrapper[string]{ val: "hi" }
	_ := wi.get()
	_ := ws.get()
}
', 'generic_method.v', prefs) or { panic(err) }
	// A method on a generic struct is monomorphized per instantiation: the receiver
	// type becomes the mangled struct name (with a single underscore, since `__` is
	// FastC's module separator) so its C method name resolves correctly.
	assert c_source.contains('int Wrapper_mono_int_get(Wrapper_mono_int w)'), c_source
	assert c_source.contains('string Wrapper_mono_string_get(Wrapper_mono_string w)'), c_source
	assert c_source.contains('Wrapper_mono_int_get(wi)'), c_source
	assert c_source.contains('Wrapper_mono_string_get(ws)'), c_source
}

fn test_struct_array_field_default_initializes_element_size() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct IntStack {
mut:
	items []int
}

fn (mut s IntStack) push(x int) {
	s.items << x
}

fn main() {
	mut st := IntStack{}
	st.push(3)
	_ := st
}
', 'struct_array_default.v', prefs) or { panic(err) }
	// A `[]int` field with no explicit default must construct as a properly sized
	// empty array (element_size set), not a zeroed one, so `s.items << x` copies the
	// element. `X{}` gets the field default applied.
	assert c_source.contains('.items=(((Array_int)builtin____new_array(0, 0, sizeof(int))))'), c_source
}

fn test_struct_array_alias_field_default_initializes_element_size() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type ByteBuilder = []u8

struct Output {
mut:
	bytes ByteBuilder
}

fn (mut o Output) write(x u8) {
	o.bytes << x
}

fn main() {
	mut out := Output{}
	out.write(3)
	_ := out
}
', 'struct_array_alias_default.v', prefs) or { panic(err) }
	// An omitted array-alias field still needs a typed empty array. In particular,
	// strings.Builder fields otherwise start with element_size 0 and corrupt writes.
	assert c_source.contains('.bytes=(((ByteBuilder)builtin____new_array(0, 0, sizeof(u8))))'), c_source
}

fn test_struct_map_field_default_initializes_runtime() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Registry {
mut:
	data map[string]int
}

fn (mut r Registry) set(k string, v int) {
	r.data[k] = v
}

fn main() {
	mut reg := Registry{}
	reg.set("a", 5)
	_ := reg
}
', 'struct_map_default.v', prefs) or { panic(err) }
	// A `map[string]int` field with no explicit default must construct as a real
	// `new_map` (key/value sizes + hash/eq/clone/free), not a zeroed map, so a
	// later `m.data[k] = v` works. `X{}` gets the field default applied.
	assert c_source.contains('.data=((builtin__new_map(sizeof(string), sizeof(int), &builtin__map_hash_string, &builtin__map_eq_string, &builtin__map_clone_string, &builtin__map_free_string)))'), c_source
}

fn test_generic_multiple_type_parameters() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Pair[K, V] {
	key   K
	value V
}

fn (p Pair[K, V]) same(v V) bool {
	return p.value == v
}

fn firstof[T, U](a T, b U) T {
	return a
}

fn main() {
	p := Pair[string, int]{ key: "age", value: 30 }
	_ := p.same(30)
	_ := firstof[int, string](5, "x")
}
', 'generic_multi_param.v', prefs) or { panic(err) }
	// Multiple type parameters are substituted independently; the mangled name
	// joins the concrete args with `_` (a struct, its method, and a function).
	assert c_source.contains('struct Pair_mono_string_int {'), c_source
	assert c_source.contains('string key;'), c_source
	assert c_source.contains('int value;'), c_source
	assert c_source.contains('Pair_mono_string_int_same(Pair_mono_string_int p, int v)'), c_source
	assert c_source.contains('int firstof_mono_int_string(int a, string b)'), c_source
}

fn test_comptime_for_fields_unrolling() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Point {
	x int
	y int
}

fn describe(p Point) string {
	mut r := ""
	\$for field in Point.fields {
		r += field.name
		r += "="
	}
	return r
}

fn total(p Point) int {
	mut t := 0
	\$for field in Point.fields {
		t += p.\$(field.name)
	}
	return t
}

fn main() {
	p := Point{ x: 1, y: 2 }
	_ := describe(p)
	_ := total(p)
}
', 'comptime_for.v', prefs) or { panic(err) }
	// `$for field in Point.fields` unrolls once per field: `field.name` becomes the
	// field-name string, and `p.$(field.name)` becomes a static field access.
	assert c_source.contains('r=builtin__string_plus(r,_S("x"))'), c_source
	assert c_source.contains('r=builtin__string_plus(r,_S("y"))'), c_source
	assert c_source.contains('t+=p.x'), c_source
	assert c_source.contains('t+=p.y'), c_source
}

fn test_comptime_for_field_typ_is_dispatch() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Row {
	id   int
	name string
	ok   bool
	note ?string
}

fn kinds(r Row) string {
	mut out := ""
	\$for field in Row.fields {
		\$if field.typ is int {
			out += "i"
		} \$else \$if field.typ is string {
			out += "s"
		} \$else \$if field.typ is ?string {
			out += "o"
		} \$else {
			out += "?"
		}
	}
	return out
}

fn main() { _ := kinds(Row{ id: 1, name: "a", ok: true }) }
', 'comptime_typ_is.v', prefs) or { panic(err) }
	// Per-field `$if field.typ is X` takes the matching branch: int, string, else.
	assert c_source.contains('out=builtin__string_plus(out,_S("i"))'), c_source
	assert c_source.contains('out=builtin__string_plus(out,_S("s"))'), c_source
	assert c_source.contains('out=builtin__string_plus(out,_S("o"))'), c_source
	assert c_source.contains('out=builtin__string_plus(out,_S("?"))'), c_source
}

fn test_comptime_for_contextual_shared_loop_variable() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Point {
	x int
	y int
}

fn describe(p Point) string {
	mut out := ""
	mut total := 0
	\$for shared in Point.fields {
		\$if shared.typ is int {
			out += shared.name
			total += p.\$(shared.name)
		}
	}
	_ = total
	return out
}

fn main() {
	_ := describe(Point{ x: 1, y: 2 })
}
', 'comptime_for_contextual_shared.v', prefs) or { panic(err) }
	assert c_source.contains('out=builtin__string_plus(out,_S("x"))'), c_source
	assert c_source.contains('total+=p.x'), c_source
	assert c_source.contains('out=builtin__string_plus(out,_S("y"))'), c_source
	assert c_source.contains('total+=p.y'), c_source
}

fn test_selfhost_sum_type_field_default_is_boxed() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Null {}

type Value = Null | int

struct Box {
	value Value = Null{}
}

fn main() {
	b := Box{}
	_ := b
}
', 'selfhost_sum_field_default.v', prefs) or { panic(err) }
	// A variant literal defaulting a sum-type field is boxed, not left as the bare
	// variant struct which is not assignable to the boxed `{_object,_typ,_methods}`.
	assert c_source.contains('.value=('), c_source
	assert c_source.contains('._typ=__v_typeid_Null'), c_source
}

fn test_selfhost_sum_type_constant_field_default_is_not_reboxed() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Empty {
	value int = 1
}

type Expr = Empty | int

const empty_expr = Expr(Empty{})

struct Holder {
	expr Expr = empty_expr
}

fn main() {
	_ = Holder{}
}
', 'selfhost_sum_constant_field_default.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_struct_default.expr=(main__empty_expr);'), c_source
	assert !c_source.contains('__v_fastc_box_value = (main__empty_expr)'), c_source
	assert c_source.contains('static Expr main__empty_expr;'), c_source
	assert c_source.contains('main__empty_expr = (Expr){._object='), c_source
	assert c_source.contains('._typ=__v_typeid_Empty'), c_source
}

fn test_selfhost_interface_method_named_like_keyword() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Conn {
	select(id int) int
}

struct Db {}

fn (d Db) select(id int) int {
	return id
}

fn run(c Conn) int {
	return c.select(5)
}

fn main() {
	db := Db{}
	_ := run(db)
}
', 'selfhost_keyword_interface_method.v', prefs) or { panic(err) }
	// `select` is also a keyword; it must still be collected as an interface method
	// (so its dispatch function exists) and the call routed to that dispatch.
	assert c_source.contains('Conn_select('), c_source
	assert c_source.contains('Conn_select(c,5)'), c_source
}

fn test_selfhost_spaced_shared_selector_call() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Worker {}

fn (worker Worker) shared(value int) int {
	_ = worker
	return value
}

fn main() {
	worker := Worker{}
	_ = worker.shared (1)
	mut values := [1]
	for mut shared in values {
		_ = worker.shared(1)
	}
}
', 'selfhost_spaced_shared_selector_call.v', prefs) or { panic(err) }
	assert c_source.count('Worker_shared(worker,1)') == 2, c_source
	assert !c_source.contains('worker.&'), c_source
	assert !c_source.contains('worker.(*'), c_source
}

fn test_selfhost_match_multi_array_branch_smartcasts_to_array() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Value = []int | []string | int

fn value_len(v Value) int {
	return match v {
		[]int, []string {
			v.len
		}
		else {
			-1
		}
	}
}

fn main() {
	_ := value_len(Value(5))
}
', 'selfhost_match_multi_array.v', prefs) or { panic(err) }
	// A branch listing several array variants cannot pick one element type, so `v` is
	// smart-cast to the shared `array` layout to expose common fields like `len`.
	assert c_source.contains('array v = *(array *)'), c_source
}

fn test_selfhost_append_of_method_call_result() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Counter {
	n int
}

fn (c Counter) value() int {
	return c.n
}

fn build(c Counter, xs []int) []int {
	mut result := xs
	result << c.value()
	return result
}

fn main() {
	_ := build(Counter{ n: 2 }, [1])
}
', 'selfhost_append_method_call.v', prefs) or { panic(err) }
	// The appended value is a method call and must be routed through the argument
	// renderer, not streamed raw as `c.value()` (an unresolved struct field call).
	assert c_source.contains('Counter_value(c)'), c_source
}

fn test_orm_where_operator_mapping() {
	// The `update ... where` lowering maps V comparison tokens to `orm.OperationKind`
	// variant names; `!=` becomes `neq`, and unsupported operators return ''.
	assert fastc_orm_where_op(token.Token.eq) == 'eq'
	assert fastc_orm_where_op(token.Token.ne) == 'neq'
	assert fastc_orm_where_op(token.Token.gt) == 'gt'
	assert fastc_orm_where_op(token.Token.lt) == 'lt'
	assert fastc_orm_where_op(token.Token.ge) == 'ge'
	assert fastc_orm_where_op(token.Token.le) == 'le'
	assert fastc_orm_where_op(token.Token.plus) == ''
}

fn test_orm_field_zero_mapping() {
	// The `select` row-parser uses these zero literals for the `match` else-branch when
	// unboxing a column Primitive to its field type; unsupported types return none.
	assert fastc_orm_field_zero('int')? == '0'
	assert fastc_orm_field_zero('i64')? == '0'
	assert fastc_orm_field_zero('u32')? == '0'
	assert fastc_orm_field_zero('f64')? == '0.0'
	assert fastc_orm_field_zero('f32')? == '0.0'
	assert fastc_orm_field_zero('bool')? == 'false'
	assert fastc_orm_field_zero('string')? == "''"
	assert fastc_orm_field_zero('time__Time')? == 'time.Time{}'
	assert fastc_orm_field_zero('Array_int') == none
	assert fastc_orm_field_match_type('time__Time') == 'time.Time'
	assert fastc_orm_field_match_type('int') == 'int'
}

fn test_orm_select_or_exit_detection() {
	prefs := pref.new_preferences()
	assert fastc_orm_or_source_starts_with_exit('return []Item{}', prefs)
	assert fastc_orm_or_source_starts_with_exit('continue', prefs)
	assert !fastc_orm_or_source_starts_with_exit('[]Item{}', prefs)
}

fn test_source_uses_sql_detection() {
	prefs := pref.new_preferences()
	// A `sql <conn> { ... }` block (statement or expression form) is detected; `sql`
	// as part of another name, after `.`, or inside a string is not.
	assert fastc_source_uses_sql('fn f() { sql db { insert x into Y }! }', prefs)
	assert fastc_source_uses_sql('fn f() { x := sql app.db { select from Y } }', prefs)
	assert !fastc_source_uses_sql('fn f() { a := sql_query() }', prefs)
	assert !fastc_source_uses_sql('fn f() { obj.sql(db) }', prefs)
	assert !fastc_source_uses_sql('fn f() { println("sql db {") }', prefs)
	assert !fastc_source_uses_sql('fn f() { return 1 }', prefs)
}

fn test_flag_is_skippable() {
	// `#flag` payloads that only affect linking or header lookup are skippable; a `-D`
	// define or other compile flag is not.
	assert fastc_flag_is_skippable('-lsqlite3')
	assert fastc_flag_is_skippable('-I@VEXEROOT/thirdparty/legacy/include/LegacySupport')
	assert fastc_flag_is_skippable('-L/usr/lib -lpq')
	assert fastc_flag_is_skippable('-framework Cocoa')
	assert !fastc_flag_is_skippable('-DSQLITE_ENABLE')
	assert !fastc_flag_is_skippable('-std=c11')
}

fn test_hex_nul_string_literal_is_renderable() {
	// A `\x00`/`\000` NUL escape renders to the stable octal `\000` and the `_S`
	// sizeof-based length preserves it; other NUL escapes stay unrenderable.
	assert !fastc_string_has_unrenderable_nul(r'\x00tail', false)
	assert !fastc_string_has_unrenderable_nul(r'\000tail', false)
	assert fastc_string_has_unrenderable_nul(r'\0tail', false)
	assert fastc_string_has_unrenderable_nul(r'\400tail', false)
}

fn test_selfhost_spawn_method_call_packs_receiver() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := 'module main

struct Worker {
	id int
}

fn (w Worker) run() int {
	return w.id
}

fn main() {
	w := Worker{ id: 5 }
	h := spawn w.run()
	println(h.wait())
}
'
	c_source := generate(source, 'selfhost_spawn_method.v', prefs) or { panic(err) }
	// A method spawn calls the method C name with the receiver packed as arg0.
	assert c_source.contains('args->result = Worker_run(args->arg0);'), c_source
}

fn test_selfhost_spawn_method_call_with_shared_receiver_name() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := 'module main

struct Worker {
	id int
}

fn (w Worker) run() int {
	return w.id
}

fn main() {
	shared := Worker{ id: 5 }
	h := spawn shared.run()
	println(h.wait())
}
'
	c_source := generate(source, 'selfhost_spawn_shared_receiver.v', prefs) or { panic(err) }
	assert c_source.contains('args->result = Worker_run(args->arg0);'), c_source
}

fn test_selfhost_spawn_method_named_shared() {
	$if windows {
		return
	}
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := 'module main

struct Worker {
	id int
}

fn (w Worker) shared() int {
	return w.id
}

fn main() {
	w := Worker{ id: 5 }
	h := spawn w.shared()
	println(h.wait())
}
'
	c_source := generate(source, 'selfhost_spawn_shared_method.v', prefs) or { panic(err) }
	assert c_source.contains('args->result = Worker_shared(args->arg0);'), c_source
}

fn test_selfhost_spawn_qualified_function_named_shared() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_spawn_qualified_shared_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'worker')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'worker', 'worker.v')
	os.write_file(main_file, 'module main\nimport worker\nfn main() { h := spawn worker.shared(5); println(h.wait()) }\n') or {
		panic(err)
	}
	os.write_file(module_file, 'module worker\npub fn shared(value int) int { return value }\n') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	sources, aliases := fastc_resolve_source_files([main_file], prefs) or { panic(err) }
	prefs.building_v = true
	c_source, _, _ := generate_source_files(sources, aliases, prefs) or { panic(err) }
	assert c_source.contains('args->result = worker__shared(args->arg0);'), c_source
}

fn test_selfhost_or_block_with_trailing_value_fallback() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := "module main

fn main() {
	m := {
		'a': 1
		'b': 2
	}
	mut flag := 0
	x := m['zzz'] or {
		flag = 9
		42
	}
	println(x)
	println(flag)
}
"
	c_source := generate(source, 'selfhost_or_value.v', prefs) or { panic(err) }
	// The or-block runs its leading statement (`flag = 9`), then uses the trailing value
	// (`42`) on failure and the unwrapped option value on success, via an
	// `__v_fastc_or_result` temp.
	assert c_source.contains('__v_fastc_or_result'), c_source
	assert c_source.contains('flag=9'), c_source
	assert c_source.contains(' = (42)'), c_source
}

fn test_selfhost_or_block_with_multiline_struct_fallback_is_a_value() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {}

fn get(items map[string]Item) Item {
	return items[\'missing\'] or {
		Item{}
	}
}

fn main() {
	_ := get(map[string]Item{})
}
', 'selfhost_multiline_struct_fallback.v', prefs) or { panic(err) }
	assert c_source.contains('? ((Item){}) : *((Item *)'), c_source
	assert !c_source.contains('if (__v_fastc_option_'), c_source
}

fn test_veb_template_compiles_to_builder() {
	dir := os.join_path(os.temp_dir(), 'v3_veb_tmpl_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	tmpl := os.join_path_single(dir, 't.html')
	os.write_file(tmpl, '<h1>@{name}</h1>\n@if ok {\n<p>%greeting</p>\n@for x in xs {\n<li>@{x}</li>\n}\n}\n') or {
		panic(err)
	}
	src := fastc_veb_compile_template(tmpl, '__b', 'ctx') or { panic(err) }
	// The compiler accumulates plain text into the builder, HTML-escapes interpolations
	// via veb.filter_html, lowers @if/@for to control flow, and %key to veb.tr.
	assert src.contains("mut __b := ''"), src
	assert src.contains('veb.filter_html(name)'), src
	assert src.contains('if ok {'), src
	assert src.contains('for x in xs {'), src
	assert src.contains('veb.filter_html(x)'), src
	assert src.contains('veb.tr(ctx.lang.str(), "greeting")'), src
}

fn test_selfhost_match_branch_with_statement_and_multireturn() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn choose(k int) (int, string) {
	return match k {
		1 {
			x := 10
			x, "ten"
		}
		else {
			0, "zero"
		}
	}
}

fn main() {
	a, b := choose(1)
	println(a)
	println(b)
}
', 'selfhost_match_multireturn.v', prefs) or { panic(err) }
	// A match branch with leading statements and a trailing comma-separated value packs
	// the values into a MultiReturn, like a single-line multi-return branch: the branch's
	// leading `x := 10` renders, then its `x, "ten"` value is packed.
	assert c_source.contains('V_FASTC_MULTI_VALUE'), c_source
	assert c_source.contains('x = (10)'), c_source
}

fn test_veb_template_html_shorthands_and_dotted() {
	dir := os.join_path(os.temp_dir(), 'v3_veb_tmpl_html_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	tmpl := os.join_path_single(dir, 't.html')
	lines := [
		'<a href="/@author.username">@author.username</a>',
		"@css '/style.css'",
		"@js '/app.js'",
		'.card {',
		'<p>@issue.title</p>',
		'}',
		'span.tag {',
		'x',
		'}',
		'#main {',
		'y',
		'}',
	]
	os.write_file(tmpl, lines.join('\n') + '\n') or { panic(err) }
	src := fastc_veb_compile_template(tmpl, '__b', 'ctx') or { panic(err) }
	// A bare `@a.b.c` interpolation covers the whole member chain, `@css`/`@js` expand to
	// literal <link>/<script> tags, and `.class {` / `span.x {` / `#id {` / `}` lower to
	// HTML <div>/<span> open/close text (not V braces).
	assert src.contains('veb.filter_html(author.username)'), src
	assert src.contains('veb.filter_html(issue.title)'), src
	assert src.contains('<link href="/style.css"'), src
	assert src.contains('<script src="/app.js">'), src
	assert src.contains('<div class="card">'), src
	assert src.contains('<span class="tag">'), src
	assert src.contains('</span>'), src
	assert src.contains('<div id="main">'), src
	assert src.contains('</div>'), src
}

fn test_selfhost_method_on_or_unwrap() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Addr {
	x int
}

fn (a Addr) label() string {
	return "a"
}

fn get(fail bool) !Addr {
	return Addr{ x: 5 }
}

fn value_fallback(fail bool) string {
	return get(fail) or { Addr{ x: 0 } }.label()
}

fn diverging(fail bool) string {
	s := get(fail) or { return "early" }.label()
	return s
}

fn main() {
	println(value_fallback(false))
	println(diverging(false))
}
', 'selfhost_or_method.v', prefs) or { panic(err) }
	// `expr or { ... }.method()` without wrapping parens binds the trailing method to the
	// rendered or-unwrap `({ Option ...; ... })`, for both a value-fallback block and a
	// diverging (`return`) block.
	assert c_source.contains('label'), c_source
	assert c_source.contains('.state'), c_source
}

fn test_selfhost_skips_method_with_undefined_receiver() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Real {
	x int
}

fn (r Real) handle() int {
	return r.x
}

struct Broken {
	y int
}

// Dead, broken code: ctx is undefined here. It is kept past name-grouped reachability
// only because handle is also a genuinely-used method on Real. FastC must skip it
// (like the mainline compiler -skip-unused) rather than fail on ctx. The undefined ctx is
// used via an indexed member access (ctx.headers[...]), exercising the R.field[...] form
// of the detection (as veb no-ctx-param handlers do).
fn (b Broken) handle() int {
	marker := ctx.headers["x-marker-key"]
	return marker.len
}

fn main() {
	r := Real{ x: 5 }
	println(r.handle())
}
', 'selfhost_undefined_receiver.v', prefs) or { panic(err) }
	// `Real.handle` compiles; `Broken.handle` (undefined `ctx` receiver) is skipped, so
	// generation succeeds and no `x-marker-key` access is emitted.
	assert c_source.contains('Real_handle'), c_source
	assert !c_source.contains('x-marker-key'), c_source
}

fn test_selfhost_or_block_in_struct_literal_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Config {
	x int
}

struct App {
mut:
	a      int
	b      int
	config Config
}

fn connect(c Config) !int {
	return c.x
}

fn build(conf Config) int {
	app := App{
		a:      connect(conf) or { return -1 }
		b:      7
		config: conf
	}
	return app.a + app.b
}

fn main() {
	println(build(Config{ x: 5 }))
}
', 'selfhost_or_struct_field.v', prefs) or { panic(err) }
	// An `or` inside a struct-literal field value renders as a self-contained `({ Option
	// ... })` for that field only; the other fields (`b`, `config`) stay separate rather
	// than being swallowed into the first field`s value.
	assert c_source.contains('.a='), c_source
	assert c_source.contains('.b='), c_source
	assert c_source.contains('.config='), c_source
	assert c_source.contains('Option'), c_source
}

fn test_selfhost_array_sort_with_comparison() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct User {
	name string
	age  int
}

fn build() []User {
	mut xs := []User{}
	xs << User{ name: "bob", age: 30 }
	xs << User{ name: "al", age: 25 }
	xs.sort(a.age < b.age)
	return xs
}

fn main() {
	xs := build()
	println(xs.len)
}
', 'selfhost_sort.v', prefs) or { panic(err) }
	// `.sort(a.x < b.x)` generates a comparator (a/b bound to the element type in two
	// blocks with swapped assignments for the -1/+1 ordering) and lowers to a
	// sort_with_compare call.
	assert c_source.contains('sort_with_compare'), c_source
	assert c_source.contains('v_fastc_sort_'), c_source
	assert c_source.contains('a.age'), c_source
}

fn test_selfhost_default_struct_interpolation() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate(r'module main

struct Point {
	x int
	y int
}

fn describe(p Point) string {
	return "point is ${p}"
}

fn main() {
	println(describe(Point{ x: 1, y: 2 }))
}
', 'selfhost_struct_interp.v', prefs) or { panic(err) }
	// Interpolating a struct with no user `str()` auto-generates a default `str()` (V-style
	// `Type{ ... }`) and calls it from the interpolation.
	assert c_source.contains('v_fastc_default_str_'), c_source
	assert c_source.contains('Point{'), c_source
	// The field labels appear in the generated str() body.
	assert c_source.contains('    x: '), c_source
	assert c_source.contains('    y: '), c_source
	prototype := 'string v_fastc_default_str_Point(Point it);'
	definition := 'string v_fastc_default_str_Point(Point it) {'
	prototype_index := c_source.index(prototype) or { -1 }
	definition_index := c_source.index(definition) or { -1 }
	assert prototype_index >= 0, c_source
	assert prototype_index < definition_index, c_source
}

fn test_selfhost_explicit_default_struct_str_method() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate(r'module main

struct Point {
	x int
}

fn main() {
	s := Point{ x: 1 }.str()
	println(s)
}
', 'selfhost_struct_str_method.v', prefs) or { panic(err) }
	assert c_source.contains('v_fastc_default_str_Point('), c_source
	assert !c_source.contains('.str()'), c_source
}

fn test_direct_array_access_does_not_rewrite_a_lowered_member_suffix() {
	source := 'builtin__u8_is_digit(((str).str[i]))'
	assert fastc_replace_c_root_identifier(source, 'str[i]', '((str).str[i])') == source
}

fn test_direct_array_access_lowers_both_indexed_comparison_operands() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

@[direct_array_access]
fn compare(s string, a string, i int) bool {
	return s[i] > a[i]
}

fn main() {
	_ = compare("a", "b", 0)
}
', 'direct_array_access_comparison.v', prefs) or { panic(err) }
	assert c_source.contains('((s).str[i])'), c_source
	assert c_source.contains('((a).str[i])'), c_source
	assert !c_source.contains('>a[i]'), c_source
}

fn test_selfhost_parenthesized_membership_comparison_operand_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct State {
	values map[string]string
}

fn check(name string, state State, known bool) bool {
	return (name in state.values) != known
}

fn main() {
	_ = check("x", State{}, false)
}
', 'selfhost_membership_comparison_operand.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__map_get_check'), c_source
	assert !c_source.contains('name in state'), c_source
}

fn test_direct_array_access_lowers_indexed_method_receiver_once() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn (c u8) is_digit() bool {
	return c >= `0` && c <= `9`
}

@[direct_array_access]
fn check(str string, i int) bool {
	return (str[i] != `-` && str[i] != `+`) && !str[i].is_digit()
}

fn main() {
	_ = check("0", 0)
}
', 'direct_array_access_method.v', prefs) or { panic(err) }
	assert c_source.contains('u8_is_digit(((str).str[i]))'), c_source
	assert !c_source.contains('(str).((str).str[i])'), c_source
}

fn test_direct_string_index_builtin_method_receiver_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

@[direct_array_access]
fn character(str string, i int) string {
	return str[i].ascii_str()
}

fn main() {
	_ = character("0", 0)
}
', 'direct_string_index_builtin_method.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__u8_ascii_str(((str).str[i]))'), c_source
	assert !c_source.contains('str[i]).ascii_str'), c_source
}

fn test_pointer_global_array_field_index_before_member_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	name string
}

struct Table {
	items []&Item
}

__global table &Table

fn lookup(i int) string {
	return if i >= 0 && i < table.items.len {
		table.items[i].name
	} else {
		""
	}
}

fn main() {
	_ = lookup(0)
}
', 'pointer_global_array_field_index.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__array_get'), c_source
	assert !c_source.contains('table->items[i]'), c_source
	assert c_source.contains('))->name'), c_source
	assert !c_source.contains(')).name'), c_source
}

fn test_pointer_member_array_field_index_before_member_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	kind int
}

struct Ast {
	items []Item
}

struct Gen {
	a &Ast
}

fn lookup(g &Gen, i i64) int {
	return g.a.items[int(i)].kind
}

fn main() {
	_ = lookup(&Gen{}, 0)
}
', 'pointer_member_array_field_index.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__array_get'), c_source
	assert !c_source.contains('items[((int)'), c_source
}

fn test_pointer_member_array_indexed_by_array_element_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Node {
	kind int
}

struct Ast {
	nodes []Node
}

struct Parser {
	a &Ast
}

fn lookup(p &Parser, ids []int, i int) bool {
	return p.a.nodes[int(ids[i])].kind != 1
}

fn main() {
	_ = lookup(&Parser{}, [], 0)
}
', 'pointer_member_array_nested_index.v', prefs) or { panic(err) }
	assert c_source.count('builtin__array_get') >= 2, c_source
	assert !c_source.contains('p->a->nodes['), c_source
}

fn test_pointer_member_array_indexed_by_method_call_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Node {
	kind int
}

struct Ast {
	nodes []Node
}

fn (a &Ast) child(i int) int {
	return i
}

struct Transformer {
	a &Ast
}

fn is_first(t &Transformer, i int) bool {
	return t.a.nodes[int(t.a.child(i))].kind == 1
}

fn main() {
	_ = is_first(&Transformer{}, 0)
}
', 'pointer_member_array_method_index.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__array_get'), c_source
	assert !c_source.contains('t->a->nodes['), c_source
}

fn test_indexed_struct_pointer_field_chain_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Scope {
	parent &Scope
}

struct Branch {
	scope &Scope
}

struct Tree {
	branches []Branch
}

fn parent(tree &Tree, i int) &Scope {
	mut continuation_scope := tree.branches[i].scope.parent
	return continuation_scope
}

fn main() {
	_ = parent(&Tree{}, 0)
}
', 'indexed_struct_pointer_field_chain.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__array_get'), c_source
	assert c_source.contains('))->scope->parent'), c_source
	assert !c_source.contains('.scope.parent'), c_source
}

fn test_mut_map_index_field_in_if_expression_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Info {
	module_name string
}

fn existing_module(mut needed map[string]Info, name string) string {
	return if name in needed {
		needed[name].module_name
	} else {
		""
	}
}

fn main() {
	mut needed := map[string]Info{}
	_ = existing_module(mut needed, "x")
}
', 'mut_map_index_field_if_expression.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__map_get'), c_source
	assert !c_source.contains('(needed)[name]'), c_source
}

fn test_map_index_comparison_in_logical_expression_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn before(values map[int]int, key int, offset int) bool {
	return key !in values || offset < values[key]
}

fn main() {
	_ = before(map[int]int{}, 0, 0)
}
', 'map_index_logical_comparison.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__map_get'), c_source
	assert !c_source.contains('(values)[key]'), c_source
}

fn test_method_array_result_index_before_member_is_lowered() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Node {
	params []string
}

fn (node &Node) generic_params() []string {
	return node.params
}

fn has_generic(node Node) bool {
	return node.generic_params().len > 0 && node.generic_params()[0].len > 0
}

fn main() {
	_ = has_generic(Node{})
}
', 'method_array_result_index.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__array_get'), c_source
	assert !c_source.contains('Node_generic_params(&(node))[0]'), c_source
}

fn test_selfhost_or_in_array_literal_element() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn get(fail bool) !string {
	return "a"
}

fn pick(fallback string) string {
	for branch in [get(false) or { "" }, fallback, "main", "master"] {
		if branch != "" {
			return branch
		}
	}
	return fallback
}

fn main() {
	println(pick("fb"))
}
', 'selfhost_or_array.v', prefs) or { panic(err) }
	// An `or` inside an array-literal element is scoped to that element (not the whole `[..`
	// prefix), so the string array builds and the for-in gets a concrete element type.
	assert c_source.contains('new_array_from_c_array'), c_source
	assert c_source.contains('Option'), c_source
}

fn test_selfhost_named_args_collapse_into_struct() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Point {
	x int
	y int
}

fn build(p Point) int {
	return p.x + p.y
}

fn main() {
	v := build(x: 3, y: 4)
	println(v)
}
', 'selfhost_named_args.v', prefs) or { panic(err) }
	// `build(x: 3, y: 4)` collapses the trailing named args into the last struct parameter
	// (`build(Point{ x: 3, y: 4 })`), for a regular struct (not only `@[params]`).
	assert c_source.contains('.x='), c_source
	assert c_source.contains('.y='), c_source
	assert c_source.contains('build'), c_source
}

fn test_selfhost_primitive_cast_around_diverging_or() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn get(fail bool) !u64 {
	return 5
}

fn convert() int {
	x := int(get(false) or { return -1 })
	return x
}

fn main() {
	println(convert())
}
', 'selfhost_cast_or.v', prefs) or { panic(err) }
	// `int(f() or { return ... })`: the diverging (multi-statement) or-path under a primitive
	// cast uses the cast type for the unwrapped value and consumes the cast`s `)`.
	assert c_source.contains('Option'), c_source
	assert c_source.contains('int *)'), c_source
}

fn test_selfhost_for_in_inline_map_literal() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn main() {
	mut total := 0
	for _, v in {
		"a": 1
		"b": 2
	} {
		total += v
	}
	_ := total
}
', 'selfhost_for_map_literal.v', prefs) or { panic(err) }
	// A two-value for-in whose collection is an inline map literal opens with `{`; the
	// loop must read the map literal instead of stopping at that brace, so it iterates
	// over the map`s keys/values rather than an empty collection.
	assert c_source.contains('builtin__map_keys'), c_source
	assert c_source.contains('builtin__map_values'), c_source
}

fn test_selfhost_nested_blank_for_loops() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn main() {
	fields := ["a", "b", "c"]
	mut n := 0
	for _ in 0 .. 2 {
		for _ in fields {
			n += 1
		}
	}
	_ := n
}
', 'selfhost_blank_for.v', prefs) or { panic(err) }
	// `_` is the blank identifier: nested `for _ in ...` loops must not be rejected as
	// a redeclaration, and the range counter for `_` gets a private name, not `_`.
	assert c_source.contains('__v_fastc_range_index'), c_source
}

fn test_selfhost_if_is_smartcast_field_access() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Infix {
	name string
}

struct Other {
	x int
}

type Prim = Infix | Other

fn label(p Prim) string {
	if p is Infix {
		return p.name
	}
	return "?"
}

fn main() {
	iv := Infix{ name: "hi" }
	println(label(Prim(iv)))
}
', 'selfhost_is_smartcast.v', prefs) or { panic(err) }
	// Inside `if p is Infix { ... }` the boxed local `p` smart-casts to the concrete
	// variant: it is copied into a temporary and unboxed so `p.name` resolves.
	assert c_source.contains('Infix p = *((Infix *)'), c_source
}

fn test_selfhost_if_is_smartcast_member_field_access() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Writer {
	write()
}

struct File {
	fd int
}

fn (mut f File) write() {}

struct Holder {
mut:
	output Writer
}

fn flush(mut h Holder) {
	if mut h.output is File {
		match h.output.fd {
			1 {}
			else {}
		}
	}
}

fn main() {
	mut h := Holder{
		output: File{ fd: 1 }
	}
	flush(mut h)
}
', 'selfhost_member_is_smartcast.v', prefs) or { panic(err) }
	// A member smart-cast keeps a pointer to the boxed concrete value, and the nested
	// member chain is rendered through it so both type inference and C access use File.
	assert c_source.contains('File *__v_fastc_smartcast_member_'), c_source
	assert c_source.contains('__v_fastc_smartcast_member_'), c_source
	assert c_source.contains('->fd'), c_source
}

fn test_selfhost_if_multi_return_option_guard() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn split_pair(s string) ?(string, string) {
	if s == "" {
		return none
	}
	return s, s
}

fn joined(s string) string {
	if a, b := split_pair(s) {
		return a + b
	}
	return "none"
}

fn main() {
	_ := joined("x")
}
', 'selfhost_multi_guard.v', prefs) or { panic(err) }
	// `if a, b := opt_fn() { ... }` unwraps an option whose value is a multi-return
	// tuple: on success the boxed MultiReturn is copied out and each component bound.
	assert c_source.contains('MultiReturn'), c_source
	assert c_source.contains('.values[0], sizeof('), c_source
	assert c_source.contains('.values[1], sizeof('), c_source
}

fn test_if_expression_multi_return_guard_uses_pointer_backed_storage() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Large {
	bytes [40]u8
}

fn pair(ok bool) ?(Large, int) {
	if !ok {
		return none
	}
	return Large{}, 1
}

fn choose(ok bool) Large {
	return if value, _ := pair(ok) {
		value
	} else {
		Large{}
	}
}

fn main() {
	_ := choose(true)
}
', 'if_expression_large_multi_return_guard.v', prefs) or { panic(err) }
	assert c_source.contains('memcpy(&value, V_FASTC_MULTI_SOURCE(__v_fastc_multi_return_'), c_source
	assert !c_source.contains('.values[0].data, sizeof(value)'), c_source
}

fn test_selfhost_if_multi_return_option_guard_with_shared_bindings() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn split_pair() ?(int, int) {
	return 1, 2
}

fn shared_first() int {
	if shared, other := split_pair() {
		return shared + other
	}
	return 0
}

fn shared_second() int {
	if other, shared := split_pair() {
		return other + shared
	}
	return 0
}

fn main() {
	println(shared_first() + shared_second())
}
', 'selfhost_multi_guard_shared_bindings.v', prefs) or { panic(err) }
	assert c_source.contains('int shared ='), c_source
	assert c_source.contains('int other ='), c_source
}

fn test_selfhost_if_single_option_guard_with_shared_binding() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn maybe_value() ?int {
	return 1
}

fn read_value() int {
	if shared := maybe_value() {
		return shared
	}
	return 0
}

fn main() {
	println(read_value())
}
', 'selfhost_single_guard_shared_binding.v', prefs) or { panic(err) }
	assert c_source.contains('int shared ='), c_source
	assert c_source.contains('return shared;'), c_source
}

fn test_selfhost_multi_return_interface_component_is_macro_safe() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Frame {
	marker()
}

struct Padding {
	length int
}

fn (Padding) marker() {}

fn parse() ?(Frame, int) {
	return Padding{ length: 1 }, 2
}

fn main() {
	_, _ := parse() or { return }
}
', 'selfhost_multi_return_interface_macro.v', prefs) or { panic(err) }
	assert c_source.contains('V_FASTC_MULTI_VALUE(((Frame){'), c_source
}

fn test_selfhost_match_statement_smartcasts_member_subject() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Frame {
	marker()
}

struct DataFrame {
	data []u8
}

fn (DataFrame) marker() {}

struct Decoded {
	frame Frame
}

fn size(decoded Decoded) int {
	match decoded.frame {
		DataFrame { return decoded.frame.data.len }
		else { return 0 }
	}
}

fn main() {}
', 'selfhost_match_member_smartcast.v', prefs) or { panic(err) }
	assert c_source.contains('DataFrame *__v_fastc_smartcast_member_'), c_source
	assert c_source.contains('__v_fastc_smartcast_member_'), c_source
	assert c_source.contains('->data.len'), c_source
	assert !c_source.contains('decoded.frame.data'), c_source
}

fn test_selfhost_result_match_expression_error_branch_returns_option_error() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

enum Kind {
	one
	two
}

fn decode(bits u8) !Kind {
	return match bits {
		0 { Kind.one }
		1 { Kind.two }
		else { error('bad kind') }
	}
}

fn main() {}
", 'selfhost_result_match_error_branch.v', prefs) or { panic(err) }
	assert c_source.contains('return (Option){.err=builtin__error(_S("bad kind")), .state=1}'), c_source
	assert !c_source.contains('? (Kind__two) : (builtin__error'), c_source
}

fn test_selfhost_optional_enum_shorthand_return_uses_payload_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Kind {
	one
}

fn classify(ok bool) ?Kind {
	if ok {
		return .one
	}
	return none
}

fn main() {}
', 'selfhost_optional_enum_shorthand_return.v', prefs) or { panic(err) }
	assert c_source.contains('Kind __v_fastc_box_value = (Kind__one)'), c_source
	assert !c_source.contains('__v_fastc_box_value = (.one)'), c_source
	assert !c_source.contains('sizeof()'), c_source
}

fn test_selfhost_result_if_expression_error_branch_returns_option_error() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate("module main

fn classify(value int) !string {
	return if value == 1 {
		'one'
	} else if value == 2 {
		'two'
	} else {
		error('bad value')
	}
}

fn main() {}
", 'selfhost_result_if_error_branch.v', prefs) or { panic(err) }
	assert c_source.contains('return (Option){.err=builtin__error(_S("bad value")), .state=1}'), c_source
	assert !c_source.contains('? (_S("two")) : (builtin__error'), c_source
}

fn test_selfhost_function_typed_parameter() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn conv(x int) string {
	return "v"
}

fn apply(f fn (int) string, x int) string {
	return "got " + f(x)
}

fn main() {
	println(apply(conv, 7))
}
', 'selfhost_fn_param.v', prefs) or { panic(err) }
	// An inline function-typed parameter is declared as a real C function pointer with
	// unspecified args (so `f(x)` compiles as a direct call), and its return type is
	// recovered so `"got " + f(x)` resolves as a string concatenation.
	assert c_source.contains('string (*f)()'), c_source
	assert c_source.contains('builtin__string_plus'), c_source
}

fn test_selfhost_function_typed_struct_field_with_mut_argument() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Server {
mut:
	on_stopped fn (mut s Server) = unsafe { nil }
}

fn (mut s Server) stop() {
	if s.on_stopped != unsafe { nil } {
		s.on_stopped(mut s)
	}
}

fn main() {}
', 'selfhost_fn_field.v', prefs) or { panic(err) }
	assert c_source.contains('((void (*)(main__Server*))('), c_source
	assert c_source.contains('))(&(s))'), c_source
}

fn test_selfhost_map_value_array_of_pointers_keeps_composite_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {}

struct Pool {
mut:
	idle map[string][]&Item
}

fn (mut p Pool) take(key string) {
	mut list := p.idle[key] or { return }
	_ = list
}

fn main() {}
', 'selfhost_map_array_ptr.v', prefs) or { panic(err) }
	assert c_source.contains('Array_main__Item_ptr *__v_fastc_map_value'), c_source
	assert !c_source.contains('Array_main__Item* *__v_fastc_map_value'), c_source
}

fn test_selfhost_map_lookup_or_return_uses_option_value_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Kind {
	item
}

const labels = {
	Kind.item: "item"
}

fn write(kind Kind) {
	label := labels[kind] or { return }
	println(label)
}

fn main() {
	write(.item)
}
', 'selfhost_map_lookup_or_return.v', prefs) or { panic(err) }
	assert c_source.contains('string *__v_fastc_map_value'), c_source
	assert c_source.contains('*((string *)__v_fastc_option_'), c_source
	assert !c_source.contains('Option __v_fastc_option_0 = (main__labels[kind])'), c_source
}

fn test_selfhost_address_of_map_lookup_or_returns_stored_value_pointer() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	value int
}

fn find_ptr(items map[string]Item, key string) &Item {
	return unsafe { &items[key] or { nil } }
}

fn main() {}
', 'selfhost_map_lookup_address.v', prefs) or { panic(err) }
	assert c_source.contains('Item *__v_fastc_map_value'), c_source
	assert c_source.contains('Item* __v_fastc_box_value = (__v_fastc_map_value)'), c_source
	assert !c_source.contains('&({ Option'), c_source
}

fn test_selfhost_mut_alias_struct_boxes_as_underlying_interface_implementer() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	main_source := 'module main
import transport

interface Reader {
	read() int
}

fn accept(r Reader) {}

fn pass(mut conn transport.Conn) {
	accept(conn)
}

fn main() {}
'
	transport_source := 'module transport

pub struct Base {}
pub type Conn = Base
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'transport/transport.v'
			source: transport_source
			header: fastc_scan_source_header(transport_source, 'transport/transport.v', prefs) or {
				panic(err)
			}
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('(main__Reader){._object=(void*)(conn), ._typ=__v_typeid_transport__Base'), c_source
}

fn test_selfhost_explicit_reference_argument_is_dereferenced_for_value_parameter() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn take(value string) {}

fn forward(value &string) {
	take(value)
}

fn main() {}
', 'selfhost_reference_value_argument.v', prefs) or { panic(err) }
	assert c_source.contains('take(*(value))'), c_source
}

fn test_selfhost_mutable_array_slice_argument_is_not_dereferenced() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn take(mut value []u8) {}

fn forward(mut value []u8) {
	take(mut value[1..])
}

fn main() {}
', 'selfhost_mut_slice_argument.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__array_slice(*(value), 1, value->len)'), c_source
	assert !c_source.contains('*(builtin__array_slice'), c_source
}

fn test_selfhost_c_interface_object_address_uses_pointer_cast() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Logger {
	free()
}

fn object_address(logger &Logger) voidptr {
	return unsafe { &C.main__Logger(logger)._object }
}

fn main() {}
', 'selfhost_c_interface_object.v', prefs) or { panic(err) }
	assert c_source.contains('&(((main__Logger *)(logger))->_object)'), c_source
	assert !c_source.contains('&main__Logger(logger)._object'), c_source
}

fn test_selfhost_sizeof_c_struct_uses_struct_tag() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct C.sockaddr_in6 {
	family u16
}

fn size() int {
	return sizeof(C.sockaddr_in6)
}

fn main() {}
', 'selfhost_sizeof_c_struct.v', prefs) or { panic(err) }
	assert c_source.contains('return sizeof(struct sockaddr_in6);'), c_source
}

fn test_selfhost_as_cast_interface_and_concrete() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

interface Animal {
	sound() int
}

interface Loud {
	sound() int
}

struct Dog {
	volume int
}

fn (d Dog) sound() int {
	return d.volume
}

type Beast = Cat | Dog

struct Cat {
	volume int
}

fn to_loud(a Animal) Loud {
	return a as Loud
}

fn as_dog(b Beast) Dog {
	return b as Dog
}

fn main() {
	_ := to_loud(Dog{ volume: 7 })
	_ := as_dog(Beast(Dog{ volume: 3 }))
}
', 'selfhost_as_cast.v', prefs) or { panic(err) }
	// `a as Loud` (interface -> interface) re-boxes the same object under the target
	// type; `b as Dog` (sum -> concrete) unboxes the stored object.
	assert c_source.contains('._object = __v_fastc_as_src'), c_source
	assert c_source.contains('*((Dog *)__v_fastc_as_src'), c_source
}

fn test_selfhost_as_cast_to_module_qualified_concrete_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	main_source := 'module main
import animals

fn as_dog(value animals.Animal) animals.Dog {
	return value as animals.Dog
}

fn main() {}
'
	animals_source := 'module animals

pub interface Animal {
	sound() int
}

pub struct Dog {}

pub fn (d Dog) sound() int {
	return 1
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'animals/animals.v'
			source: animals_source
			header: fastc_scan_source_header(animals_source, 'animals/animals.v', prefs) or {
				panic(err)
			}
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('*((animals__Dog *)__v_fastc_as_src'), c_source
	assert !c_source.contains(' as animals__Dog'), c_source
}

fn test_selfhost_multiline_boolean_continues_into_unsafe_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn overlaps(x []u8, y []u8) bool {
	return x.len > 0 && y.len > 0 &&
		unsafe { &x[0] <= &y[y.len - 1] && &y[0] <= &x[x.len - 1] }
}

fn main() {}
', 'multiline_unsafe_boolean.v', prefs) or { panic(err) }
	assert !c_source.contains('y.len>0&&)'), c_source
	assert c_source.contains('builtin__array_get(x, 0)'), c_source
	assert c_source.contains('builtin__array_get(y, y.len-1)'), c_source
}

fn test_selfhost_multiline_cast_ignores_semicolon_inside_parentheses() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn word(bytes []u8) u64 {
	return (u64(bytes[0]) << 8) | u64(bytes[1]
	)
}

fn main() {}
', 'multiline_cast_parentheses.v', prefs) or { panic(err) }
	assert c_source.contains('((u64)((*(u8 *)builtin__array_get(bytes, 1))))'), c_source
	assert !c_source.contains('builtin__array_get(bytes, 1);'), c_source
}

fn test_selfhost_channel_send_receive() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	id int
}

struct Pool {
mut:
	items chan Item
}

fn make_pool() Pool {
	ch := chan Item{cap: 2}
	it := Item{ id: 9 }
	ch <- it
	return Pool{
		items: ch
	}
}

fn (mut p Pool) take() !Item {
	return <-p.items or { return error("empty") }
}

fn (mut p Pool) drain() int {
	mut total := 0
	if p.items.closed {
		return total
	}
	mut c := <-p.items or { return total }
	total += c.id
	for _ in 0 .. p.items.len {
		total += 1
	}
	return total
}

fn main() {
	mut p := make_pool()
	got := p.take() or { Item{} }
	total := p.drain() + got.id
	if total > 0 {
		println("nonempty")
	}
}
', 'selfhost_channel.v', prefs) or { panic(err) }
	// Channel send lowers to `try_push` of the value address; receive pops via
	// `try_pop` into an element-typed temp recovered from the `chan Item` field
	// (so `<-p.items` yields `Item`, not the `int` fallback).
	assert c_source.contains('builtin__chan_try_push'), c_source
	assert c_source.contains('builtin__chan_try_pop'), c_source
	assert c_source.contains('builtin__chan_close(ch,(Array_IError){0})'), c_source
	assert c_source.contains('Item __v_fastc_chan_recv'), c_source
	assert !c_source.contains('items.closed'), c_source
}

fn test_selfhost_typed_empty_channel_array() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Waiters {
mut:
	items []chan bool
}

fn new_waiters() Waiters {
	return Waiters{
		items: []chan bool{}
	}
}

fn main() {
	_ := new_waiters()
}
', 'selfhost_channel_array.v', prefs) or { panic(err) }
	assert c_source.contains('Array_chan items'), c_source
	assert c_source.contains('(Array_chan){'), c_source
	assert !c_source.contains('[]chan bool{}'), c_source
}

fn test_selfhost_pthread_rwlock_fallback_follows_includes() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

#include <pthread.h>

fn main() {}
', 'selfhost_pthread_fallback.v', prefs) or { panic(err) }
	include_index := c_source.index('#include <pthread.h>') or { panic(c_source) }
	fallback_index := c_source.index('#ifndef PTHREAD_RWLOCK_PREFER_WRITER_NONRECURSIVE_NP') or {
		panic(c_source)
	}
	assert include_index < fallback_index, c_source
}

fn test_selfhost_is_composite_variant_smartcast() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Param = int | []string

fn describe(p Param) int {
	if p is []string {
		return p.len
	}
	return -1
}

fn main() {
	_ := describe(Param(["a", "b"]))
}
', 'selfhost_is_composite.v', prefs) or { panic(err) }
	// `x is []string` tests/smart-casts a composite (`Array_string`) sum variant via
	// its generated `__v_typeid_` tag, so `p.len` resolves inside the branch.
	assert c_source.contains('__v_typeid_Array_string'), c_source
	assert c_source.contains('Array_string p = *((Array_string *)'), c_source
}

fn test_selfhost_enum_shorthand_after_type_refinement() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Kind {
	one
	two
}

type Value = Kind | string

fn is_one(value Value) bool {
	return value is Kind && value == .one
}

fn main() {
	assert is_one(Value(Kind.one))
}
', 'selfhost_enum_refinement.v', prefs) or { panic(err) }
	// The enum is unboxed only in the short-circuited right operand, and shorthand
	// resolves against the concrete type established by the left operand.
	assert c_source.contains('__v_typeid_Kind'), c_source
	assert c_source.contains('*((Kind *)value._object)'), c_source
	assert c_source.contains('Kind__one'), c_source
	assert !c_source.contains('value==.one'), c_source
}

fn test_selfhost_enum_shorthand_in_smartcast_conjunction() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Op {
	mul
}

struct PrefixExpr {
	op Op
}

type Expr = PrefixExpr | int

fn check(left Expr) bool {
	return left is PrefixExpr && left.op == .mul
}

fn main() {
	_ = check(1)
}
', 'selfhost_smartcast_enum_shorthand.v', prefs) or { panic(err) }
	assert c_source.contains('Op__mul'), c_source
	assert !c_source.contains('== (.mul)'), c_source
}

fn test_selfhost_enum_shorthand_in_parenthesized_smartcast_guard() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Language {
	v
	c
}

struct Ident {
	language Language
	obj int
}

type Expr = Ident | int

struct Arg {
	expr Expr
}

struct Table {}

fn (t &Table) is_interface_smartcast(obj int) bool {
	return obj != 0
}

struct Gen {
	table &Table
}

fn check(g &Gen, arg Arg) bool {
	if arg.expr is Ident && (arg.expr.language == .c || g.table.is_interface_smartcast(arg.expr.obj)) {
		return true
	}
	return false
}

fn main() {
	_ = check(&Gen{}, Arg{})
}
', 'selfhost_smartcast_parenthesized_enum_guard.v', prefs) or { panic(err) }
	assert c_source.contains('Language__c'), c_source
	assert !c_source.contains('language==.c'), c_source
}

fn test_selfhost_method_call_in_smartcast_conjunction() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

enum Flag {
	generic
}

type Type = u64

fn (t Type) has_flag(flag Flag) bool {
	return t & u64(flag) != 0
}

struct StructInit {
	unresolved bool
	typ Type
}

type Expr = StructInit | int

fn check(right &Expr) bool {
	return right is StructInit && right.unresolved && right.typ.has_flag(.generic)
}

fn main() {
	value := Expr(1)
	_ = check(&value)
}
', 'selfhost_smartcast_method_conjunction.v', prefs) or { panic(err) }
	assert c_source.contains('Type_has_flag(((StructInit *)'), c_source
	assert !c_source.contains('right.typ.has_flag'), c_source
}

fn test_selfhost_nested_member_smartcasts_with_mutable_first_subject() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Var {}

type ScopeObject = Var | int

struct Ident {
	obj ScopeObject
}

type Expr = Ident | int

struct Selector {
mut:
	left Expr
}

fn check(mut selector Selector) bool {
	return if mut selector.left is Ident && selector.left.obj is Var {
		true
	} else {
		false
	}
}

fn main() {
	mut selector := Selector{}
	_ = check(mut selector)
}
', 'selfhost_nested_member_smartcasts.v', prefs) or { panic(err) }
	assert c_source.contains('__v_typeid_Ident'), c_source
	assert c_source.contains('__v_typeid_Var'), c_source
	assert !c_source.contains('selector->left.obj is'), c_source
}

fn test_selfhost_flag_define_directive() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

#flag -DGITLY_FASTC_DEFINE

fn main() {
	println("ok")
}
', 'selfhost_flag_define.v', prefs) or { panic(err) }
	// A `#flag -DNAME` preprocessor define is emitted as a `#define` so it is in effect
	// for any C `#include` that follows.
	assert c_source.contains('#define GITLY_FASTC_DEFINE 1'), c_source
}

fn test_selfhost_parallel_option_tuple_decl() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn split_pair(s string) ?(string, string) {
	if s == "" {
		return none
	}
	return s, s
}

fn describe(part string) string {
	mut name, mut val := split_pair(part) or { part, "" }
	return name + val
}

fn main() {
	_ := describe("x")
}
', 'selfhost_parallel_opt_tuple.v', prefs) or { panic(err) }
	// `a, b := opt_tuple() or { x, y }`: declare each target, then on the option`s
	// failure state assign the fallback tuple, else unbox the MultiReturn component.
	assert c_source.contains('.state) {'), c_source
	assert c_source.contains('MultiReturn'), c_source
	assert c_source.contains('.values[1], sizeof('), c_source
}

fn test_selfhost_assign_concrete_value_to_option_local() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn main() {
	mut bytes := ?[]u8(none)
	bytes = [u8(1), 2]
	if value := bytes {
		assert value.len == 2
	}
}
', 'selfhost_option_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('bytes=(Option){.data='), c_source
	assert c_source.contains('sizeof(Array_u8)'), c_source
}

fn test_selfhost_assign_concrete_value_to_option_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	id int
}

struct State {
mut:
	item ?Item
}

fn make_item() !Item {
	return Item{ id: 3 }
}

fn fill(mut state State) ! {
	state.item = make_item()!
}

fn main() {
	mut state := State{}
	fill(mut state) or { panic(err) }
}
', 'selfhost_option_field_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('state->item=(Option){.data='), c_source
	assert c_source.contains('sizeof(Item)'), c_source
}

fn test_selfhost_option_payload_through_if_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Limits {
	a ?u64
	b ?u64
}

fn choose(l Limits, first bool) u64 {
	selected := if first { l.a } else { l.b }
	if value := selected {
		return value
	}
	return 0
}

fn main() {
	_ := choose(Limits{}, true)
}
', 'selfhost_option_if_expression.v', prefs) or { panic(err) }
	assert c_source.contains('Option __v_fastc_if_guard'), c_source
	assert c_source.contains('u64 value = *((u64 *)'), c_source
	assert !c_source.contains('value:=selected'), c_source
}

fn test_selfhost_option_or_through_nested_pointer_member() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Inner {
	value ?u64
}

struct Outer {
	inner &Inner
}

fn read_value(outer &Outer) u64 {
	return outer.inner.value or { u64(0) }
}

fn main() {
	_ := read_value(&Outer{ inner: &Inner{} })
}
', 'selfhost_nested_pointer_option.v', prefs) or { panic(err) }
	assert c_source.contains('outer->inner->value'), c_source
	assert !c_source.contains('outer->inner.value'), c_source
}

fn test_selfhost_option_or_through_pointer_returning_method_member() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct State {
	value ?u64
}

struct Owner {
	state State
}

fn (mut owner Owner) get_state() &State {
	return &owner.state
}

fn read(mut owner Owner) u64 {
	return owner.get_state().value or { u64(0) }
}

fn main() {}
', 'selfhost_method_pointer_option_member.v', prefs) or { panic(err) }
	assert c_source.contains('Owner_get_state(owner)->value'), c_source
	assert !c_source.contains('Owner_get_state(owner).value'), c_source
}

fn test_selfhost_static_method_multi_return_types() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Maker {}

fn Maker.make() !(int, string) {
	return 7, "ok"
}

fn main() {
	number, text := Maker.make()!
	println(number)
	println(text)
}
', 'selfhost_static_multi_return.v', prefs) or { panic(err) }
	assert c_source.contains('int number = (int){0}'), c_source
	assert c_source.contains('string text = (string){0}'), c_source
	assert !c_source.contains('usize number'), c_source
}

fn test_selfhost_explicit_embed_method_call() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Base {
	x int
}

fn (b Base) hello() int {
	return b.x
}

struct Derived {
	Base
	y int
}

fn use(d Derived) int {
	return d.Base.hello()
}

fn main() {
	_ := use(Derived{ Base: Base{ x: 7 }, y: 1 })
}
', 'selfhost_embed_method.v', prefs) or { panic(err) }
	// `d.Base.hello()` accesses the embed by type name (`__embedded_0`) and calls its
	// method on that field — no doubled `__embedded_0`.
	assert c_source.contains('Base_hello(&(d.__embedded_0))') || c_source.contains('Base_hello(d.__embedded_0)'), c_source
	assert !c_source.contains('__embedded_0.__embedded_0'), c_source
}

fn test_selfhost_method_on_propagated_result() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Box {
	v int
}

fn (b Box) doubled() int {
	return b.v * 2
}

fn make_box(x int) !Box {
	return Box{ v: x }
}

fn use_box(x int) !int {
	return make_box(x)!.doubled()
}

fn main() {
	_ := use_box(3) or { 0 }
}
', 'selfhost_bang_method.v', prefs) or { panic(err) }
	// `decode(raw)!.len`: the receiver `decode(raw)!` unwraps the result (propagating
	// its error) before `.len` is taken.
	assert c_source.contains('__v_fastc_option_propagate'), c_source
}

fn test_selfhost_mut_method_on_pointer_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Lock {
mut:
	held int
}

fn (mut l Lock) grab() {
	l.held += 1
}

struct Wrap {
	mu &Lock = unsafe { nil }
}

fn touch(w &Wrap) {
	w.mu.grab()
}

fn main() {
	l := &Lock{ held: 0 }
	touch(&Wrap{ mu: l })
}
', 'selfhost_ptr_mut_method.v', prefs) or { panic(err) }
	// A `mut`-receiver method on a `&Lock` pointer field is valid through the immutable
	// `&Wrap` param (the pointer carries mutable access).
	assert c_source.contains('Lock_grab'), c_source
}

fn test_selfhost_spawn_mut_argument() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Counter {
mut:
	n int
}

fn bump(mut c Counter) {
	c.n += 1
}

fn launch(mut c Counter) {
	t := spawn bump(mut c)
	t.wait()
}

fn main() {
	mut c := Counter{ n: 0 }
	launch(mut c)
}
', 'selfhost_spawn_mut.v', prefs) or { panic(err) }
	// `spawn bump(mut c)`: the `mut` parameter is a pointer, so c is passed by address.
	assert c_source.contains('__v_fastc_spawn_start'), c_source
}

fn test_selfhost_mut_map_option_guard() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Val {
mut:
	x int
}

fn lookup(m map[int]Val) int {
	if mut v := m[1] {
		v.x += 1
		return v.x
	}
	return -1
}

fn main() {
	mut m := map[int]Val{}
	m[1] = Val{ x: 5 }
	_ := lookup(m)
}
', 'selfhost_mut_map_guard.v', prefs) or { panic(err) }
	// `if mut v := m[1] { ... }`: the leading `mut` guard binds `v` to the map value.
	assert c_source.contains('if_guard'), c_source
	assert c_source.contains('.state == 0'), c_source
}

fn test_selfhost_mut_iterate_pointer_array_method() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Stream {
mut:
	failed bool
}

fn (mut s Stream) fail() {
	s.failed = true
}

fn process() int {
	mut above := []&Stream{}
	above << &Stream{ failed: false }
	mut n := 0
	for mut st in above {
		st.fail()
		n += 1
	}
	return n
}

fn main() {
	_ := process()
}
', 'selfhost_ptr_array_iter.v', prefs) or { panic(err) }
	// `[]&Stream` stores `_ptr`-encoded elements; the mut-iteration element type is
	// decoded to `Stream*`, so `st.fail()` resolves to `Stream_fail`, not the invalid
	// `Stream_ptr_fail`.
	assert c_source.contains('Stream* st = ((Stream* *)'), c_source
	assert !c_source.contains('Stream* *st = &(((Stream* *)'), c_source
	assert c_source.contains('Stream_fail'), c_source
	assert !c_source.contains('Stream_ptr_fail'), c_source
}

fn test_selfhost_fixed_array_field_iteration_uses_element_pointer() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	value int
}

struct Holder {
	items [4]Item
}

fn total(holder Holder) int {
	mut sum := 0
	for _, item in holder.items {
		sum += item.value
	}
	return sum
}

fn main() {
	_ := total(Holder{})
}
', 'fixed_array_field_iteration.v', prefs) or { panic(err) }
	assert c_source.contains('Item *__v_fastc_collection_'), c_source
	assert c_source.contains('= &((holder.items)[0]);'), c_source
	assert !c_source.contains('__typeof__((holder.items)) __v_fastc_collection_'), c_source
	assert !c_source.contains('__v_fastc_collection_0.data'), c_source
}

fn test_selfhost_fixed_array_struct_field_copy_uses_memcpy() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Holder {
	items [4]int
	len   int
}

fn copy(source Holder) Holder {
	return Holder{
		items: source.items
		len: source.len
	}
}

fn main() {
	_ := copy(Holder{})
}
', 'fixed_array_struct_field_copy.v', prefs) or { panic(err) }
	assert c_source.contains('memcpy(__v_fastc_struct_fixed.items, source.items, sizeof(__v_fastc_struct_fixed.items));'), c_source

	assert !c_source.contains('__typeof__((source.items)) __v_fastc_struct_field_'), c_source
}

fn test_selfhost_panic_argument_uses_string_concatenation_lowering() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn maybe() ?int {
	return none
}

fn result() !int {
	return error("no")
}

fn fail(message string) int {
	return maybe() or {
		panic("failed: " + message)
	}
}

fn fail_with_error() int {
	return result() or {
		panic(err)
	}
}

fn main() {
	_ := fail("now")
	_ := fail_with_error()
}
', 'panic_string_concatenation.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__panic(builtin__string_plus(_S("failed: "),message))'), c_source
	assert c_source.contains('builtin__panic(builtin__IError_msg(err))'), c_source
	assert !c_source.contains('_S("failed: ")+message'), c_source
}

fn test_selfhost_map_guard_function_alias_retains_result_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Handler = fn (value int) !string

fn call(handlers map[string]Handler) !string {
	if handler := handlers["test"] {
		return handler(1)
	}
	return error("missing")
}

fn main() {
	_ := call(map[string]Handler{})
}
', 'map_guard_function_alias.v', prefs) or { panic(err) }
	assert c_source.contains('Handler handler = *((Handler *)__v_fastc_if_guard_'), c_source
	assert c_source.contains('return handler(1);'), c_source
	assert !c_source.contains('sizeof());'), c_source
}

fn test_selfhost_function_field_retains_result_payload_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Server {
	handler fn (int) !string
}

fn call(server Server) string {
	return server.handler(1) or {
		return "failed"
	}
}

fn main() {
	_ := call(Server{})
}
', 'function_field_result_payload.v', prefs) or { panic(err) }
	assert c_source.contains('*((string *)__v_fastc_option_'), c_source
	assert !c_source.contains('return; } 0;'), c_source
}

fn test_selfhost_optional_function_field_guard_binds_typed_callback() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Config {
	callback ?fn (int) !string
}

fn call(config Config) !string {
	if callback := config.callback {
		return callback(1)
	}
	return error("missing")
}

fn main() {
	_ := call(Config{})
}
', 'optional_function_field_guard.v', prefs) or { panic(err) }
	assert c_source.contains('config.callback != NULL'), c_source
	assert c_source.contains('Option (*callback)(int)'), c_source
	assert !c_source.contains('Option __v_fastc_if_guard_'), c_source
}

fn test_selfhost_derived_overloaded_comparisons() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Moment {
	value int
}

fn (left Moment) < (right Moment) bool {
	return left.value < right.value
}

fn (left Moment) == (right Moment) bool {
	return left.value == right.value
}

fn compare(left Moment, right Moment) {
	println(left < right)
	println(left > right)
	println(left <= right)
	println(left >= right)
	println(left == right)
	println(left != right)
}

fn main() {
	compare(Moment{ value: 1 }, Moment{ value: 2 })
}
', 'derived_overloaded_comparisons.v', prefs) or { panic(err) }
	assert c_source.contains('Moment_lt(left,right)'), c_source
	assert c_source.contains('Moment_lt(right,left)'), c_source
	assert c_source.contains('!(Moment_lt(right,left))'), c_source
	assert c_source.contains('!(Moment_lt(left,right))'), c_source
	assert c_source.contains('Moment_eq(left,right)'), c_source
	assert c_source.contains('!(Moment_eq(left,right))'), c_source
}

fn test_selfhost_overloaded_operator_retains_private_helper_dependency() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Count {
	value int
}

fn (left Count) + (right Count) Count {
	return left.combine(right)
}

fn (left Count) combine(right Count) Count {
	return Count{
		value: left.value + right.value
	}
}

fn main() {
	_ := Count{ value: 1 } + Count{ value: 2 }
}
', 'overloaded_operator_private_helper.v', prefs) or { panic(err) }
	assert c_source.contains('main__Count_combine'), c_source
	assert c_source.contains('return main__Count_combine(left,right);'), c_source
}

fn test_selfhost_compound_assignment_uses_overloaded_operator() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Count {
	value int
}

fn (left Count) * (right Count) Count {
	return Count{
		value: left.value * right.value
	}
}

fn main() {
	mut value := Count{ value: 2 }
	value *= Count{ value: 3 }
}
', 'overloaded_compound_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('Count *__v_fastc_overloaded_assignment_target = &(value)'), c_source
	assert c_source.contains('main__Count_mul(*__v_fastc_overloaded_assignment_target'), c_source
	assert !c_source.contains('value*='), c_source
}

fn test_selfhost_nested_overloaded_result_is_not_auto_dereferenced() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Count {
	value int
}

fn (left Count) * (right Count) Count {
	return Count{ value: left.value * right.value }
}

fn (left Count) + (right Count) Count {
	return Count{ value: left.value + right.value }
}

fn update(mut result Count, factor Count, extra Count) {
	result = (result * factor) + extra
}

fn main() {
	mut result := Count{ value: 2 }
	update(mut result, Count{ value: 3 }, Count{ value: 1 })
}
', 'nested_overloaded_mut_receiver.v', prefs) or { panic(err) }
	assert c_source.contains('main__Count_plus((main__Count_mul(*(result),factor)),extra)'), c_source
	assert !c_source.contains('*(main__Count_mul'), c_source
}

fn test_selfhost_overloaded_expression_as_method_receiver() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Count {
	value int
}

fn (left Count) + (right Count) Count {
	return Count{ value: left.value + right.value }
}

fn (value Count) scaled(factor int) Count {
	return Count{ value: value.value * factor }
}

fn calculate(left Count, right Count) Count {
	return (left + right).scaled(2)
}

fn main() {
	_ := calculate(Count{ value: 1 }, Count{ value: 2 })
}
', 'overloaded_expression_method_receiver.v', prefs) or { panic(err) }
	assert c_source.contains('main__Count_scaled((main__Count_plus(left,right)),2)'), c_source
	assert !c_source.contains('main__Count_plus(left,right)+'), c_source
}

fn test_selfhost_dynamic_array_equality_is_elementwise() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn same(left []u64, right []u64) bool {
	return left == right
}

fn different(left []string, right []string) bool {
	return left != right
}

fn main() {
	_ := same([u64(1)], [u64(1)])
	_ := different(["a"], ["b"])
}
', 'dynamic_array_equality.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_array_eq_left.len == __v_fastc_array_eq_right.len'), c_source
	assert c_source.contains('((u64 *)__v_fastc_array_eq_left.data)'), c_source
	assert c_source.contains('builtin__string_eq(((string *)__v_fastc_array_eq_left.data)'), c_source
	assert c_source.contains('!__v_fastc_array_equal'), c_source
	assert !c_source.contains('left==right'), c_source
}

fn test_selfhost_fixed_array_field_equality_is_elementwise() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Preferred {
	ipv4 [4]u8
}

fn missing(value Preferred) bool {
	return value.ipv4 == [4]u8{}
}

fn main() {}
', 'fixed_array_field_equality.v', prefs) or { panic(err) }
	assert c_source.contains('u8 *__v_fastc_array_eq_left = (u8 *)(value.ipv4)'), c_source
	assert c_source.contains('__v_fastc_array_eq_index < 4'), c_source
	assert !c_source.contains('==[4]u8{}'), c_source
}

fn test_selfhost_nested_fixed_array_field_assignment_uses_raw_storage() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Grid {
mut:
	values [2][3]int
}

fn set(mut grid Grid) {
	grid.values[0][1] = 7
}

fn main() {}
', 'nested_fixed_array_field_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('grid->values)[builtin__v_fixed_index(0, 2)]'), c_source
	assert c_source.contains('builtin__v_fixed_index(1, 3)]'), c_source
	assert !c_source.contains('grid->values)[builtin__v_fixed_index(0, 2)]).data'), c_source
}

fn test_selfhost_constant_rewrite_does_not_rename_same_named_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

const p = [2]int{init: 3}

struct Pair {
	p [2]int
}

fn total(value Pair) int {
	return value.p[0] + p[0]
}

fn main() {}
', 'constant_and_field_name_collision.v', prefs) or { panic(err) }
	assert c_source.contains('value.p'), c_source
	assert c_source.contains('main__p'), c_source
	assert !c_source.contains('value.main__p'), c_source
}

fn test_selfhost_erased_generic_compound_assignment_uses_stub() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn sum[T](values []T) T {
	mut result := values[0]
	result += values[1]
	return result
}

fn main() {}
', 'erased_generic_compound_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('voidptr main__sum(Array_voidptr values)'), c_source
	assert c_source.contains('return (voidptr){0};'), c_source
	assert !c_source.contains('result+='), c_source
}

fn test_failed_generic_placeholder_block_unwinds_local_scope() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := 'mut local := result\nlocal += result\n}'
	mut file_set := token.FileSet.new()
	mut file := file_set.add_file('failed_generic_scope.v', source.len)
	file.index_lines_without_digest(source)
	mut g := Parser{
		prefs: prefs
		selfhost: true
		in_generic_placeholder: true
		locals: {
			'result': FastcLocal{
				is_mut: true
				typ: 'voidptr'
			}
		}
		s: scanner.new_scanner(prefs, .normal)
		out: strings.new_builder(64)
		statement_reachable: true
	}
	for _ in 0 .. 3 {
		out_checkpoint := g.out.len
		g.s.init(file, source)
		g.next()
		mut body_end := g.s
		mut body_depth := 1
		for body_depth > 0 {
			body_tok := body_end.scan()
			if body_tok == .eof {
				break
			}
			if body_tok == .lcbr {
				body_depth++
			} else if body_tok == .rcbr {
				body_depth--
			}
		}
		assert g.parse_generic_body_or_stub('voidptr', out_checkpoint, 0, body_end, false)
		assert g.local_scope_depth == 0
		assert g.local_scope_changes.len == 0
		assert 'local' !in g.locals
		assert g.locals['result'].typ == 'voidptr'
	}
}

fn test_selfhost_erased_generic_type_reflection_uses_stub() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn is_pointer[T]() bool {
	return T.name[0] == `&`
}

fn main() {}
', 'erased_generic_type_reflection.v', prefs) or { panic(err) }
	assert c_source.contains('bool main__is_pointer(void)'), c_source
	assert c_source.contains('return (bool){0};'), c_source
	assert !c_source.contains('T.name'), c_source
}

fn test_selfhost_monomorphized_mut_generic_parameter_keeps_pointer() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Decoder {}

fn (mut decoder Decoder) set[T](mut value T) {
	value = value
}

fn (mut decoder Decoder) use(mut value string) {
	decoder.set[string](mut value)
}

fn main() {}
', 'monomorphized_mut_generic_parameter.v', prefs) or { panic(err) }
	assert c_source.contains('Decoder_set_mono_string(Decoder* decoder, string* value)'), c_source
	assert c_source.contains('Decoder_set_mono_string(decoder,value)'), c_source
	assert !c_source.contains('Decoder_set_mono_string(decoder,*(value))'), c_source
}

fn test_selfhost_for_key_mut_value_map() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Val {
mut:
	x int
}

fn adjust(mut m map[int]&Val) {
	for _, mut v in m {
		v.x += 1
	}
}

fn main() {
	mut m := map[int]&Val{}
	m[1] = &Val{ x: 0 }
	adjust(mut m)
}
', 'selfhost_for_mut_value.v', prefs) or { panic(err) }
	// `for _, mut v in m`: the `mut` value binds; the map value is a pointer so
	// mutations through `v` persist. Compiles via `builtin__map_values`.
	assert c_source.contains('builtin__map_values'), c_source
}

fn test_selfhost_fixed_array_slice() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn take(n int) int {
	mut msg := [8]u8{}
	msg[0] = 104
	s := msg[..n]
	return s.len
}

fn main() {
	_ := take(2)
}
', 'selfhost_fixed_slice.v', prefs) or { panic(err) }
	// Slicing a fixed array yields a dynamic `Array_u8` copied from the element range,
	// not the `FixedArray_...` type, so `.len` resolves on the resulting array.
	assert c_source.contains('builtin__new_array_from_c_array'), c_source
	assert c_source.contains('Array_u8'), c_source
}

fn test_selfhost_fixed_array_struct_field_slice_uses_raw_field_storage() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Address {
	bytes [16]u8
}

fn slice(address Address) []u8 {
	return address.bytes[..]
}

fn main() {}
', 'selfhost_fixed_field_slice.v', prefs) or { panic(err) }
	assert c_source.contains('&(((address.bytes))[0])'), c_source
	assert !c_source.contains('__typeof__((address.bytes)) __v_fastc_slice_receiver'), c_source
}

fn test_selfhost_embedded_union_fixed_array_slice_promotes_member() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Unix {
	path [8]char
}

union Data {
	Unix
}

struct Address {
	data Data
}

fn path_data(address Address) byteptr {
	return address.data.Unix.path[..].data
}

fn main() {}
', 'selfhost_embedded_union_fixed_slice.v', prefs) or { panic(err) }
	assert c_source.contains('address.data.__embedded_0.path'), c_source
	assert !c_source.contains('address.data.Unix.path'), c_source
	assert c_source.contains('typedef array Array_char;'), c_source
}

fn test_selfhost_keyword_named_function_call_in_or_expression() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn select(value int) !bool {
	return value > 0
}

fn ready() !bool {
	value := select(1) or { return err }
	return value
}

fn main() {}
', 'selfhost_keyword_function_call.v', prefs) or { panic(err) }
	assert c_source.contains('select(1)'), c_source
	assert c_source.contains('*((bool *)'), c_source
}

fn test_selfhost_if_expression_panic_arm_uses_sibling_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Stamp {
	n int
}

fn choose(value int) Stamp {
	result := if value == 0 {
		Stamp{ n: 1 }
	} else if value < 0 {
		panic("negative")
	} else {
		Stamp{ n: 2 }
	}
	return result
}

fn main() {}
', 'selfhost_if_panic_arm.v', prefs) or { panic(err) }
	assert c_source.contains('(Stamp){0}'), c_source
	assert c_source.contains('builtin__panic(_S("negative"))'), c_source
}

fn test_selfhost_method_on_parenthesized_or() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Box {
	v int
}

fn (b Box) doubled() int {
	return b.v * 2
}

fn make_box(x int) ?Box {
	if x < 0 {
		return none
	}
	return Box{ v: x }
}

fn use_box(x int) int {
	return (make_box(x) or { Box{ v: 0 } }).doubled()
}

fn main() {
	_ := use_box(5)
}
', 'selfhost_paren_or_method.v', prefs) or { panic(err) }
	// `(make_box(x) or { … }).doubled()`: the grouping parens leave the option expression
	// balanced (`Option t = (make_box(x))`, not `((make_box(x))`) and the grouped operand`s
	// value type (Box) flows into the result token so the method binds to the unwrapped Box.
	assert c_source.contains('Box_doubled'), c_source
	assert c_source.contains('__v_fastc_option'), c_source
	assert !c_source.contains('= ((make_box'), c_source
}

fn test_selfhost_parenthesized_or_inside_boolean_comparison() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn maybe_role() ?string {
	return "admin"
}

fn can_admin(logged_in bool) bool {
	return logged_in && (maybe_role() or { "" }) == "admin"
}

fn main() {
	_ := can_admin(true)
}
', 'selfhost_parenthesized_or_comparison.v', prefs) or { panic(err) }
	// The synthetic option-unwrapping atom must retain its full statement expression
	// when the surrounding string equality re-renders the comparison operands.
	assert c_source.contains('Option __v_fastc_option_'), c_source
	assert c_source.contains('= (maybe_role())'), c_source
	assert c_source.contains('builtin__string_eq'), c_source
}

fn test_selfhost_map_lookup_inside_arithmetic_assignment() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn increase(mut counts map[string]int, key string, amount int) {
	counts[key] = counts[key] + amount
}

fn main() {
	mut counts := map[string]int{}
	increase(mut counts, "v", 2)
}
', 'selfhost_map_lookup_arithmetic.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__map_get_check'), c_source
	assert c_source.contains('builtin__map_set'), c_source
	assert !c_source.contains('counts[key]+amount'), c_source
}

fn test_selfhost_map_assignment_keeps_overloaded_operation_in_value() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Text {
	value string
}

fn (left Text) + (right Text) Text {
	return Text{ value: left.value + right.value }
}

fn append_value(mut values map[string]Text, key string, suffix Text) {
	previous := values[key] or { Text{} }
	values[key] = previous + suffix
}

fn main() {
	mut values := map[string]Text{}
	append_value(mut values, "compiler", Text{ value: " initializer" })
}
', 'selfhost_map_assignment_overloaded_operation.v', prefs) or { panic(err) }
	assert c_source.contains('Text __v_fastc_map_value = (Text_plus(previous,suffix))'), c_source
	assert !c_source.contains('Text_plus(({ string __v_fastc_map_key'), c_source
}

fn test_selfhost_map_assignment_with_propagated_result() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {
	value int
}

fn load_item() !Item {
	return Item{ value: 42 }
}

fn insert(mut items map[string]Item) ! {
	items[\'answer\'] = load_item()!
}

fn main() {
	mut items := map[string]Item{}
	insert(mut items) or { panic(err) }
}
', 'selfhost_map_assignment_propagated_result.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__map_set'), c_source
	assert c_source.contains('__v_fastc_option_propagate'), c_source
	assert !c_source.contains('items[_S("answer")]'), c_source
}

fn test_selfhost_nested_map_assignment_initializes_addressable_inner_map() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn insert(mut values map[string]map[string]string, group string, key string, value string) {
	values[group][key] = value
}

fn main() {
	mut values := map[string]map[string]string{}
	insert(mut values, "group", "key", "value")
}
', 'selfhost_nested_map_assignment.v', prefs) or { panic(err) }
	assert c_source.contains('Map_string_string *__v_fastc_nested_map_value'), c_source
	assert c_source.contains('builtin__new_map(sizeof(string), sizeof(string)'), c_source
	assert c_source.contains('builtin__map_set((map *)({'), c_source
	assert !c_source.contains('&values[group]'), c_source
}

fn test_selfhost_array_any_binds_implicit_it() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn has_ignored(parts []string, ignored []string) bool {
	return parts.any(ignored.contains(it))
}

fn main() {
	_ := has_ignored(["src"], ["thirdparty"])
}
', 'selfhost_array_any.v', prefs) or { panic(err) }
	assert c_source.contains('string it ='), c_source
	assert c_source.contains('bool __v_fastc_mapped_'), c_source
	assert !c_source.contains('builtin__array_any'), c_source
}

fn test_selfhost_print_uses_alias_str_method() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Duration = i64

fn (d Duration) str() string {
	return "elapsed"
}

fn elapsed() Duration {
	return Duration(1)
}

fn main() {
	println(elapsed())
}
', 'selfhost_print_alias_str.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__println(Duration_str(elapsed()))'), c_source
}

fn test_selfhost_method_on_arithmetic_with_nested_or() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn maybe_number() ?int {
	return 2
}

fn (n int) doubled() int {
	return n * 2
}

fn use_number() int {
	return (100 + (1 + maybe_number() or { 0 })).doubled()
}

fn main() {
	_ := use_number()
}
', 'selfhost_arithmetic_or_method.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_option'), c_source
	assert c_source.contains('100+('), c_source
	assert !c_source.contains('.doubled()'), c_source
}

fn test_selfhost_pointer_struct_with_or_inside_call_argument() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Params {
	value ?u64
}

struct Conn {
	value u64
}

fn take(value u64) u64 {
	return value
}

fn (mut c Conn) use() {}

fn run(params Params) {
	mut c := &Conn{
		value: take(params.value or { u64(0) })
	}
	c.use()
}

fn main() {
	run(Params{})
}
', 'selfhost_pointer_struct_or_call.v', prefs) or { panic(err) }
	assert c_source.contains('(Conn*)v_fastc_interface_box'), c_source
	assert c_source.contains('Conn_use(c)'), c_source
	assert c_source.contains('take(({ Option __v_fastc_option_'), c_source
}

fn test_selfhost_method_on_parenthesized_or_in_struct_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct S {
	v int
}

fn (s S) plus() S {
	return S{ v: s.v + 1 }
}

fn mk(x int) ?S {
	if x < 0 {
		return none
	}
	return S{ v: x }
}

struct Config {
	a S
	b int
}

fn build(x int, y int) Config {
	return Config{
		a:      (mk(x) or { S{ v: 0 } }).plus()
		b:      y
	}
}

fn main() {
	_ := build(5, 9)
}
', 'selfhost_paren_or_method_struct.v', prefs) or { panic(err) }
	// `Config{ a: (mk(x) or { … }).plus(), b: y }`: the grouping parens around the option
	// are absorbed so the method binds to the unwrapped S, and the struct keeps rendering
	// the trailing `.b` field (no unmatched `)` trailing the or-result token).
	assert c_source.contains('S_plus'), c_source
	assert c_source.contains('(Config){.a='), c_source
	assert c_source.contains('.b='), c_source
}

fn test_selfhost_diverging_or_then_binary_operator() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn maybe(x int) ?int {
	if x < 0 {
		return none
	}
	return x
}

fn use_it(x int) int {
	v := maybe(x) or { return -1 } + 10
	return v
}

fn main() {
	_ := use_it(5)
}
', 'selfhost_or_binop.v', prefs) or { panic(err) }
	// `maybe(x) or { return -1 } + 10`: the diverging or-block returns on failure, and the
	// `+ 10` binds to the unwrapped success value rather than orphaning as its own statement.
	assert c_source.contains('__v_fastc_option'), c_source
	assert c_source.contains('+ 10') || c_source.contains('+10'), c_source
}

fn test_selfhost_option_if_guard_binds_err_in_else_branch() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn maybe() !int {
	return error("no")
}

fn use_it() string {
	if value := maybe() {
		return value.str()
	} else {
		return err.msg()
	}
}

fn main() {
	_ := use_it()
}
', 'selfhost_option_if_guard_err.v', prefs) or { panic(err) }
	assert c_source.contains('IError err = __v_fastc_if_guard_'), c_source
}

fn test_selfhost_multi_return_macro_uses_collision_safe_temporaries() {
	assert c_selfhost_preamble.contains('__v_fastc_multi_value')
	assert !c_selfhost_preamble.contains('__typeof__(expression) value =')
}

fn test_selfhost_mut_guard_on_option_struct_field() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Conn {
	id int
}

fn (c Conn) close() int {
	return c.id
}

struct H {
mut:
	vsc ?Conn
}

fn (mut h H) shut() int {
	if mut v := h.vsc {
		return v.close()
	}
	return -1
}

fn main() {
	mut h := H{}
	_ := h.shut()
}
', 'selfhost_option_field_guard.v', prefs) or { panic(err) }
	// `if mut v := h.vsc {` where `vsc ?Conn`: the guard local `v` binds to the field`s
	// wrapped value type (Conn) recovered from FastcStructField.option_value_type, so
	// `v.close()` resolves to Conn_close rather than defaulting the receiver to int.
	assert c_source.contains('Conn v = *((Conn *)'), c_source
	assert c_source.contains('Conn_close(v)'), c_source
}

fn test_selfhost_option_field_local_retains_guard_payload_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Stream {
	id ?u64
}

fn remove(stream Stream) u64 {
	stream_id := stream.id
	if id := stream_id {
		return id
	}
	return 0
}

fn main() {
	_ := remove(Stream{})
}
', 'option_field_local_guard.v', prefs) or { panic(err) }
	assert c_source.contains('Option __v_fastc_if_guard_'), c_source
	assert c_source.contains('u64 id = *((u64 *)__v_fastc_if_guard_'), c_source
	assert !c_source.contains('if (id:=stream_id)'), c_source
}

fn test_selfhost_explicit_option_casts() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn value_or_zero(present bool) u64 {
	mut value := ?u64(none)
	if present {
		value = ?u64(7)
	}
	if unwrapped := value {
		return unwrapped
	}
	return 0
}

fn main() {
	_ := value_or_zero(true)
}
', 'explicit_option_casts.v', prefs) or { panic(err) }
	assert c_source.contains('(Option){.state=2}'), c_source
	assert c_source.contains('(Option){.data='), c_source
	assert c_source.contains('u64 unwrapped = *((u64 *)__v_fastc_if_guard_'), c_source
	assert !c_source.contains('?((u64)'), c_source
}

fn test_selfhost_concrete_argument_is_lifted_to_option() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn take(value ?[]u8) int {
	return 1
}

fn main() {
	_ := take([u8(1), 2])
	_ := take(none)
}
', 'option_argument_lift.v', prefs) or { panic(err) }
	assert c_source.contains('take((Option){.data='), c_source
	assert c_source.contains('sizeof(Array_u8)'), c_source
	assert c_source.contains('take((Option){.state=2})'), c_source
}

fn test_selfhost_option_returning_method_call_compared_with_none() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Checker {}

fn (c &Checker) find(name string, values []string) ?int {
	_ = c
	_ = name
	_ = values
	return none
}

fn has_value(c &Checker) bool {
	return c.find("value", []string{}) != none
}

fn has_named_value(c &Checker, name string) bool {
	return name.len > 0 && c.find(name, []string{}) != none
}

fn main() {
	checker := &Checker{}
	_ := has_value(checker)
	_ := has_named_value(checker, "value")
}
', 'option_method_call_none_comparison.v', prefs) or { panic(err) }
	assert c_source.contains(').state != 2)'), c_source
	assert !c_source.contains('!= ((Option){.state=2})'), c_source
	assert !c_source.contains('v_fastc_box_value'), c_source
}

fn test_selfhost_anonymous_function_is_rejected() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	if _ := generate('module main

fn apply(f fn (int) int, x int) int {
	return f(x)
}

fn main() {
	g := fn (x int) int {
		return x + 1
	}
	_ := apply(g, 5)
}
', 'selfhost_anon_fn.v', prefs) {
		assert false, 'function literal unexpectedly compiled'
	} else {
		message = err.msg()
	}
	assert message.contains('function literals and closures'), message
}

fn test_selfhost_c_struct_keyword_field_name() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct C.Node {
	next &C.Node
	type int
}

fn main() {
	_ := 0
}
', 'selfhost_c_struct_keyword_field.v', prefs) or { panic(err) }
	// A C struct field named with a V keyword (`type int`, as in `C.cJSON`) is accepted;
	// the field name is a plain C identifier.
	assert c_source.len > 0, 'expected generated C'
}

fn test_selfhost_lowercase_c_struct_pointer_cast_keeps_member_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct C.node {
	value int
	next  &C.node
}

fn main() {
	results := &C.node(unsafe { nil })
	for result := unsafe { results }; result != unsafe { nil }; result = result.next {
		_ := result.value.str()
	}
}
', 'selfhost_lowercase_c_struct_pointer_cast.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__int_str(result->value)'), c_source
}

fn test_selfhost_c_pointer_cast_member_does_not_leak_pointer_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct C.Detail {
	level u32
}

fn main() {
	raw := voidptr(0)
	level := unsafe { &C.Detail(raw) }.level
	detail := unsafe { &C.Detail(raw) }
	println(level)
	_ = detail
}
', 'selfhost_c_pointer_cast_member_type.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__u32_str(level)'), c_source
	assert !c_source.contains('builtin__u32_str(*(level))'), c_source
	assert c_source.contains('Detail*)(raw)'), c_source
	assert !c_source.contains('Detail*)(*((Detail* *)(raw)))'), c_source
}

fn test_selfhost_composite_receiver_method_name() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

type Val = int | string

fn (v Val) label() int {
	return 1
}

fn (v []Val) label() int {
	return 2
}

fn main() {
	v := Val(5)
	mut arr := []Val{}
	arr << Val(6)
	_ := v.label()
	_ := arr.label()
}
', 'selfhost_composite_method.v', prefs) or { panic(err) }
	// A method on `[]Val` must not share the C name of the method on `Val` (both would
	// collapse to `Val_label` → C redefinition); the composite keeps its `Array_` prefix.
	assert c_source.contains('Array_Val_label'), c_source
	assert !c_source.contains('Array_Val_label(Val '), c_source
}

fn test_selfhost_match_range_case() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn classify(b u8) int {
	return match b {
		32...126 { 1 }
		else { 0 }
	}
}

fn main() {
	_ := classify(65)
}
', 'selfhost_match_range.v', prefs) or { panic(err) }
	// A `32...126` match range lowers to a `>= 32 && <= 126` comparison, not the invalid
	// C literal `== (32...126)`.
	assert c_source.contains('>= (32)') && c_source.contains('<= (126)'), c_source
	assert !c_source.contains('32...126'), c_source
}

fn test_selfhost_explicit_deref_of_mut_receiver() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Box {
	n int
}

fn (mut b Box) grab() Box {
	r := *b
	return r
}

fn main() {
	mut x := Box{
		n: 5
	}
	_ := x.grab()
}
', 'selfhost_deref_receiver.v', prefs) or { panic(err) }
	// `*b` where `b` is a `mut` receiver (C `Box*`) is a single deref `*(b)`, not the
	// double `*(*(b))` (auto-deref stacked on the explicit `*`).
	assert !c_source.contains('*(*(b)))'), c_source
}

fn test_selfhost_generic_method_monomorphization() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Enc {
mut:
	n int
}

fn (mut e Enc) put[T](val T) {
	e.n++
}

fn main() {
	mut e := Enc{}
	e.put(5)
	e.put(true)
	_ := e.n
}
', 'selfhost_generic_method.v', prefs) or { panic(err) }
	// A method with its own type parameter (`put[T]`) is monomorphized per call type into
	// `Enc_put_mono_int` / `Enc_put_mono_bool` (not left as a single `voidptr`-param method
	// whose call would pass `&(literal)`).
	assert c_source.contains('Enc_put_mono_int'), c_source
	assert c_source.contains('Enc_put_mono_bool'), c_source
}

fn test_selfhost_nested_generic_fixpoint() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn inner[T](x T) T {
	return x
}

fn outer[T](x T) T {
	return inner(x)
}

fn main() {
	r := outer(5)
	_ := r
}
', 'selfhost_nested_generic.v', prefs) or { panic(err) }
	// A generic call INSIDE a generic body (`outer[T]` calls `inner(x)`) is only concrete
	// in the emitted instance (`outer_mono_int` calls `inner(x)` with `x int`); the
	// monomorphizer`s fixpoint discovers `inner[int]` on the next pass.
	assert c_source.contains('outer_mono_int'), c_source
	assert c_source.contains('inner_mono_int'), c_source
}

fn test_selfhost_nested_explicit_generic_rewrite_is_emitted_in_outer_copy() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn helper[T](value T) T {
	return value
}

fn outer[T](value T) int {
	_ = value
	return helper[int](1)
}

fn main() {
	_ := outer(true)
}
', 'selfhost_nested_explicit_generic.v', prefs) or { panic(err) }
	assert c_source.contains('int helper_mono_int(int value)'), c_source
	assert c_source.contains('return helper_mono_int(1);'), c_source
	assert !c_source.contains('helper[int]'), c_source
}

fn test_selfhost_comptime_unaliased_typ() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn kind[T](val T) int {
	\$if T.unaliased_typ is int {
		return 1
	} \$else {
		return 0
	}
}

fn main() {
	_ := kind(5)
	_ := kind(true)
}
', 'selfhost_unaliased_typ.v', prefs) or { panic(err) }
	// `$if T.unaliased_typ is int` (json2 encoder dispatch) resolves at comptime: the int
	// instance takes the `1` branch, the bool instance the `0` branch.
	assert c_source.contains('kind_mono_int'), c_source
	assert c_source.contains('kind_mono_bool'), c_source
}

fn test_selfhost_comptime_type_indirections() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Item {}

fn kind[T](val T) int {
	\$if T.indirections == 0 {
		return 1
	} \$else {
		return 0
	}
}

fn main() {
	item := Item{}
	_ := kind(item)
}
', 'selfhost_type_indirections.v', prefs) or { panic(err) }
	assert c_source.contains('kind_mono_Item'), c_source
}

fn test_selfhost_comptime_type_match_statement() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn assign[T](mut value T) {
	\$match T.unaliased_typ {
		int { value = 7 }
		string { value = "wrong" }
		\$else { value = value }
	}
}

fn main() {
	mut value := 0
	assign(mut value)
}
', 'selfhost_comptime_type_match.v', prefs) or { panic(err) }
	assert c_source.contains('value=7'), c_source
	assert !c_source.contains('_S("wrong")'), c_source
}

fn test_selfhost_comptime_match_subject_named_shared() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn main() {
	shared := 1
	\$match shared {
		int { println("int") }
		\$else { println("other") }
	}
}
', 'selfhost_comptime_match_shared.v', prefs) or { panic(err) }
	assert c_source.contains('println(_S("int"))'), c_source
	assert !c_source.contains('println(_S("other"))'), c_source
}

fn test_selfhost_on_demand_comptime_recursion() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Point {
	x int
	y int
}

struct Enc {
mut:
	out int
}

fn (mut e Enc) ev[T](val T) {
	\$if T.unaliased_typ is int {
		e.out++
	} \$else {
		\$for field in T.fields {
			e.ev(val.\$(field.name))
		}
	}
}

fn enc[T](val T) int {
	mut e := Enc{}
	e.ev(val)
	return e.out
}

fn main() {
	p := Point{
		x: 1
		y: 2
	}
	_ := enc(p)
}
', 'selfhost_on_demand_recursion.v', prefs) or { panic(err) }
	// A generic method (`ev[T]`) whose body recurses through a comptime `$for` is
	// monomorphized ON DEMAND at parse time: `ev[Point]` (from `enc[Point]`s direct call)
	// then, via the `$for`-unrolled `ev(val.x)`, `ev[int]`. Both instances are emitted.
	assert c_source.contains('Enc_ev_mono_Point'), c_source
	assert c_source.contains('Enc_ev_mono_int'), c_source
	assert !c_source.contains('Enc_ev_mono_voidptr'), c_source
}

fn test_selfhost_on_demand_generic_is_deduplicated_across_source_files() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	util_source := 'module util

pub fn identity[T](value T) T {
	return value
}
'
	first_source := 'module first
import util

pub fn first() string {
	return util.identity[string]("first")
}
'
	main_source := 'module main
import first
import util

fn main() {
	_ := first.first()
	_ := util.identity[string]("main")
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'util/util.v'
			source: util_source
			header: fastc_scan_source_header(util_source, 'util/util.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'first/first.v'
			source: first_source
			header: fastc_scan_source_header(first_source, 'first/first.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	definition := 'string util__identity_mono_string(string value) {'
	assert c_source.count(definition) == 1, c_source
}

fn test_selfhost_on_demand_generic_specializes_composite_return_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	util_source := 'module util

pub fn wrap[T](value T) []T {
	return [value]
}

pub fn maybe[T](value T) ?[]T {
	return [value]
}

pub fn lookup[T](value T) map[string]T {
	return {
		"value": value
	}
}

pub fn pair[T](value T) (T, []T) {
	return value, [value]
}

pub struct Wrapper {}

pub fn (wrapper Wrapper) wrap_method[T](value T) []T {
	_ = wrapper
	return [value]
}
'
	main_source := 'module main
import util

fn main() {
	values := util.wrap[int](1)
	_ := values[0]
	maybe_values := util.maybe[int](1) or { []int{} }
	_ := maybe_values[0]
	mapped := util.lookup[int](1)
	_ := mapped["value"]
	left, right := util.pair[int](1)
	_ = left
	_ := right[0]
	wrapper := util.Wrapper{}
	method_values := wrapper.wrap_method[int](1)
	_ := method_values[0]
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'util/util.v'
			source: util_source
			header: fastc_scan_source_header(util_source, 'util/util.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('Array_int util__wrap_mono_int(int value)'), c_source
	assert c_source.contains('Option util__maybe_mono_int(int value)'), c_source
	assert c_source.contains('Map_string_int util__lookup_mono_int(int value)'), c_source
	assert c_source.contains('MultiReturn util__pair_mono_int(int value)'), c_source
	assert c_source.contains('Array_int util__Wrapper_wrap_method_mono_int('), c_source
	assert c_source.contains('__typeof__((util__wrap_mono_int(1))) values'), c_source
	assert !c_source.contains('Array_voidptr values'), c_source
	assert fastc_specialized_generic_result_type('Map_string_Array_voidptr', 'int') == 'Map_string_Array_int'
	assert fastc_specialized_generic_result_type('voidptr*', 'int') == 'int*'
}

fn test_selfhost_on_demand_generic_infers_composite_parameter_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	util_source := 'module util

pub fn head[T](items []T) T {
	return items[0]
}

pub fn lookup[T](items map[string]T) T {
	return items["value"]
}
'
	main_source := 'module main
import util

fn main() {
	items := [11, 22]
	_ := util.head(items)
	mapped := {
		"value": 33
	}
	_ := util.lookup(mapped)
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'util/util.v'
			source: util_source
			header: fastc_scan_source_header(util_source, 'util/util.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('int util__head_mono_int(Array_int items)'), c_source
	assert c_source.contains('int util__lookup_mono_int(Map_string_int items)'), c_source
	assert c_source.contains('util__head_mono_int(items)'), c_source
	assert c_source.contains('util__lookup_mono_int(mapped)'), c_source
	assert fastc_infer_generic_type_from_parameter('Array_voidptr', 'Array_int') == 'int'
	assert fastc_infer_generic_type_from_parameter('Map_string_Array_voidptr', 'Map_string_Array_example__Item_ptr') == 'example__Item*'
}

fn test_selfhost_on_demand_generic_does_not_partially_infer_multiple_type_parameters() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	util_source := 'module util

pub fn pair[T, U](left T, right U) T {
	_ = right
	return left
}
'
	main_source := 'module main
import util

fn main() {
	_ := util.pair(1, "right")
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'util/util.v'
			source: util_source
			header: fastc_scan_source_header(util_source, 'util/util.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert !c_source.contains('util__pair_mono_'), c_source
}

fn test_selfhost_user_voidptr_generic_method_named_check_element_type_valid_keeps_body() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Validator {}

fn (v Validator) check_element_type_valid[T](element T) bool {
	_ = v
	_ = element
	return true
}

fn validate(value voidptr) bool {
	validator := Validator{}
	return validator.check_element_type_valid(value)
}

fn main() {
	_ := validate(unsafe { nil })
}
', 'selfhost_user_check_element_type_valid.v', prefs) or { panic(err) }
	marker := '\nbool Validator_check_element_type_valid_mono_voidptr('
	definition_start := c_source.last_index(marker) or { -1 }
	assert definition_start >= 0, c_source
	definition := c_source[definition_start..]
	assert definition.contains('return ((bool)true);'), definition
}

fn test_selfhost_on_demand_generic_erases_imported_generic_struct_arguments() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	util_source := 'module util

struct Result[T] {
	value T
}

pub fn wrap[T](value T) Result[T] {
	return Result[T]{
		value: value
	}
}

pub fn fill[T](mut value T) {
	\$for field in T.fields {
		value.\$(field.name) = 1
	}
}

pub fn take[T](mut value T) {
	_ = value
}
'
	main_source := 'module main
import util

struct Payload {
	value int
}

fn main() {
	mut payload := Payload{
		value: 1
	}
	_ := util.wrap[Payload](payload)
	util.fill(mut payload)
	mut values := ["one"]
	util.take(mut values)
}
'
	c_source, _, _ := generate_source_files([
		FastcSourceFile{
			path: 'util/util.v'
			source: util_source
			header: fastc_scan_source_header(util_source, 'util/util.v', prefs) or { panic(err) }
		},
		FastcSourceFile{
			path: 'main.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'main.v', prefs) or { panic(err) }
		},
	], map[string]string{}, prefs) or { panic(err) }
	assert c_source.contains('util__wrap_mono_Payload'), c_source
	assert c_source.contains('util__fill_mono_Payload'), c_source
	assert c_source.contains('util__take_mono_Array_string'), c_source
	assert c_source.contains('(util__Result){'), c_source
	assert !c_source.contains('Result[Payload]'), c_source
}

fn test_selfhost_dynamic_array_positional_struct_literals_use_compound_literals() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Pair {
	left  string
	right string
}

const pairs = [Pair{"left", "right"}]

fn main() {
	_ := pairs
}
', 'selfhost_dynamic_array_positional_struct_literals.v', prefs) or { panic(err) }
	assert c_source.contains('(Pair[]){(Pair){_S("left"),_S("right")}}'), c_source
}

fn test_selfhost_nil_pointer_struct_field_is_cast_without_address_of() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Node {
	children &voidptr
}

fn make_node() &Node {
	return &Node{
		children: unsafe { nil }
	}
}

fn main() {
	_ := make_node()
}
', 'selfhost_nil_pointer_struct_field.v', prefs) or { panic(err) }
	assert c_source.contains('= (NULL)'), c_source
	assert !c_source.contains('&(NULL)'), c_source
}

fn test_selfhost_c_pointer_argument_accepts_zero_as_null() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn C.consume_status(&int) int

fn main() {
	_ := C.consume_status(0)
}
', 'selfhost_c_pointer_zero.v', prefs) or { panic(err) }
	assert c_source.contains('consume_status(NULL)'), c_source
	assert !c_source.contains('&(0)'), c_source
}

fn test_selfhost_fixed_array_of_c_struct_uses_registered_compound_type() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

const event_count = 4

pub struct C.kevent {
	value int
}

fn C.kevent() int

fn main() {
	mut events := [event_count]C.kevent{}
	_ = events[0]
	_ = C.kevent()
}
', 'selfhost_fixed_array_c_struct.v', prefs) or { panic(err) }
	assert c_source.contains('FixedArray_main__event_count_FASTC_ARRAY_OF_struct_kevent'), c_source
	assert !c_source.contains('[main__event_count](struct kevent){}'), c_source
	assert c_source.contains('kevent()'), c_source
	assert !c_source.contains('(struct kevent)(*('), c_source
}
