module parser

import os
import strings
import v.pref
import v.workers

fn speculated_batch_test_root(name string) string {
	root := os.join_path(os.vtmp_dir(), 'v3_speculated_batch_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	return root
}

// speculated_batch_source is a module of `fns` functions; a `$if` over
// `guard_const`, when given, picks one of two more, and `declares` adds a const.
fn speculated_batch_source(mod string, tag string, fns int, declares string, guard_const string) string {
	mut src := strings.new_builder(fns * 64)
	src.writeln('module ${mod}')
	src.writeln('')
	if declares.len > 0 {
		src.writeln('const ${declares} = 7')
		src.writeln('')
	}
	if guard_const.len > 0 {
		src.writeln('\$if ${guard_const} == 7 {')
		src.writeln("\tfn ${tag}_guarded() string { return 'seven' }")
		src.writeln('} \$else {')
		src.writeln("\tfn ${tag}_guarded() string { return 'other' }")
		src.writeln('}')
		src.writeln('')
	}
	for i in 0 .. fns {
		src.writeln('fn ${tag}_padding_${i}(x int) int { return x + ${i} }')
	}
	return src.str()
}

struct SpeculatedBatchFiles {
	first  []string
	second []string
}

// write_speculated_batch_files writes two batches. The first is one module, and
// declares a const. The second is two other modules, one of them with a const
// of one file that decides a guard of another; with `continues_module`, its
// first files belong to the module of the first batch and read its const.
fn write_speculated_batch_files(root string, continues_module bool) SpeculatedBatchFiles {
	mut first := []string{}
	mut second := []string{}
	for i in 0 .. 5 {
		path := os.join_path(root, 'first_${i}.v')
		declares := if i == 1 { 'batch_width' } else { '' }
		// One file is much larger than the others, like a real batch.
		fns := if i == 3 { 2400 } else { 500 }
		os.write_file(path, speculated_batch_source('alpha', 'first_${i}', fns, declares,
			'')) or { panic(err) }
		first << path
	}
	for i in 0 .. 6 {
		path := os.join_path(root, 'second_${i}.v')
		mut mod := if i < 3 { 'beta' } else { 'gamma' }
		mut guard := if i == 2 { 'later_width' } else { '' }
		declares := if i == 1 { 'later_width' } else { '' }
		if continues_module && i == 0 {
			mod = 'alpha'
			guard = 'batch_width'
		}
		// The large file is a chunk of its own, after the one of the const.
		fns := if i == 2 { 1800 } else { 400 }
		os.write_file(path, speculated_batch_source(mod, 'second_${i}', fns, declares,
			guard)) or { panic(err) }
		second << path
	}
	return SpeculatedBatchFiles{
		first:  first
		second: second
	}
}

// speculated_batch_shape describes every node of `p`, and what else a batch
// leaves behind in the parser.
fn speculated_batch_shape(p &Parser, starts [][]int) string {
	mut out := strings.new_builder(p.a.nodes.len * 24)
	for i, node in p.a.nodes {
		out.writeln('${i} ${node.kind} ${node.value} ${node.typ_text()} ${node.pos.id} ${node.pos.offset} ${node.children_start} ${node.children_count}')
	}
	for child in p.a.children {
		out.writeln('c ${int(child)}')
	}
	out.writeln('starts ${starts}')
	out.writeln('file ids ${p.a.file_node_ids}')
	out.writeln('next file ${p.next_file_id}')
	mut values := []string{}
	for key, value in p.comptime_const_values {
		values << '${key.replace('\n', '.')}=${value}'
	}
	values.sort()
	out.writeln('values ${values}')
	mut names := p.comptime_string_consts.keys()
	names.sort()
	out.writeln('names ${names.map(it.replace('\n', '.'))}')
	out.writeln('texts ${p.a.text_count()}')
	out.writeln('files ${p.parsed_v_file_paths}')
	return out.str()
}

fn parse_two_batches(files SpeculatedBatchFiles, speculate []string) (string, bool, bool) {
	mut p := Parser.new(pref.new_preferences())
	p.a.worker_pool = workers.new(3)
	if speculate.len > 0 {
		p.speculate_next_batch(speculate)
	}
	first_starts, first_parallel := p.parse_files_dispatch(files.first, true)
	ahead := !isnil(p.speculated_batch)
	second_starts, second_parallel := p.parse_files_dispatch(files.second, true)
	assert isnil(p.speculated_batch)
	assert p.diagnostics.len == 0
	p.discard_speculated_batch()
	return speculated_batch_shape(p, [first_starts, second_starts]), ahead, first_parallel
		&& second_parallel
}

fn test_a_batch_parsed_ahead_is_the_batch_parsed_in_its_turn() {
	root := speculated_batch_test_root('same')
	defer {
		os.rmdir_all(root) or {}
	}
	files := write_speculated_batch_files(root, false)
	plain, plain_ahead, plain_parallel := parse_two_batches(files, [])
	assert !plain_ahead
	assert plain_parallel
	assert plain.contains('second_2_guarded')
	ahead, was_ahead, ahead_parallel := parse_two_batches(files, files.second)
	assert was_ahead
	assert ahead_parallel
	assert ahead == plain
}

fn test_a_batch_that_continues_a_module_is_parsed_in_its_turn() {
	root := speculated_batch_test_root('module')
	defer {
		os.rmdir_all(root) or {}
	}
	// The guard of the second batch asks for a const name that the first batch
	// added to its module: the parse ahead could not know it.
	files := write_speculated_batch_files(root, true)
	plain, _, _ := parse_two_batches(files, [])
	assert plain.contains('second_0_guarded')
	ahead, was_ahead, _ := parse_two_batches(files, files.second)
	assert was_ahead
	assert ahead == plain
	mut p := Parser.new(pref.new_preferences())
	p.a.worker_pool = workers.new(3)
	p.speculate_next_batch(files.second)
	p.parse_files_dispatch(files.first, true)
	spec := unsafe { &SpeculatedBatch(p.speculated_batch) }
	assert p.speculated_batch_mismatch(spec, files.second).contains('alpha.batch_width')
	p.discard_speculated_batch()
}

fn test_a_batch_parsed_ahead_is_not_read_again() {
	root := speculated_batch_test_root('taken')
	defer {
		os.rmdir_all(root) or {}
	}
	files := write_speculated_batch_files(root, false)
	mut p := Parser.new(pref.new_preferences())
	p.a.worker_pool = workers.new(3)
	p.speculate_next_batch(files.second)
	p.parse_files_dispatch(files.first, true)
	assert !isnil(p.speculated_batch)
	// The batch arrives as expected: its files were parsed already.
	os.write_file(files.second[3], 'module beta\n\nfn written_after_the_parse() {}\n')!
	p.parse_files_dispatch(files.second, true)
	mut fn_names := map[string]bool{}
	for node in p.a.nodes {
		if node.kind == .fn_decl {
			fn_names[node.value] = true
		}
	}
	assert 'second_3_padding_0' in fn_names
	assert 'written_after_the_parse' !in fn_names
}

fn test_another_batch_drops_the_parse_ahead() {
	root := speculated_batch_test_root('other')
	defer {
		os.rmdir_all(root) or {}
	}
	files := write_speculated_batch_files(root, false)
	plain, _, _ := parse_two_batches(files, [])
	// One file less than the batch holds, and the files in another order.
	for expected in [files.second[1..], files.second.reverse()] {
		other, was_ahead, _ := parse_two_batches(files, expected)
		assert was_ahead
		assert other == plain
	}
}

fn test_a_changed_const_state_drops_the_parse_ahead() {
	root := speculated_batch_test_root('state')
	defer {
		os.rmdir_all(root) or {}
	}
	files := write_speculated_batch_files(root, false)
	mut shapes := []string{}
	for speculate in [false, true] {
		mut p := Parser.new(pref.new_preferences())
		p.a.worker_pool = workers.new(3)
		if speculate {
			p.speculate_next_batch(files.second)
		}
		first_starts, _ := p.parse_files_dispatch(files.first, true)
		assert isnil(p.speculated_batch) == !speculate
		// What a caller can do between two batches: the parser no longer holds
		// the const values that the parse ahead started from.
		p.comptime_const_values[comptime_const_value_key('alpha', 'batch_width')] = '8'
		if speculate {
			spec := unsafe { &SpeculatedBatch(p.speculated_batch) }
			assert p.speculated_batch_mismatch(spec, files.second).contains('alpha')
		}
		second_starts, _ := p.parse_files_dispatch(files.second, true)
		assert isnil(p.speculated_batch)
		shapes << speculated_batch_shape(p, [first_starts, second_starts])
	}
	assert shapes[0] == shapes[1]
}

fn test_a_parse_ahead_can_be_discarded() {
	root := speculated_batch_test_root('discard')
	defer {
		os.rmdir_all(root) or {}
	}
	files := write_speculated_batch_files(root, false)
	mut p := Parser.new(pref.new_preferences())
	p.a.worker_pool = workers.new(3)
	p.speculate_next_batch(files.second)
	p.parse_files_dispatch(files.first, true)
	assert !isnil(p.speculated_batch)
	nodes := p.a.nodes.len
	p.discard_speculated_batch()
	assert isnil(p.speculated_batch)
	assert p.a.nodes.len == nodes
	p.discard_speculated_batch()
}

fn test_a_serial_batch_ignores_the_files_ahead() {
	root := speculated_batch_test_root('serial')
	defer {
		os.rmdir_all(root) or {}
	}
	files := write_speculated_batch_files(root, false)
	mut p := Parser.new(pref.new_preferences())
	p.speculate_next_batch(files.second)
	_, was_parallel := p.parse_files_dispatch(files.first, false)
	assert !was_parallel
	assert isnil(p.speculated_batch)
	assert p.next_batch_paths.len == 0
}
