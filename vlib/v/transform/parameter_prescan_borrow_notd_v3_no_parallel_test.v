module transform

import os
import v.parser
import v.pref
import v.types

struct ParamPrescanBorrowSnapshot {
	params  map[string][]string
	borrows map[string][]bool
	file    string
	mod     string
}

fn param_prescan_borrow_snapshot(path string, parallel bool) ParamPrescanBorrowSnapshot {
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	mut t := new_transformer(mut a, &tc, map[string]bool{})
	t.parallel_enabled = parallel
	t.skip_generics = true
	t.scope_parallel_workers = true
	t.retain_prescan_scopes = true
	t.prepare_with_pre_scans()
	mut params := map[string][]string{}
	mut borrows := map[string][]bool{}
	for i, node in a.nodes {
		if node.kind != .fn_decl {
			continue
		}
		param_types := t.call_param_types_from_decl(node.value) or { panic(node.value) }
		params[node.value] = param_types.map(it.name().clone())
		borrows[node.value] = (t.fixed_array_borrow_params[i] or { []bool{} }).clone()
	}
	result := ParamPrescanBorrowSnapshot{
		params:  params
		borrows: borrows
		file:    tc.cur_file.clone()
		mod:     tc.cur_module.clone()
	}
	for scope in t.prescan_scopes {
		transform_worker_scope_free(scope)
	}
	return result
}

fn test_parameter_prescan_preserves_fixed_array_borrow_summaries() {
	previous := os.getenv_opt('V3_NO_PAR_TRANSFORM_PARAM_PREP')
	os.unsetenv('V3_NO_PAR_TRANSFORM_PARAM_PREP')
	defer {
		if value := previous {
			os.setenv('V3_NO_PAR_TRANSFORM_PARAM_PREP', value, true)
		} else {
			os.unsetenv('V3_NO_PAR_TRANSFORM_PARAM_PREP')
		}
	}
	path := os.join_path(os.vtmp_dir(), 'param_prescan_borrow_${os.getpid()}.v')
	os.write_file(path, 'module main
type Row = [4]int
struct Holder { row Row }
fn read_row(row &Row) int { return row[0] }
fn write_row(mut row Row) { row[1] = 7 }
fn read_holder(holder &Holder) int { return holder.row[0] }
fn alias_row(row &Row) int {
 alias := row
 return unsafe { alias[0] }
}

fn escape_row(row &Row) &Row { return row }
fn take_address(row &Row) &int { return unsafe { &row[0] } }
fn forward_row(row &Row) int { return read_row(row) }
fn forward_escape(row &Row) &Row { return escape_row(row) }
fn main() {}
')!
	defer { os.rm(path) or {} }
	serial := param_prescan_borrow_snapshot(path, false)
	parallel := param_prescan_borrow_snapshot(path, true)
	assert serial.params.len == parallel.params.len
	for name, params in serial.params {
		assert parallel.params[name] == params, name
		assert parallel.borrows[name] == serial.borrows[name], name
	}
	// forward_row hands its parameter straight to read_row, which borrows it
	for name in ['read_row', 'write_row', 'read_holder', 'forward_row'] {
		assert parallel.borrows[name] == [true], name
	}
	for name in ['alias_row', 'escape_row', 'take_address', 'forward_escape'] {
		assert parallel.borrows[name].len == 0, name
	}
	assert parallel.file == serial.file
	assert parallel.mod == serial.mod
}

fn test_borrow_scan_keeps_parameter_and_function_results_independent() {
	path := os.join_path(os.vtmp_dir(), 'borrow_scan_parameters_${os.getpid()}.v')
	os.write_file(path, 'module main
type Row = [4]int
struct Holder { row Row }
fn read_row(row &Row) int { return row[0] }
fn read_pair(left &Row, right &Row) int { return left[0] + right[0] }
fn (holder &Holder) read_pair(row &Row) int { return holder.row[0] + row[0] }
fn forward_pair(left &Row, right &Row) int {
 return read_pair(left, right) + read_pair(right, left)
}
fn forward_method(holder &Holder, row &Row) int { return holder.read_pair(row) }
fn read_both(left &Row, right &Row) int {
 return read_row(left) + (read_row(right) + read_row(left))
}
fn escape_second(left &Row, right &Row) &int {
 _ = read_row(left) + read_row(left)
 return unsafe { &right[0] }
}
fn escape_first(left &Row, right &Row) &int {
 _ = read_row(right) + read_row(right)
 return unsafe { &left[0] }
}
fn read_after(row &Row) int { return read_row(row) }
fn main() {}
')!
	defer { os.rm(path) or {} }
	for parallel in [false, true] {
		snapshot := param_prescan_borrow_snapshot(path, parallel)
		assert snapshot.borrows['read_row'] == [true]
		assert snapshot.borrows['read_pair'] == [true, true]
		assert snapshot.borrows['forward_pair'] == [true, true]
		assert snapshot.borrows['forward_method'] == [true, true]
		assert snapshot.borrows['read_both'] == [true, true]
		assert snapshot.borrows['escape_second'] == [true, false]
		assert snapshot.borrows['escape_first'] == [false, true]
		assert snapshot.borrows['read_after'] == [true]
	}
}

fn test_borrow_summary_dependency_chains_cycles_and_repeated_edges() {
	path := os.join_path(os.vtmp_dir(), 'borrow_dependency_graph_${os.getpid()}.v')
	mut source := 'module main\ntype Row = [4]int\n'
	for i in 0 .. 48 {
		body := if i == 47 { 'return p[0]' } else { 'return step${i + 1}(p)' }
		source += 'fn step${i}(p &Row) int { ${body} }\n'
	}
	source += 'fn repeated(p &Row) int { return step0(p) + step0(p) }\n'
	source += 'fn cycle_a(p &Row) int { return cycle_b(p) }\n'
	source += 'fn cycle_b(p &Row) int { return cycle_a(p) }\n'
	source += 'fn retained(p &Row) &Row { return p }\n'
	source += 'fn forward_retained(p &Row) &Row { return retained(p) }\n'
	source += 'fn independent(p &Row, q &Row) int { return step0(p) + unsafe { retained(q)[0] } }\n'
	source += 'fn main() {}\n'
	os.write_file(path, source)!
	defer { os.rm(path) or {} }
	for parallel in [false, true] {
		snapshot := param_prescan_borrow_snapshot(path, parallel)
		for i in 0 .. 48 {
			assert snapshot.borrows['step${i}'] == [true], 'step${i}'
		}
		assert snapshot.borrows['repeated'] == [true]
		assert snapshot.borrows['independent'] == [true, false]
		for name in ['cycle_a', 'cycle_b', 'retained', 'forward_retained'] {
			assert snapshot.borrows[name].len == 0, name
		}
	}
}
