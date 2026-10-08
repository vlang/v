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
	for name in ['read_row', 'write_row', 'read_holder'] {
		assert parallel.borrows[name] == [true], name
	}
	for name in ['alias_row', 'escape_row', 'take_address', 'forward_row'] {
		assert parallel.borrows[name].len == 0, name
	}
	assert parallel.file == serial.file
	assert parallel.mod == serial.mod
}
