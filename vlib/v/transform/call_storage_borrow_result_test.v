module transform

import os
import v.flat
import v.gen.c as cgen
import v.parser
import v.pref
import v.types

const call_storage_borrow_result_source = 'module main
struct Row { payload []u8 }
struct Cols { n int; idx [16]int }
struct Payload { pointer &int }
struct Envelope { inner Payload }
struct Sink { mut: pointer &int = unsafe { nil } }
fn (row Row) at(i int) !u8 {
 if i < 0 || i >= row.payload.len { return error("no column") }
 return row.payload[i]
}
fn (cols &Cols) is_zero(row Row) !bool { return row.at(cols.idx[0])! == 0 }
fn zero_from_cols(cols &Cols, row Row) !bool { return row.at(cols.idx[0])! == 0 }
fn resolve_cols() !Cols { return Cols{n: 1} }
fn read_receiver(row Row) int {
 cols := Cols{n: 1}
 zero := cols.is_zero(row) or { return -1 }
 return if zero { 0 } else { 1 }
}
fn read_argument(row Row) int {
 cols := Cols{n: 1}
 zero := zero_from_cols(cols, row) or { return -1 }
 return if zero { 0 } else { 1 }
}
fn read_unwrapped(row Row) int {
 cols := resolve_cols() or { return -1 }
 zero := cols.is_zero(row) or { return -1 }
 return if zero { 0 } else { 1 }
}
fn (cols &Cols) slice(row Row) ![]u8 {
 index := cols.idx[0]
 _ = row.at(index)!
 return row.payload[index..]
}
fn read_slice(row Row) []u8 {
 cols := Cols{n: 1}
 _ = row.at(0) or { return []u8{} }
 return cols.slice(row) or { return []u8{} }
}
fn (cols &Cols) slice_direct(row Row) ![]u8 {
 _ = row.at(cols.idx[0])!
 return row.payload[cols.idx[0]..]
}
fn read_slice_direct(row Row) []u8 {
 cols := Cols{n: 1}
 _ = row.at(0) or { return []u8{} }
 return cols.slice_direct(row) or { return []u8{} }
}
fn retain_copy(copy Payload, mut sink Sink) !bool {
 sink.pointer = copy.pointer
 return true
}
fn (envelope &Envelope) retain(mut sink Sink) !bool {
 return retain_copy(envelope.inner, mut sink)
}
fn retain_payload(value int, mut sink Sink) {
 local := value
 envelope := Envelope{inner: Payload{pointer: &local}}
 _ = envelope.retain(mut sink) or { return }
}
fn retain_argument(mut envelope Envelope, mut sink Sink) !bool {
 return retain_copy(envelope.inner, mut sink)
}
fn retain_indirect_receiver(value int, mut sink Sink) {
 local := value
 envelope := Envelope{inner: Payload{pointer: &local}}
 alias := &envelope
 _ = (*alias).retain(mut sink) or { return }
}
fn retain_indirect_argument(value int, mut sink Sink) {
 local := value
 mut envelope := Envelope{inner: Payload{pointer: &local}}
 alias := &envelope
 _ = retain_argument(mut *alias, mut sink) or { return }
}
fn retain_loop_receiver(value int, mut sink Sink) {
 local := value
 mut items := [Envelope{inner: Payload{pointer: &local}}]!
 for mut item in items {
  _ = item.retain(mut sink) or { return }
 }
}
fn retain_loop_argument(value int, mut sink Sink) {
 local := value
 mut items := [Envelope{inner: Payload{pointer: &local}}]!
 for mut item in items {
  _ = retain_argument(mut item, mut sink) or { return }
 }
}
fn retain_receiver(cols &Cols) !&Cols { return cols }
fn escaping_receiver() &Cols {
 cols := Cols{n: 1}
 return retain_receiver(cols) or { panic("unreachable") }
}
fn main() {
 mut sink := Sink{}
 retain_payload(11, mut sink)
 assert *sink.pointer == 11
 retain_indirect_receiver(12, mut sink)
 assert *sink.pointer == 12
 retain_indirect_argument(13, mut sink)
 assert *sink.pointer == 13
 retain_loop_receiver(14, mut sink)
 assert *sink.pointer == 14
 retain_loop_argument(15, mut sink)
 assert *sink.pointer == 15
}
'

fn call_storage_borrow_result_function_body(generated string, name string) string {
	headers := generated.split_into_lines().filter((it.contains(' ${name}(') || it.contains(' main__${name}(')) && it.ends_with(' {'))
	assert headers.len == 1, name
	return generated.all_after(headers[0]).all_before('\n}')
}

fn test_consumed_result_keeps_proven_storage_borrows_without_losing_embedded_aliases() {
	path := os.join_path(os.vtmp_dir(), 'call_storage_borrow_result_${os.getpid()}.v')
	os.write_file(path, call_storage_borrow_result_source)!
	defer { os.rm(path) or {} }
	for parallel in [false, true] {
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		mut t := new_transformer(mut a, &tc, map[string]bool{})
		t.parallel_enabled = parallel
		t.scope_parallel_workers = true
		t.skip_generics = true
		t.cur_module = 'main'
		t.cur_file = path
		t.transformed_fns = []bool{len: a.nodes.len}
		t.prepare_with_pre_scans()
		mut used := map[string]bool{}
		for i, node in a.nodes {
			if node.kind != .fn_decl { continue }
			used[node.value] = true
			// Direct receiver reads in returned slice bounds remain conservative.
			if node.value == 'Cols.slice_direct' {
				summary := t.fixed_array_borrow_params[i] or { []bool{} }
				assert summary.len == 0 || !summary[0], node.value
			}
			if node.value in ['Cols.is_zero', 'zero_from_cols', 'Cols.slice', 'Envelope.retain',
				'retain_argument'] {
				summary := t.fixed_array_borrow_params[i] or { []bool{} }
				assert summary.len > 0, node.value
				assert summary[0], node.value
			}
		}
		t.transform_all()
		assert t.monomorph_errors.len == 0, t.monomorph_errors.str()
		mut gen := cgen.FlatGen.new()
		generated := gen.gen_with_used(a, used, &tc)
		for name in ['read_receiver', 'read_argument', 'read_unwrapped', 'read_slice'] {
			body := call_storage_borrow_result_function_body(generated, name)
			assert !body.contains('memdup('), '${name}: ${body}'
		}
		for name in ['retain_payload', 'retain_indirect_receiver', 'retain_indirect_argument',
			'retain_loop_receiver', 'retain_loop_argument', 'escaping_receiver', 'read_slice_direct'] {
			body := call_storage_borrow_result_function_body(generated, name)
			if name.starts_with('retain_') {
				assert body.split_into_lines().any(it.contains('local =') && it.contains('memdup(')), body
			} else {
				assert body.contains('memdup('), '${name}: ${body}'
			}
		}
	}
	run := os.exec([@VEXE, '-cc', @CCOMPILER, '-no-retry-compilation', '-nocache', 'run', path])
	assert run.exit_code == 0, run.output
}

fn test_imported_result_borrow_has_no_per_call_heap_allocation() {
	root := os.join_path(os.vtmp_dir(), 'imported_result_borrow_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'storagecols'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "borrow_fixture" }')!
	os.write_file(os.join_path(root, 'storagecols', 'cols.v'), 'module storagecols
pub struct Row { pub: payload []u8 }
pub struct Cols { idx [16]int }
fn (row Row) at(i int) !u8 {
 if i >= row.payload.len { return error("no column") }
 return row.payload[i]
}
pub fn (cols &Cols) is_zero(row Row) !bool { return row.at(cols.idx[0])! == 0 }
pub fn zero(cols &Cols, row Row) !bool { return row.at(cols.idx[0])! == 0 }
pub fn resolve() !Cols { return Cols{} }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import storagecols
@[noinline]
fn read_method(row storagecols.Row) int {
 cols := storagecols.Cols{}
 zero := cols.is_zero(row) or { return -1 }
 return if zero { 0 } else { 1 }
}
@[noinline]
fn read_parameter(row storagecols.Row) int {
 cols := storagecols.Cols{}
 zero := storagecols.zero(cols, row) or { return -1 }
 return if zero { 0 } else { 1 }
}
@[noinline]
fn read_or(row storagecols.Row) int {
 cols := storagecols.resolve() or { return -1 }
 zero := cols.is_zero(row) or { return -1 }
 return if zero { 0 } else { 1 }
}
fn main() {
 one := storagecols.Row{payload: [u8(1)]}
 zero := storagecols.Row{payload: [u8(0)]}
 empty := storagecols.Row{}
 assert read_method(one) == 1
 assert read_parameter(one) == 1
 assert read_or(one) == 1
 assert read_method(zero) == 0
 assert read_parameter(zero) == 0
 assert read_or(zero) == 0
 assert read_method(empty) == -1
 assert read_parameter(empty) == -1
 assert read_or(empty) == -1
 before := gc_heap_usage().total_bytes
 mut sum := 0
 for _ in 0 .. 2000 {
  sum += read_method(one) + read_parameter(one) + read_or(one)
 }
 assert gc_heap_usage().total_bytes == before
 assert sum == 6000
}
')!
	cpath := os.join_path(root, 'borrow.c')
	main := os.join_path(root, 'main.v')
	compiled := os.exec([@VEXE, '-cc', @CCOMPILER, '-no-retry-compilation', '-nocache', '-o', cpath,
		main])
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(cpath)!
	for name in ['read_method', 'read_parameter', 'read_or'] {
		body := call_storage_borrow_result_function_body(generated, name)
		assert !body.contains('memdup('), '${name}: ${body}'
	}
	run := os.exec([@VEXE, '-cc', @CCOMPILER, '-no-retry-compilation', '-nocache', 'run', main])
	assert run.exit_code == 0, run.output
}
