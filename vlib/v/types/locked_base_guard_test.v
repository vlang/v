module types

import os
import v.flat
import v.parser
import v.pref

fn test_empty_locked_shared_bases_skip_lvalue_and_mut_receiver_analysis() {
	mut a := flat.FlatAst.new()
	ident := a.add_val(.ident, 'holder')
	mut tc := TypeChecker.new(a)
	assert !tc.check_locked_shared_base_lvalue_mutation(ident)
	assert !tc.check_locked_shared_mut_receiver_mutation(ident)
	assert !tc.check_locked_shared_base_lvalue_mutation(flat.NodeId(-1))
	assert !tc.check_locked_shared_mut_receiver_mutation(flat.NodeId(-1))
	assert tc.errors.len == 0
	tc.fn_context.locked_shared_base_names['holder'] = 'holder.item'
	assert tc.check_locked_shared_base_lvalue_mutation(ident)
	tc.fn_context.locked_shared_base_names.clear()
	assert !tc.check_locked_shared_base_lvalue_mutation(ident)
	assert !tc.check_locked_shared_mut_receiver_mutation(ident)
}

fn test_locked_shared_base_guard_preserves_source_diagnostics() {
	cases := [
		'fn main() { mut value := 1; value = 2; mut values := [1]; values[0] = 3 }',
		'struct Cell { mut: count int }
fn change(mut value Cell) { value.count++ }
fn main() { mut value := Cell{}; value.count = 2; change(mut value) }',
		'struct Cell { mut: count int }
fn main() { shared value := Cell{}; lock value { value.count = 1 } }',
		'struct Cell { mut: count int }
fn main() { shared value := Cell{}; rlock value { value.count = 1 } }',
		'struct Cell { mut: count int }
struct Holder { mut: item shared Cell }
fn main() { mut holder := &Holder{}; lock holder.item { holder = &Holder{} } }',
		'struct Cell { mut: count int }
struct Holder { mut: item shared Cell }
fn main() { mut holders := [Holder{}]; lock holders[0].item { holders = []Holder{} } }',
		'struct Cell { mut: count int }
struct Holder { mut: item shared Cell }
fn main() { mut holder := &Holder{}; lock holder.item { unsafe { alias := &holder; *alias = &Holder{} } } }',
		'struct Cell { mut: count int }
struct Holder { mut: item shared Cell }
fn update(mut holder Holder) {}
fn main() { mut holder := Holder{}; lock holder.item { update(mut holder) } }',
	]
	for index, body in cases {
		for scoped in [false, true] {
			path := os.join_path(os.vtmp_dir(), 'locked_base_guard_${index}_${os.getpid()}.v')
			os.write_file(path, 'module main
' + body + '
')!
			mut p := parser.Parser.new(pref.new_preferences())
			a := p.parse_file(path)
			assert p.diagnostics.len == 0, '${index}: ${p.diagnostics}'
			mut tc := TypeChecker.new(a)
			tc.scope_parallel_check_workers = scoped
			tc.collect(a)
			_ = tc.check_semantics_opt(false)
			assert tc.errors.len == [0, 0, 0, 2, 1, 1, 1, 1][index]
			os.rm(path) or {}
			println('LOCK_CASE ${index} scoped=${scoped}: errors=${tc.errors.len} notices=${tc.notices.len}')
			for err in tc.errors {
				println('LOCK_ERROR:${err.kind}:${int(err.node)}:${err.pos.offset}:${err.pos.end}:${err.msg}:${err.details.join('|')}')
			}
			for notice in tc.notices {
				println('LOCK_NOTICE:${notice.kind}:${int(notice.node)}:${notice.pos.offset}:${notice.pos.end}:${notice.msg}:${notice.details.join('|')}')
			}
		}
	}
}
