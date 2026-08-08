// Tests for the user-tunable recursion / cutoff guard limits
// (defaults live in vlib/v/ast/table.v, wiring in vlib/v/pref/pref.v).
//
// Every guard can be overridden with a compiler flag, e.g.
// `v -generic-fn-postprocess-iters 512 run file.v`.
module main

import os

const vexe = @VEXE

const work_dir = os.join_path(os.vtmp_dir(), 'recursion_cutoff_flags_test')

const bug_recursive_generic_fn = 'fn foo[T]() {
	foo[[]T]()
}

fn main() {
	foo[int]()
}
'

const bug_recursive_generic_method = 'struct Box[T] {
	val T
}

fn (b Box[T]) foo() {
	Box[Box[T]]{}.foo()
}

fn main() {
	Box[int]{}.foo()
}
'

const bug_nested_generic_struct = 'struct Box[T] {
	Box[Box[T]]
}

fn main() {
	b := Box[int]{}
	println(b)
}
'

const bug_circular_sum_type = 'type MySum[T] = T | MySum[MySum[T]]

fn main() {
	mut x := MySum[int](0)
	println(x)
}
'

const bug_recursive_fn_alias = 'type Func[T] = fn (f Func[[]T]) T

fn main() {
	_ := Func[int](none)
}
'

const valid_fn_instantiations = 'fn id[T](x T) T {
	return x
}

fn main() {
	assert id(1) == 1
	assert id(i64(2)) == i64(2)
	assert id(u8(3)) == u8(3)
	assert id(`a`) == `a`
	assert id(f32(1.5)) == f32(1.5)
	assert id(f64(2.5)) == f64(2.5)
	println(12345)
}
'

const valid_struct_instantiations = "struct Box[T] {
pub:
	val T
}

fn main() {
	a := Box[int]{100}
	b := Box[string]{'str'}
	c := Box[f64]{1.5}
	d := Box[bool]{true}
	e := Box[rune]{`z`}
	f := Box[u64]{42}
	assert a.val == 100
	assert b.val == 'str'
	assert c.val == 1.5
	assert d.val
	assert e.val == `z`
	assert f.val == 42
	println('struct insts ok')
}
"

fn compile_and_run(file_name string, contents string, args string) os.Result {
	os.mkdir_all(work_dir) or { panic(err) }
	path := os.join_path(work_dir, file_name)
	os.write_file(path, contents) or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	return os.execute('${os.quoted_path(vexe)} ${args} run ${os.quoted_path(path)} 2>&1')
}

fn test_recursive_generic_fn_caught_by_default_cutoff() {
	res := compile_and_run('bug_recursive_generic_fn.v', bug_recursive_generic_fn, '')
	assert res.exit_code == 1, res.output
	assert res.output.contains('cutoff limit of 128 iterations'), res.output
}

fn test_recursive_generic_method_caught_by_cutoff_flag() {
	// NB: the default of 128 is intentionally not used here. The method/struct
	// expansion variant consumes memory so aggressively, that on
	// memory-constrained machines the process can get OOM-killed before the
	// 128th pass. A small explicit limit keeps the failure fast and deterministic,
	// and doubles as a check that the flag is honored for this code path too.
	res := compile_and_run('bug_recursive_generic_method.v', bug_recursive_generic_method,
		'-generic-fn-postprocess-iters 16')
	assert res.exit_code == 1, res.output
	assert res.output.contains('cutoff limit of 16 iterations'), res.output
}

fn test_generic_fn_postprocess_iters_flag_is_respected() {
	res := compile_and_run('bug_recursive_generic_fn_flag.v', bug_recursive_generic_fn,
		'-generic-fn-postprocess-iters 16')
	assert res.exit_code == 1, res.output
	// the reported limit must be the one passed on the command line:
	assert res.output.contains('cutoff limit of 16 iterations'), res.output
}

fn test_circular_sum_type_is_rejected() {
	res := compile_and_run('bug_circular_sum_type.v', bug_circular_sum_type, '')
	assert res.exit_code == 1, res.output
	assert res.output.contains('sum type cannot hold itself'), res.output
}

fn test_nested_generic_struct_hits_depth_limit_by_default() {
	res := compile_and_run('bug_nested_generic_struct.v', bug_nested_generic_struct, '')
	assert res.exit_code == 1, res.output
	assert res.output.contains('generic instantiation depth limit 256 exceeded'), res.output
}

fn test_depth_limit_flag_is_respected() {
	res := compile_and_run('bug_nested_generic_struct_flag.v', bug_nested_generic_struct,
		'-generic-inst-depth-limit 8')
	assert res.exit_code == 1, res.output
	assert res.output.contains('generic instantiation depth limit 8 exceeded'), res.output
}

fn test_depth_limit_flag_is_clamped_to_a_safe_maximum() {
	res := compile_and_run('bug_nested_generic_struct_clamp.v', bug_nested_generic_struct,
		'-generic-inst-depth-limit 1000')
	assert res.exit_code == 1, res.output
	// values above 512 are clamped, the reported limit must be 512:
	assert res.output.contains('generic instantiation depth limit 512 exceeded'), res.output
}

fn test_recursive_fn_alias_hits_depth_limit_by_default() {
	res := compile_and_run('bug_recursive_fn_alias.v', bug_recursive_fn_alias, '')
	assert res.exit_code == 1, res.output
	assert res.output.contains('generic instantiation depth limit 256 exceeded'), res.output
}

fn test_fn_instantiation_limit_flag_is_respected() {
	res := compile_and_run('valid_fn_instantiations_flag.v', valid_fn_instantiations,
		'-generic-fn-inst-limit 4')
	assert res.exit_code == 1, res.output
	assert res.output.contains('generic function instantiation limit 4 exceeded'), res.output
}

fn test_max_postprocess_iterations_flag_is_respected() {
	res := compile_and_run('valid_struct_instantiations_flag.v', valid_struct_instantiations,
		'-max-postprocess-iterations 5')
	assert res.exit_code == 1, res.output
	assert res.output.contains('generic_insts_to_concrete limit 5 exceeded'), res.output
}

fn test_valid_code_compiles_with_default_limits() {
	r1 := compile_and_run('valid_fn_instantiations.v', valid_fn_instantiations, '')
	assert r1.exit_code == 0, r1.output
	assert r1.output.contains('12345'), r1.output
	r2 := compile_and_run('valid_struct_instantiations.v', valid_struct_instantiations, '')
	assert r2.exit_code == 0, r2.output
}

fn test_flags_are_documented_in_help() {
	res := os.execute('${os.quoted_path(vexe)} help build')
	assert res.exit_code == 0, res.output
	assert res.output.contains('-generic-fn-inst-limit'), res.output
	assert res.output.contains('-generic-inst-depth-limit'), res.output
	assert res.output.contains('-generic-fn-postprocess-iters'), res.output
	assert res.output.contains('-alias-unwrap-depth-limit'), res.output
	assert res.output.contains('-max-postprocess-iterations'), res.output
}
