// Regression test for https://github.com/vlang/v/issues/28022
//
// An `or {}` block whose body is a compile-time `$if / $else` used to emit a
// broken result temp. When the *selected* branch ends in a void/noreturn
// expression such as `panic(err)`:
//   * if the value-producing branch is pruned as dead code (an ordinary build),
//     the result temp was referenced but never declared -> `use of undeclared
//     identifier '_t3'`;
//   * if the value-producing branch is retained (`-cross`/`output_cross_c`
//     mode), the void branch was still assigned to the temp -> `void value not
//     ignored as it ought to be`.
// Whether a branch assigns to the result temp must be decided per branch, from
// that branch's own final statement.
import os

fn g(fail bool) !int {
	if fail {
		return error('boom')
	}
	return 42
}

// `$if true` guarantees the panicking (void) branch is the selected one on
// every target, so the fix is exercised regardless of the host OS. The `or {}`
// result temp must be declared/omitted correctly for the `panic(err)` body.
fn assign_form(fail bool) int {
	x := g(fail) or {
		$if true {
			panic(err)
		} $else {
			int(0)
		}
	}
	return x
}

fn test_comptime_if_in_or_block_compiles_and_runs() {
	// The call succeeds, so the `panic(err)` branch is never taken at runtime.
	assert assign_form(false) == 42
}

// In `-cross`/`output_cross_c` mode the non-selected comptime branch is kept, so
// the void `panic(err)` branch and the value `int(0)` branch are emitted side by
// side under `#if/#else`. The void branch must be a plain statement (not
// `<tmp> = panic(...)`), while the value branch assigns to the temp.
fn test_comptime_if_in_or_block_cross_codegen() {
	vexe := os.getenv('VEXE')
	if vexe == '' {
		return
	}
	tmp := os.join_path(os.vtmp_dir(), 'comptime_if_or_block_cross_${os.getpid()}')
	os.mkdir_all(tmp) or { return }
	defer {
		os.rmdir_all(tmp) or {}
	}
	src := os.join_path(tmp, 'prog.v')
	out := os.join_path(tmp, 'prog.c')
	os.write_file(src, "module main

fn g(i int) !int {
	return error('!')
}

fn main() {
	x := g(1) or {
		\$if !windows {
			panic(err)
		} \$else {
			int(0)
		}
	}
	println(x)
}
")!
	res :=
		os.execute('${os.quoted_path(vexe)} -os cross -o ${os.quoted_path(out)} ${os.quoted_path(src)}')
	assert res.exit_code == 0, res.output
	generated := os.read_file(out)!
	// The `panic(err)` branch must be emitted as a bare statement, never as an
	// assignment to the result temp.
	assert !generated.contains('= builtin___v_panic'), generated
	// The value branch must still assign to the result temp.
	assert generated.contains('builtin___v_panic')
}
