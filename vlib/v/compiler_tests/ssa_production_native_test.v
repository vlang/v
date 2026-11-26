import os
import v.cmdexec

const ssa_production_vlib_dir = os.dir(os.dir(os.dir(@FILE)))
const ssa_production_v3_dir = os.dir(os.dir(@FILE))
const ssa_production_v3_src = os.join_path(ssa_production_v3_dir, 'v.v')

fn compile_ssa_production_fixture(root string, args []string) os.Result {
	if os.getenv('VFLAGS') == ''
		&& os.getenv('VTOOLS_NO_CACHE').trim_space().to_lower() !in ['1', 'true', 'yes', 'on'] {
		compile := cmdexec.run(@VEXE, args)
		if compile.exit_code == 0
			|| !compile.output.contains('ARM64 support is not compiled into this executable') {
			return compile
		}
	}
	// Standalone runs can build just the ARM64 backend for this fixture.
	v3_bin := os.join_path(root, 'v3')
	build := cmdexec.run(@VEXE, ['-gc', 'none', '-compile-backend', 'arm64', '-path',
		'${ssa_production_vlib_dir}|@vlib|@vmodules', '-o', v3_bin, ssa_production_v3_src])
	assert build.exit_code == 0, build.output
	return cmdexec.run(v3_bin, args)
}

fn test_production_native_pipeline_promotes_branch_and_loop_locals() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'v3_ssa_production_${os.getpid()}')
		os.rmdir_all(root) or {}
		os.mkdir_all(root) or { panic(err) }
		defer {
			os.rmdir_all(root) or {}
		}
		source := os.join_path(root, 'main.v')
		os.write_file(source, 'fn choose(ok bool) int {
	mut value := 0
	if ok { value = 10 } else { value = 20 }
	return value
}

fn loop_sum(n int) int {
	mut total := 0
	mut i := 0
	for i < n {
		if i % 2 == 0 { total += i } else { total -= i }
		i++
	}
	return total
}

struct Index {
mut:
	files map[string]int
}

struct State {
mut:
	index Index
}

fn add_file(mut index Index) {
	index.files["main.v"] = 7
}

fn main() {
	println(int_str(choose(true)))
	println(int_str(choose(false)))
	println(int_str(loop_sum(10)))
	mut state := State{}
	add_file(mut state.index)
	println(int_str(state.index.files["main.v"]))
}
') or {
			panic(err)
		}
		output := os.join_path(root, 'program')
		// No `-gc none`: the ARM64 backend links no collector, so it must not use one.
		compile := compile_ssa_production_fixture(root, ['-prod', '-b', 'arm64', '-o', output,
			source])
		assert compile.exit_code == 0, compile.output
		run := cmdexec.run(output, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '10\n20\n-5\n7'
	} $else {
		assert true
	}
}
