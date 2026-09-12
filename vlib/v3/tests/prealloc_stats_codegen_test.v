import os

const prealloc_stats_vexe = @VEXE
const prealloc_stats_tests_dir = os.dir(@FILE)
const prealloc_stats_v3_dir = os.dir(prealloc_stats_tests_dir)
const prealloc_stats_vlib_dir = os.dir(prealloc_stats_v3_dir)
const prealloc_stats_v3_src = os.join_path(prealloc_stats_v3_dir, 'v3.v')

fn test_prealloc_stats_i64_atomics_are_defined() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_prealloc_stats_test_${pid}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${prealloc_stats_vexe} -gc none -path "${prealloc_stats_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${prealloc_stats_v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_prealloc_stats_input_${pid}.v')
	os.write_file(src, "fn main() {\n\tstats := prealloc_stats_snapshot()\n\tassert stats.enabled\n\tprintln('prealloc-stats-ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(os.temp_dir(), 'v3_prealloc_stats_input_${pid}')
	os.rm(bin) or {}
	compile := os.execute('${v3_bin} -prealloc -d prealloc_stats ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_src := '${bin}.c'
	generate_c := os.execute('${v3_bin} -prealloc -d prealloc_stats ${src} -o ${c_src}')
	assert generate_c.exit_code == 0, generate_c.output
	generated := os.read_file(c_src) or { panic(err) }
	atomic_decl_pos := generated.index('__atomic_exchange_4(u32* ptr') or { -1 }
	prealloc_helper_pos := generated.index('static inline int v_prealloc_atomic_store_i32') or {
		-1
	}
	assert atomic_decl_pos >= 0
	assert prealloc_helper_pos >= 0
	assert atomic_decl_pos < prealloc_helper_pos

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.starts_with('prealloc-stats-ok\n'), run.output
}
