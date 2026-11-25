import os
import v.tempname

fn test_prealloc_stats_i64_atomics_are_defined() {
	root := os.join_path(os.vtmp_dir(), 'v3_prealloc_stats_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "fn main() {\n\tstats := prealloc_stats_snapshot()\n\tassert stats.enabled\n\tprintln('prealloc-stats-ok')\n}\n") or {
		panic(err)
	}
	bin := os.join_path(root, 'program')
	compile := os.exec([@VEXE, '-prealloc', '-d', 'prealloc_stats', '${src}', '-b', 'c', '-o',
		bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	c_src := '${bin}.c'
	generate_c := os.exec([@VEXE, '-prealloc', '-d', 'prealloc_stats', '${src}', '-o', '${c_src}'])
	assert generate_c.exit_code == 0, generate_c.output
	generated := os.read_file(c_src) or { panic(err) }
	atomic_decl_pos := generated.index('__atomic_exchange_4(u32* ptr') or { -1 }
	prealloc_helper_pos := generated.index('static inline int v_prealloc_atomic_store_i32') or {
		-1
	}
	assert atomic_decl_pos >= 0
	assert prealloc_helper_pos >= 0
	assert atomic_decl_pos < prealloc_helper_pos

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.starts_with('prealloc-stats-ok\n'), run.output
}
