module c

import os
import strings
import v.cmdexec

// The transform lowers `for name in names` to `names[i]` and appends that index
// node after the declarations it generated last: here the closure lifted from
// `helper.run`, whose markers put the module `helper` in front of it. The const
// reference must still resolve in main.v, where it was written, and that must
// not depend on how many threads scan the AST.
fn test_const_array_iterated_after_generated_module_decls_gets_fixed_storage() {
	test_dir := os.join_path(os.vtmp_dir(), 'cgen_const_fixed_storage_ctx_${os.getpid()}')
	helper_dir := os.join_path(test_dir, 'helper')
	os.mkdir_all(helper_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	os.write_file(os.join_path(test_dir, 'v.mod'), "Module { name: 'const_fixed_storage_ctx' }") or {
		panic(err)
	}
	os.write_file(os.join_path(helper_dir, 'helper.v'), 'module helper

pub fn run() int {
	f := fn (x int) int {
		return x + 1
	}
	return f(1)
}
') or { panic(err) }
	// Enough lowered bodies after the closure that a scan thread starts inside them.
	fn_count := 120
	mut source := strings.new_builder(40_000)
	source.writeln("module main\n\nimport helper\n\nconst names = ['a', 'bb', 'ccc']\n")
	for i in 0 .. fn_count {
		source.writeln('fn count_${i}(k int) string {
	mut n := ${i}
	for name in names {
		n += name.len * k
	}
	s := "v\${n} \${k} \${n + k} \${n * 2} \${k - 1}"
	return s + "\${s.len}" + "\${n}x\${k}"
}
')
	}
	source.writeln('fn main() {\n\tprintln(helper.run())\n\tmut total := 0')
	for i in 0 .. fn_count {
		source.writeln('\ttotal += count_${i}(${i}).len')
	}
	source.writeln('\tprintln(total)\n}')
	v_file := os.join_path(test_dir, 'main.v')
	os.write_file(v_file, source.str()) or { panic(err) }
	for jobs in ['1', '2', '8'] {
		c_file := os.join_path(test_dir, 'main_${jobs}.c')
		generated := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', '-o', c_file, v_file],
			test_dir, {
				'VJOBS': jobs
			})
		assert generated.exit_code == 0, 'VJOBS=${jobs}: ${generated.output}'
		code := os.read_file(c_file) or { panic(err) }
		assert code.contains('const string main__names[3] = '), 'VJOBS=${jobs}: `names` lost its fixed storage'
		assert !code.contains('Array main__names;'), 'VJOBS=${jobs}'
		assert code.contains('main__names[v_fixed_index('), 'VJOBS=${jobs}'
	}
	ran := cmdexec.run_in_merged_with_env(@VEXE, ['-nocache', 'run', v_file], test_dir, {
		'VJOBS': '8'
	})
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space().split_into_lines() == ['2', '3139']
}
