import os
import strings
import v.cmdexec

fn recursive_type_graph_source() string {
	mut source := strings.new_builder(4096)
	source.write_string('module main
struct GraphLeaf { value string }
')
	// Repeated references form a compact graph whose expanded structural key is exponential.
	for i in 0 .. 24 {
		previous := if i == 0 { 'GraphLeaf' } else { 'Graph${i - 1}' }
		source.write_string('struct Graph${i} {
left &${previous} = unsafe { nil }
right &${previous} = unsafe { nil }
value string
}
')
	}
	source.write_string('fn make_root() &Graph23 { return &Graph23{ value: "alive" } }
fn main() {
root := make_root()
gc_collect()
assert root.value == "alive"
println(root.value)
}
')
	return source.str()
}

fn test_recursive_type_graph_codegen_stays_bounded_with_and_without_gc() {
	root := os.join_path(os.vtmp_dir(), 'v3_recursive_type_graph_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'graph.v')
	os.write_file(source, recursive_type_graph_source())!
	for mode in ['none', 'boehm_full_opt'] {
		// Exercise the reported 32-bit PowerPC target without needing its C toolchain.
		c_path := os.join_path(root, 'graph_${mode}.c')
		generated := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-gc', mode, '-os', 'macos',
			'-arch', 'ppc32', '-o', c_path, source], 120_000)
		assert generated.exit_code == 0, generated.output
		c_source := os.read_file(c_path)!
		for name in ['GraphLeaf', 'Graph0', 'Graph23'] {
			assert c_source.contains(name), 'missing graph type ${name}'
		}
		// Leave ample room for builtin growth while catching expanded type-name output.
		assert c_source.len < 8 * 1024 * 1024, 'C output expanded to ${c_source.len} bytes'
		mut executable := os.join_path(root, 'graph_${mode}')
		$if windows {
			executable += '.exe'
		}
		compiled := cmdexec.run_with_timeout(@VEXE, ['-new-compiler', '-gc', mode, '-o', executable,
			source], 120_000)
		assert compiled.exit_code == 0, compiled.output
		ran := cmdexec.run_with_timeout(executable, [], 30_000)
		assert ran.exit_code == 0, ran.output
		assert ran.output.trim_space() == 'alive', ran.output
	}
}
