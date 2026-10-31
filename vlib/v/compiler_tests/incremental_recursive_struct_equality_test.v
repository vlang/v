import os

fn test_incremental_cache_synthesizes_recursive_struct_equality() {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'incremental_recursive_struct_equality_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	source := 'module main
struct Node {
 name string
 children []Node
}
fn equal(left Node, right Node) bool {
 _ = left
 _ = right
 return false
}
fn main() {
 left := Node{name: "root", children: [Node{name: ["le", "af"].join("")}]}
 right := Node{name: "root", children: [Node{name: ["l", "eaf"].join("")}]}
 println(equal(left, right))
}
'
	os.write_file(path, source)!
	cache := os.join_path(root, 'cache')
	for phase in ['cold', 'warm'] {
		output := os.join_path(root, phase)
		// Explicit -b c disables module caching; the implicit C backend with an
		// explicit C compiler exercises the incremental cache instead.
		build := os.exec(['env', '-u', 'VEXE', '-u', 'VFLAGS', 'V3CACHE=${cache}', @VEXE, '-cc',
			'clang', '-show-timings', '-o', output, path])
		assert build.exit_code == 0, '${phase}: ${build.output}'
		run := os.exec([output])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'false', run.output
	}
	os.write_file(path, source.replace(' _ = left\n _ = right\n return false',
		' return left == right'))!
	for phase in ['incremental', 'warm_equality'] {
		output := os.join_path(root, phase)
		build := os.exec(['env', '-u', 'VEXE', '-u', 'VFLAGS', 'V3CACHE=${cache}', @VEXE, '-cc',
			'clang', '-show-timings', '-o', output, path])
		assert build.exit_code == 0, '${phase}: ${build.output}'
		if phase == 'incremental' {
			assert build.output.contains('transform (incremental)'), build.output
			assert build.output.contains('cgen (incremental)'), build.output
		}
		run := os.exec([output])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'true', run.output
	}
}
