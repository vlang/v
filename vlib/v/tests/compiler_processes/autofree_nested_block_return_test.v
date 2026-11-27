module main

import os
import v.cmdexec

// A `return` at the end of a nested `if`/`match` block must drop the block's locals
// once. The checker's drop list for that return already names them, so the block's
// own scope drops must not be added again.
fn test_autofree_nested_block_tail_return_drops_locals_once() {
	root := os.join_path(os.vtmp_dir(), 'autofree_nested_block_return_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Tracked { id int }
fn (item &Tracked) free() { println("drop \${item.id}") }
fn nested_if(n int) !int {
 if n > 0 {
  t := Tracked{n}
  if n > 100 { return t.id }
  return error("small")
 }
 return error("none")
}
fn nested_match(k int) !int {
 match k {
  1 {
   t := Tracked{10}
   if k > 100 { return t.id }
   return error("small")
  }
  else { return error("other") }
 }
}
fn tail_return(n int) !int {
 if n > 0 {
  t := Tracked{20}
  println("use \${t.id}")
  return error("e")
 }
 return 0
}
fn main() {
 nested_if(1) or {}
 nested_if(200) or {}
 nested_match(1) or {}
 tail_return(1) or {}
}
')!
	for serial in [false, true] {
		executable := os.join_path(root, 'program_${serial}')
		mut args := ['-autofree', '-nocache', '-gc', 'none', '-cc', @CCOMPILER, '-o', executable]
		if serial {
			args << '-no-parallel'
		}
		args << source
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, 'serial=${serial}: ${build.output}'
		run := cmdexec.run(executable, [])
		assert run.exit_code == 0, 'serial=${serial}: ${run.output}'
		assert run.output.trim_space() == 'drop 1\ndrop 200\ndrop 10\nuse 20\ndrop 20', 'serial=${serial}: ${run.output}'
	}
}
