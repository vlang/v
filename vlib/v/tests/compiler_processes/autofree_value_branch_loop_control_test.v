module main

import os
import v.cmdexec

fn test_autofree_value_branch_jumps_drop_only_initialized_locals() {
 cases := {
  'or_continue': 'get_value(i == 0) or { continue }'
  'or_break': 'get_value(i == 0) or { break }'
  'if_continue': 'if i == 0 { continue } else { 42 }'
  'if_break': 'if i == 0 { break } else { 42 }'
  'match_continue': 'match i { 0 { continue } else { 42 } }'
  'match_break': 'match i { 0 { break } else { 42 } }'
 }
 for name, expression in cases {
  root := os.join_path(os.vtmp_dir(), 'autofree_value_jump_${name}_${os.getpid()}')
  os.mkdir_all(root)!
  defer { os.rmdir_all(root) or {} }
  source := os.join_path(root, 'main.v')
  os.write_file(source, 'struct Tracked { id int }
fn (item &Tracked) free() { println("drop \${item.id}") }
fn get_value(fails bool) !int {
 if fails { return error("skip") }
 return 42
}
fn main() {
 for i in 0 .. 3 {
  before := Tracked{10 + i}
  _ := ${expression}
  middle := Tracked{20 + i}
  if i == 1 { continue }
  after := Tracked{30 + i}
  assert before.id + middle.id + after.id > 0
 }
}
')!
  for serial in [false, true] {
   executable := os.join_path(root, 'program_${serial}')
   mut args := ['-new-compiler', '-autofree', '-nocache', '-gc', 'none', '-cc', @CCOMPILER, '-o', executable]
   if serial { args << '-no-parallel' }
   args << source
   build := cmdexec.run(@VEXE, args)
   assert build.exit_code == 0, '${name} serial=${serial}: ${build.output}'
   run := cmdexec.run(executable, [])
   assert run.exit_code == 0, '${name} serial=${serial}: ${run.output}'
   expected := if name.ends_with('break') { 'drop 10' } else { 'drop 10\ndrop 21\ndrop 11\ndrop 32\ndrop 22\ndrop 12' }
   assert run.output.trim_space() == expected, '${name} serial=${serial}: ${run.output}'
  }
 }
}
