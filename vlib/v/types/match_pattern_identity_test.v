module types

import os

fn test_identical_nested_match_conditions_are_still_rejected() {
	path := os.join_path(os.vtmp_dir(), 'v3_match_pattern_identity_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'struct Params { values []int }
fn choose(p Params) int {
 return match true {
  p.values.len > 0 { 1 }
  p.values.len > 0 { 2 }
  else { 0 }
 }
}
fn main() { println(choose(Params{})) }
')!
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('handled more than once'), result.output
}

fn test_cast_target_in_index_selector_distinguishes_match_conditions() {
	path := os.join_path(os.vtmp_dir(), 'v3_cast_index_pattern_identity_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, 'struct A { x int }
struct B { x int }
type Choice = A | B
struct Entry { values []int }
fn choose(value Choice, lookup map[int]Entry) int {
 return match true {
  lookup[(value as A).x].values.len > 0 { 1 }
  lookup[(value as B).x].values.len > 0 { 2 }
  else { 0 }
 }
}
')!
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code == 0, result.output
}
