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
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('handled more than once'), result.output
}
