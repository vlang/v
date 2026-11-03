module types

import os

fn check_match_source(name string, source string) os.Result {
	path := os.join_path(os.vtmp_dir(), 'v3_match_const_${name}_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, source) or { panic(err) }
	return os.exec([@VEXE, '-check', path])
}

fn test_const_case_before_an_overlapping_range_is_accepted() {
	result := check_match_source('before_range', 'const lf_char = 0x0A
fn classify(c u32) int {
 return match c {
  lf_char { 1 }
  0...0x1F { 2 }
  else { 3 }
 }
}
fn main() { println(classify(10)) }
')
	assert result.exit_code == 0, result.output
}

fn test_repeated_const_case_is_rejected() {
	result := check_match_source('repeated', 'const lf_char = 0x0A
fn classify(c u32) int {
 return match c {
  lf_char { 1 }
  lf_char { 2 }
  else { 3 }
 }
}
fn main() { println(classify(10)) }
')
	assert result.exit_code != 0, result.output
	assert result.output.contains('match case `lf_char` is handled more than once'), result.output
}

fn test_literal_case_inside_a_later_range_is_rejected() {
	result := check_match_source('literal_range', 'fn classify(c u32) int {
 return match c {
  0x0A { 1 }
  0...0x1F { 2 }
  else { 3 }
 }
}
fn main() { println(classify(10)) }
')
	assert result.exit_code != 0, result.output
	assert result.output.contains('match case `10` is handled more than once'), result.output
}
