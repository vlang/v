module types

import os

fn test_enum_append_context_rejects_incompatible_branches() {
	root := os.join_path(os.vtmp_dir(), 'v3_enum_append_context_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for i, expr in ['if yes { .first } else { 7 }',
		'match yes { true { .first } false { Other.second } }', 'if yes { .missing } else { .second }'] {
		path := os.join_path(root, 'invalid_${i}.v')
		os.write_file(path, 'enum Mode { first second }
enum Other { first second }
fn check(yes bool) {
 mut modes := []Mode{}
 modes << ${expr}
}
fn main() { check(true) }
')!
		result := os.exec([@VEXE, '-check', path])
		assert result.exit_code != 0, result.output
		assert result.output.contains('error:'), result.output
	}
}
