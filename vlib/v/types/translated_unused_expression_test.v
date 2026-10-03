module types

import os

fn test_translated_unused_expressions_preserve_must_use_warnings() {
	root := os.join_path(os.vtmp_dir(), 'translated_must_use_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	declarations := 'module main
struct Record {}
@[must_use]
fn required() int { return 1 }
@[must_use]
fn (r Record) required() int { return 2 }
fn ordinary() int { return 3 }
'
	for translated in [false, true] {
		prefix := if translated { '@[translated]\n' } else { '' }
		for body in ['required()', 'item.required()', '(required())', '(item.required())'] {
			os.write_file(path, '${prefix}${declarations}
fn main() { item := Record{}; ${body}; ordinary() }
')!
			for flags in ['', '-W'] {
				mode := if flags == '' { '-o ${os.quoted_path(path + '.c')}' } else { '-check' }
				result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }),
					...(os.split_args(mode) or { panic(err) }), path])
				assert result.exit_code == if flags == '' { 0 } else { 1 }, result.output
				assert result.output.count('return value must be used') == 1, result.output
				kind := if body.contains('item.') { 'method' } else { 'function' }
				assert result.output.contains('${kind} `required` was tagged with `@[must_use]`'), result.output
				assert !result.output.contains('expression evaluated but not used'), result.output
			}
		}
		os.write_file(path, '${prefix}${declarations}
fn main() {
 item := Record{}
 _ = required()
 _ = item.required()
 println(required() + item.required())
 ordinary()
}
')!
		used := os.exec([@VEXE, '-W', '-check', path])
		assert used.exit_code == 0, used.output
		os.write_file(path, '${prefix}module main\nfn main() { 1 + 2 }\n')!
		unused := os.exec([@VEXE, '-check', path])
		if translated {
			assert unused.exit_code == 0, unused.output
			assert !unused.output.contains('evaluated but not used'), unused.output
		} else {
			assert unused.exit_code != 0, unused.output
			assert unused.output.contains('evaluated but not used'), unused.output
		}
	}
}
