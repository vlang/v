module types

import os
import v.parser
import v.pref

fn test_unsafe_annotation_belongs_to_the_resolved_function() {
	root := os.join_path(os.vtmp_dir(), 'unsafe_function_identity_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	dependency := os.join_path(root, 'dep.v')
	main := os.join_path(root, 'main.v')
	os.write_file(dependency, 'module dep
@[unsafe]
pub fn same() int { return 1 }
@[unsafe]
pub fn both() int { return 2 }
pub fn local_only() int { return 3 }
pub struct Item {}
@[unsafe]
pub fn (item Item) value() int { return 4 }
')!
	os.write_file(main, 'module main
import dep as remote
fn same() int { return 42 }
@[unsafe]
fn both() int { return 43 }
@[unsafe]
fn local_only() int { return 44 }
struct Item {}
fn (item Item) value() int { return 45 }
fn main() {
	assert same() == 42
	assert Item{}.value() == 45
	assert remote.local_only() == 3
	unsafe {
		assert remote.same() == 1
		assert remote.both() == 2
		assert both() == 43
		assert local_only() == 44
		assert remote.Item{}.value() == 4
	}
	assert remote.same() == 1
	assert remote.both() == 2
	assert both() == 43
	assert local_only() == 44
	assert remote.Item{}.value() == 4
}
')!
	for files in [[dependency, main], [main, dependency]] {
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_files(files)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		_ = tc.check_semantics_opt(false)
		assert tc.errors.len == 0, tc.errors.str()
		unsafe_notices := tc.notices.filter(it.msg.contains('must be called from an `unsafe` block'))
		assert unsafe_notices.len == 5, unsafe_notices.str()
		for name in ['remote.same', 'remote.both', 'both', 'local_only'] {
			assert unsafe_notices.any(it.msg == 'function `${name}` must be called from an `unsafe` block'), unsafe_notices.str()
		}
		assert unsafe_notices.any(it.msg == 'method `dep.Item.value` must be called from an `unsafe` block'), unsafe_notices.str()
	}
}
