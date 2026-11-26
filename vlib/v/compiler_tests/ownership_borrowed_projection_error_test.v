import os
import v.cmdexec
import v.tempname

const borrowed_projection_uncloneable_prelude = r'
interface Drop {
mut:
	drop()
}

struct Resource implements Drop {
mut:
	values []string
}

fn (mut value Resource) drop() {
	value.values = []
}

struct Holder {
	value Resource
}
'

fn assert_uncloneable_borrowed_copy_error(compiler string, case_name string, body string, expected string) {
	root := os.join_path(os.vtmp_dir(), 'v3_borrowed_projection_error_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_borrowed_projection_error_${case_name}.v')
	os.write_file(source, borrowed_projection_uncloneable_prelude + body) or { panic(err) }
	for mode in ['-no-parallel', ''] {
		out := cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-ownership', '-d', 'ownership',
			...(os.split_args(mode) or { panic(err) }), 'run', source], '', {
			'VFLAGS':               ''
			'VTOOLS_CACHE_VERBOSE': ''
		})
		assert out.exit_code != 0, '${case_name}: ${out.output}'
		assert out.output.contains(expected), '${case_name}: ${out.output}'
		assert out.output.contains('requires ownership destruction but has no compatible `clone()` method'), '${case_name}: ${out.output}'
		assert out.output.contains('implement `IClone` or use a pointer'), '${case_name}: ${out.output}'
	}
}

fn test_uncloneable_borrowed_projection_copies_are_rejected() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}

	assert_uncloneable_borrowed_copy_error(compiler, 'field', r'
fn copy_borrowed_field(holder &Holder) {
	copied := holder.value
	_ = copied
}

fn main() {
	holder := &Holder{
		value: Resource{
			values: ["owned"]
		}
	}
	copy_borrowed_field(holder)
}
', 'cannot copy borrowed `Resource` value')

	assert_uncloneable_borrowed_copy_error(compiler, 'receiver', r'
fn (holder &Holder) take(value Resource) {
	_ = holder
	_ = value
}

fn main() {
	holder := Holder{
		value: Resource{
			values: ["owned"]
		}
	}
	holder.take(holder.value)
}
', 'cannot copy receiver-aliased `Resource` value')
}
