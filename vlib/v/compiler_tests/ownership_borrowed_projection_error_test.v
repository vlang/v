import os
import v.cmdexec
import v.tempname

const borrowed_projection_uncloneable_source = r'
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

fn copy_borrowed_field(holder &Holder) {
	copied := holder.value
	_ = copied
}

fn check_borrowed_field() {
	holder := &Holder{
		value: Resource{
			values: ["owned"]
		}
	}
	copy_borrowed_field(holder)
}

fn (holder &Holder) take(value Resource) {
	_ = holder
	_ = value
}

fn main() {
	check_borrowed_field()
	holder := Holder{
		value: Resource{
			values: ["owned"]
		}
	}
	holder.take(holder.value)
}
'

fn test_uncloneable_borrowed_projection_copies_are_rejected() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}

	root := os.join_path(os.vtmp_dir(), 'v3_borrowed_projection_error_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, borrowed_projection_uncloneable_source)!
	lines := borrowed_projection_uncloneable_source.split_into_lines()
	expected := {
		'\tcopied := holder.value':    'cannot copy borrowed `Resource` value'
		'\tholder.take(holder.value)': 'cannot copy receiver-aliased `Resource` value'
	}
	reason := ': `Resource` requires ownership destruction but has no compatible `clone()` method; implement `IClone` or use a pointer'
	for mode in ['-no-parallel', ''] {
		out := cmdexec.run_in_merged_with_env(compiler, ['-nocache', '-ownership', '-d', 'ownership',
			'-nocolor', ...(os.split_args(mode) or { panic(err) }), 'run', source], '', {
			'VFLAGS':               ''
			'VTOOLS_CACHE_VERBOSE': ''
		})
		assert out.exit_code != 0, '${mode}: ${out.output}'
		diagnostics := out.output.split_into_lines()
		for statement, message in expected {
			line := lines.index(statement) + 1
			assert line > 0, statement
			assert diagnostics.any(it.contains('main.v:${line}:')
				&& it.ends_with(': error: ' + message + reason)), '${mode}: ${message}\n${out.output}'
		}
	}
}
