import os

const borrowed_projection_error_vexe = @VEXE
const borrowed_projection_error_tests_dir = os.dir(@FILE)
const borrowed_projection_error_v3_dir = os.dir(borrowed_projection_error_tests_dir)
const borrowed_projection_error_vlib_dir = os.dir(borrowed_projection_error_v3_dir)
const borrowed_projection_error_v3_src = os.join_path(borrowed_projection_error_v3_dir, 'v3.v')
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

fn assert_uncloneable_borrowed_copy_error(v3_bin string, case_name string, body string, expected string) {
	source := os.join_path(os.temp_dir(), 'v3_borrowed_projection_error_${case_name}_${os.getpid()}.v')
	os.rm(source) or {}
	defer {
		os.rm(source) or {}
	}
	os.write_file(source, borrowed_projection_uncloneable_prelude + body) or { panic(err) }
	for mode in ['-no-parallel', ''] {
		out := os.execute('${v3_bin} -nocache -ownership -d ownership ${mode} run ${source}')
		assert out.exit_code != 0, '${case_name}: ${out.output}'
		assert out.output.contains(expected), '${case_name}: ${out.output}'
		assert out.output.contains('requires ownership destruction but has no compatible `clone()` method'), '${case_name}: ${out.output}'
		assert out.output.contains('implement `IClone` or use a pointer'), '${case_name}: ${out.output}'
	}
}

fn test_uncloneable_borrowed_projection_copies_are_rejected() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_borrowed_projection_error_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	defer {
		os.rm(v3_bin) or {}
	}
	build :=
		os.execute('${borrowed_projection_error_vexe} -nocache -gc none -d ownership -path "${borrowed_projection_error_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${borrowed_projection_error_v3_src}')
	assert build.exit_code == 0, build.output

	assert_uncloneable_borrowed_copy_error(v3_bin, 'field', r'
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

	assert_uncloneable_borrowed_copy_error(v3_bin, 'receiver', r'
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
