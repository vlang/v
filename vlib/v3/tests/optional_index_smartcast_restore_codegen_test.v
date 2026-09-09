import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_optional_index_keeps_smartcasts_recreated_after_ancestor_write() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_index_smartcast_restore_test_${pid}')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_optional_index_smartcast_restore_input_${pid}.v')
	os.write_file(src, 'struct Foo {
	value int
}

struct Bar {}

type Value = Bar | Foo

struct Holder {
mut:
	value ?Value
}

fn main() {
	mut h := Holder{}
	h.value = Foo{
		value: 1
	}
	if h.value != none {
		h = Holder{}
	}
	h.value = Foo{
		value: 2
	}
	opts := []?int{len: 1}
	if h.value != none {
		if h.value is Foo {
			assert opts[0] == none
			assert h.value.value == 2
		}
	}
	println("ok")
}
')!

	bin := os.join_path(os.temp_dir(), 'v3_optional_index_smartcast_restore_input_${pid}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output

	reassigned_src := os.join_path(os.temp_dir(), 'v3_optional_index_smartcast_reassignment_input_${pid}.v')
	os.write_file(reassigned_src, 'struct Foo {
	value int
}

struct Holder {
mut:
	value ?Foo
}

fn main() {
	mut h := Holder{}
	h.value = Foo{
		value: 1
	}
	opts := []?int{len: 1}
	if h.value != none {
		assert opts[if true {
			h.value = Foo{
				value: 2
			}
			0
		} else {
			h.value = Foo{
				value: 3
			}
			0
		}] == none
		assert h.value.value == 2
	}
	println("ok")
}
')!

	reassigned_bin := os.join_path(os.temp_dir(), 'v3_optional_index_smartcast_reassignment_input_${pid}')
	reassigned_compile := os.execute('${v3_bin} ${reassigned_src} -b c -o ${reassigned_bin}')
	assert reassigned_compile.exit_code == 0, reassigned_compile.output
	assert !reassigned_compile.output.contains('C compilation failed'), reassigned_compile.output

	reassigned_run := os.execute(reassigned_bin)
	assert reassigned_run.exit_code == 0, reassigned_run.output
	assert reassigned_run.output.trim_space() == 'ok', reassigned_run.output
}
