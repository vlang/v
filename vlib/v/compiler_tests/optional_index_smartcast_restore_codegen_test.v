import os
import v.tempname

fn test_optional_index_keeps_smartcasts_recreated_after_ancestor_write() {
	dir := os.join_path(os.vtmp_dir(), 'v3_optional_index_smartcast_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'v3_optional_index_smartcast_restore_input')
	src := bin + '.v'
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

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output

	reassigned_bin := os.join_path(dir, 'v3_optional_index_smartcast_reassignment_input')
	reassigned_src := reassigned_bin + '.v'
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

	reassigned_compile := os.exec([@VEXE, reassigned_src, '-b', 'c', '-o', reassigned_bin])
	assert reassigned_compile.exit_code == 0, reassigned_compile.output
	assert !reassigned_compile.output.contains('C compilation failed'), reassigned_compile.output

	reassigned_run := os.exec([reassigned_bin])
	assert reassigned_run.exit_code == 0, reassigned_run.output
	assert reassigned_run.output.trim_space() == 'ok', reassigned_run.output
}
