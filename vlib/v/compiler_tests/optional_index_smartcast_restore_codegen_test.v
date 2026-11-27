import os
import v.tempname

fn test_optional_index_restores_smartcasts_after_writes() {
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
			println("ancestor write ok")
		}
	}
	check_index_reassignment()
}

struct IndexedValue {
	value int
}

struct IndexedHolder {
mut:
	value ?IndexedValue
}

fn check_index_reassignment() {
	mut h := IndexedHolder{}
	h.value = IndexedValue{
		value: 1
	}
	opts := []?int{len: 1}
	if h.value != none {
		assert opts[if true {
			h.value = IndexedValue{
				value: 2
			}
			0
		} else {
			h.value = IndexedValue{
				value: 3
			}
			0
		}] == none
		assert h.value.value == 2
		println("index reassignment ok")
	}
}
')!

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['ancestor write ok', 'index reassignment ok'], run.output
}
