import os
import v.tempname

fn test_pointer_stringification_runtime_fixtures() {
	root := os.join_path(os.vtmp_dir(), 'v3_pointer_interface_str_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src_path := os.join_path(root, 'v3_source.v')
	bin_path := os.join_path(root, 'v3_bin')
	os.write_file(src_path, "interface Named {
	str() string
}

struct NamedItem {}

fn (i NamedItem) str() string {
	return 'item'
}

fn check_pointer_to_interface() {
	mut value := Named(NamedItem{})
	ptr := &value
	text := '\${ptr}'
	println(text.len > 0)
}

struct Item {
	value int
}

fn (i &Item) str() string {
	return 'ptr:' + int_str(i.value)
}

fn check_pointer_receiver_str() {
	item := Item{
		value: 9
	}
	ptr := &item
	println('\${ptr}')
	println(ptr.str())
}

struct PtrItem {
	value int
}

struct ValItem {
	value int
}

fn (i &PtrItem) str() string {
	return 'ptr:' + int_str(i.value)
}

fn (i ValItem) str() string {
	return 'val:' + int_str(i.value)
}

fn check_nil_pointer_interpolation() {
	ptr_item := unsafe { &PtrItem(nil) }
	val_item := unsafe { &ValItem(nil) }
	println('\${ptr_item}')
	println('\${val_item}')
}

fn main() {
	check_pointer_to_interface()
	check_pointer_receiver_str()
	check_nil_pointer_interpolation()
}
") or {
		panic(err)
	}
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true\n&ptr:9\nptr:9\n&nil\n&nil', run.output
}
