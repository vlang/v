import os
import v.tempname

fn test_pointer_to_interface_stringification_uses_pointer_path() {
	root := os.join_path(os.vtmp_dir(), 'v3_pointer_interface_str_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src_path := os.join_path(root, 'v3_source.v')
	bin_path := os.join_path(root, 'v3_bin')
	os.write_file(src_path,
		"interface Named {\n\tstr() string\n}\n\nstruct Item {}\n\nfn (i Item) str() string {\n\treturn 'item'\n}\n\nfn main() {\n\tmut value := Named(Item{})\n\tptr := &value\n\ttext := '\${ptr}'\n\tprintln(text.len > 0)\n}\n") or {
		panic(err)
	}
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true'
}

fn test_pointer_receiver_str_uses_pointer_argument() {
	root := os.join_path(os.vtmp_dir(), 'v3_pointer_interface_str_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src_path := os.join_path(root, 'v3_pointer_receiver_source.v')
	bin_path := os.join_path(root, 'v3_pointer_receiver_bin')
	os.write_file(src_path, "struct Item {
	value int
}

fn (i &Item) str() string {
	return 'ptr:' + int_str(i.value)
}

fn main() {
	item := Item{
		value: 9
	}
	ptr := &item
	println('\${ptr}')
	println(ptr.str())
}
") or {
		panic(err)
	}
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '&ptr:9\nptr:9'
}

fn test_custom_pointer_interpolation_guards_nil() {
	root := os.join_path(os.vtmp_dir(), 'v3_pointer_interface_str_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	src_path := os.join_path(root, 'v3_nil_custom_source.v')
	bin_path := os.join_path(root, 'v3_nil_custom_bin')
	os.write_file(src_path, "struct PtrItem {
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

fn main() {
	ptr_item := unsafe { &PtrItem(nil) }
	val_item := unsafe { &ValItem(nil) }
	println('\${ptr_item}')
	println('\${val_item}')
}
") or {
		panic(err)
	}
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', bin_path])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin_path])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '&nil\n&nil'
}
