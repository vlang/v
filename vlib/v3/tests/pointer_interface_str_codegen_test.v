import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn tmp_pointer_interface_str_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
}

fn build_v3_pointer_interface_str() string {
	v3_bin := tmp_pointer_interface_str_path('pointer_interface_str')
	build :=
		os.execute('${os.quoted_path(vexe)} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_pointer_to_interface_stringification_uses_pointer_path() {
	v3_bin := build_v3_pointer_interface_str()
	src_path := '${tmp_pointer_interface_str_path('source')}.v'
	bin_path := tmp_pointer_interface_str_path('bin')
	os.write_file(src_path,
		"interface Named {\n\tstr() string\n}\n\nstruct Item {}\n\nfn (i Item) str() string {\n\treturn 'item'\n}\n\nfn main() {\n\tmut value := Named(Item{})\n\tptr := &value\n\ttext := '\${ptr}'\n\tprintln(text.len > 0)\n}\n") or {
		panic(err)
	}
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true'
}

fn test_explicit_pointer_str_keeps_reference_prefix() {
	v3_bin := build_v3_pointer_interface_str()
	src_path := '${tmp_pointer_interface_str_path('explicit_source')}.v'
	bin_path := tmp_pointer_interface_str_path('explicit_bin')
	os.write_file(src_path, "struct Item {
	value int
}

fn (i Item) str() string {
	return 'item:' + int_str(i.value)
}

fn main() {
	item := Item{
		value: 7
	}
	ptr := &item
	println(ptr.str())
}
") or {
		panic(err)
	}
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '&item:7'
}

fn test_pointer_receiver_str_uses_pointer_argument() {
	v3_bin := build_v3_pointer_interface_str()
	src_path := '${tmp_pointer_interface_str_path('pointer_receiver_source')}.v'
	bin_path := tmp_pointer_interface_str_path('pointer_receiver_bin')
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
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ptr:9\n&ptr:9'
}

fn test_custom_pointer_interpolation_guards_nil() {
	v3_bin := build_v3_pointer_interface_str()
	src_path := '${tmp_pointer_interface_str_path('nil_custom_source')}.v'
	bin_path := tmp_pointer_interface_str_path('nil_custom_bin')
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
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '&nil\n&nil'
}
