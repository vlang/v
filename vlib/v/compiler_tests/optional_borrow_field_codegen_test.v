import os
import v.tempname

fn test_optional_field_payload_can_be_borrowed() {
	dir := os.join_path(os.vtmp_dir(), 'v3_optional_borrow_field_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	src := os.join_path(dir, 'v3_optional_borrow_field_input.v')
	out := os.join_path(dir, 'v3_optional_borrow_field_program')
	os.write_file(src, "struct Holder {
	value ?string
}

fn (holder &^a Holder) value_ref[^a]() ?&^a string {
	if holder.value != none {
		return unsafe { &holder.value? }
	}
	return none
}

fn main() {
	holder := Holder{
		value: 'borrowed'
	}
	assert *(holder.value_ref() or { panic('missing value') }) == 'borrowed'
}
") or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-b', 'c', '-o', out, src])
	assert compile.exit_code == 0, compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
}
