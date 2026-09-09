import os

fn test_optional_field_payload_can_be_borrowed() {
	vexe := @VEXE
	v3_dir := os.dir(os.dir(@FILE))
	vlib_dir := os.dir(v3_dir)
	v3_src := os.join_path(v3_dir, 'v3.v')
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_borrow_field_compiler')
	src := os.join_path(os.temp_dir(), 'v3_optional_borrow_field_input.v')
	out := os.join_path(os.temp_dir(), 'v3_optional_borrow_field_program')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
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
	compile := os.execute('${v3_bin} -b c -o ${out} ${src}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
}
