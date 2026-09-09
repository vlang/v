import os

const optional_guard_move_vexe = @VEXE
const optional_guard_move_tests_dir = os.dir(@FILE)
const optional_guard_move_v3_dir = os.dir(optional_guard_move_tests_dir)
const optional_guard_move_vlib_dir = os.dir(optional_guard_move_v3_dir)
const optional_guard_move_v3_src = os.join_path(optional_guard_move_v3_dir, 'v3.v')

fn test_optional_guard_clears_only_moved_source_wrapper() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_optional_guard_move_${pid}')
	source := os.join_path(os.temp_dir(), 'v3_optional_guard_move_input_${pid}.v')
	output := os.join_path(os.temp_dir(), 'v3_optional_guard_move_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(source) or {}
		os.rm(output) or {}
		os.rm(output + '.c') or {}
	}
	build :=
		os.execute('${optional_guard_move_vexe} -gc none -d ownership -path "${optional_guard_move_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${optional_guard_move_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(source, "interface Drop {
mut:
\tdrop()
}

struct Holder implements Drop {
mut:
\tvalue ?[]u8
}

struct GenericHolder[T] {
mut:
\tvalue ?T
}

struct Cursor {
mut:
\tcalls int
}

fn (mut c Cursor) next() int {
\tindex := c.calls
\tc.calls++
\treturn index
}

fn (mut h Holder) drop() {
\t_ = h
}

fn (h &Holder) borrowed_len() int {
\tif bytes := h.value {
\t\treturn bytes.len
\t}
\treturn 0
}

fn (mut h Holder) clear() {
\tif bytes := h.value {
\t\tunsafe { bytes.free() }
\t\th.value = none
\t}
}

@[manualfree]
fn (mut h GenericHolder[T]) clear[T]() {
\tif bytes := h.value {
\t\tunsafe { bytes.free() }
\t\th.value = none
\t}
}

fn main() {
\tmut holder := Holder{
\t\tvalue: [u8(1), 2]
\t}
\tassert holder.borrowed_len() == 2
\tassert holder.value != none
\tholder.clear()
\tassert holder.value == none
\tmut generic_holder := GenericHolder[[]u8]{
\t\tvalue: [u8(3), 4]
\t}
\tgeneric_holder.clear()
\tassert generic_holder.value == none
\tmut holders := [Holder{
\t\tvalue: [u8(5), 6]
\t}, Holder{
\t\tvalue: [u8(7), 8]
\t}]
\tmut cursor := Cursor{}
\tif bytes := holders[cursor.next()].value {
\t\tassert bytes == [u8(5), 6]
\t}
\tassert cursor.calls == 1
\tprintln('ok')
}
") or {
		panic(err)
	}
	compile :=
		os.execute('${v3_bin} -ownership -d ownership -nocache -no-parallel -o ${output} ${source}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(output)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
