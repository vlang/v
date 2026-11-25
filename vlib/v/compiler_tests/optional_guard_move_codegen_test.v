// vtest ownership
import os
import v.cmdexec
import v.tempname

fn test_optional_guard_clears_only_moved_source_wrapper() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_optional_guard_move_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'v3_optional_guard_move_input.v')
	output := os.join_path(root, 'v3_optional_guard_move_program')
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
		cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-nocache',
			'-no-parallel', '-o', output, source], '', {
			'VFLAGS':               ''
			'VTOOLS_CACHE_VERBOSE': ''
		})
	assert compile.exit_code == 0, compile.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
