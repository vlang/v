import os

fn test_ownership_allocating_string_ranges_free_each_buffer_once() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_range_cleanup_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Holder { text string }
fn field_slice(holder &Holder) string { return holder.text[1..4] }
fn literal_slice() string { return "abcdef"[1..4] }
fn direct_slice(text &string) string { return (*text)[1..4] }
fn named_slice(text &string) string {
	slice := (*text)[1..4]
	return slice
}
fn consume(value string) { assert value.len > 0 }
fn main() {
	owner := "abcdef".to_owned()
	eprintln("BEGIN loops")
	for _ in 0 .. 3 {
		slice := owner[1..4]
		assert slice == "bcd"
		assert unsafe { slice.str != owner.str + 1 }
	}
	eprintln("END loops")
	eprintln("BEGIN returns")
	{
		direct := direct_slice(&owner)
		named := named_slice(&owner)
		holder := Holder{text: "abcdef"}
		field := field_slice(&holder)
		literal := literal_slice()
		assert direct == "bcd"
		assert named == "bcd"
		assert field == "bcd"
		assert literal == "bcd"
		assert unsafe { direct.str != owner.str + 1 && named.str != owner.str + 1 }
	}
	eprintln("END returns")
	eprintln("BEGIN calls")
	consume(owner[1..4])
	consume(direct_slice(&owner))
	eprintln("END calls")
	eprintln("BEGIN reassignment")
	{
		mut slice := owner[1..4]
		slice = owner[2..5]
		assert slice == "cde"
	}
	eprintln("END reassignment")
	eprintln("BEGIN reslice")
	{
		mut slice := owner[1..4]
		slice = slice[..2]
		assert slice == "bc"
	}
	eprintln("END reslice")
	eprintln("BEGIN conditional")
	{
		slice := if owner.len > 0 { owner[1..4] } else { owner[2..5] }
		assert slice == "bcd"
	}
	eprintln("END conditional")
	eprintln("BEGIN gated")
	{
		slice := owner#[-4..-1]
		assert slice == "cde"
	}
	eprintln("END gated")
	eprintln("BEGIN literal")
	{
		slice := "abcdef"[1..4]
		assert slice == "bcd"
	}
	eprintln("END literal")
	eprintln("BEGIN borrowed")
	{
		view := owner.substr_unsafe(1, 4)
		reference := &owner[1..4]
		assert view == "bcd"
		assert *reference == "bcd"
		assert unsafe { view.str == owner.str + 1 && reference.str == owner.str + 1 }
	}
	eprintln("END borrowed")
	eprintln("BEGIN source_move")
	{
		original := "abcdef".to_owned()
		slice := original[1..4]
		consume(original)
		assert slice == "bcd"
	}
	eprintln("END source_move")
	assert owner == "abcdef"
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-retry-compilation -no-memory-limit -nocache -ownership -gc none -cc clang -d trace_free ${mode} run ${os.quoted_path(source)}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
		for region, expected in {
			'loops':        3
			// Literal slices fold to static strings; runtime slices still own allocations.
			'returns':      3
			'calls':        2
			'reassignment': 2
			'reslice':      2
			'conditional':  1
			'gated':        1
			'literal':      0
			'borrowed':     0
			'source_move':  2
		} {
			trace := out.output.all_after('BEGIN ${region}\n').all_before('END ${region}\n')
			assert trace.count('free ptr:') == expected, '${mode} ${region}: ${out.output}'
		}
	}
}

fn test_ownership_allocating_string_ranges_transfer_ownership() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_range_moves_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for creation in ['owner[1..4]', 'owner#[-4..-1]', 'direct_slice(&owner)', 'named_slice(&owner)'] {
		os.write_file(source, 'fn direct_slice(text &string) string { return (*text)[1..4] }
fn named_slice(text &string) string {
	slice := (*text)[1..4]
	return slice
}
fn consume(value string) { _ = value }
fn main() {
	owner := "abcdef".to_owned()
	slice := ${creation}
	consume(slice)
	println(slice)
}
')!
		for mode in ['-no-parallel', ''] {
			out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -no-retry-compilation -no-memory-limit -nocache -ownership -gc none -cc clang ${mode} -check ${os.quoted_path(source)}')
			assert out.exit_code != 0, '${creation} ${mode}: ${out.output}'
			assert out.output.contains('use of moved value: `slice`'), '${creation} ${mode}: ${out.output}'
		}
	}
}
