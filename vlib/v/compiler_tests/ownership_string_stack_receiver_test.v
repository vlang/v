import os

fn test_ownership_stack_string_receiver_views_keep_local_storage_diagnostics() {
	root := os.join_path(os.vtmp_dir(), 'ownership_stack_string_receivers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for fixture in [
		'fn escaped_vstring() string {
 local := [u8(97), 98, 99, 0]!
 return unsafe { (&local[0]).vstring() }
}
fn main() {}
',
		'fn escaped() string {
 local := [u8(97), 98, 99, 0]!
 view := unsafe { (&local[0]).vstring() }
 return view
}
fn main() {}
',
		'struct Holder { text string }
fn escaped() Holder {
 local := [u8(97), 98, 99, 0]!
 holder := Holder{text: unsafe { (&local[0]).vstring() }}
 return holder
}
fn main() {}
',
	] {
		os.write_file(source, fixture)!
		for mode in ['-no-parallel', ''] {
			result := run_owned_string_storage(source, mode, '-check')
			assert result.exit_code != 0, '${mode}: ${result.output}'
			assert result.output.contains('cannot return a reference to local storage `local`'), '${mode}: ${result.output}'
		}
	}
}

fn test_ownership_copied_string_carriers_and_receiver_results_keep_clones() {
	root := os.join_path(os.vtmp_dir(), 'ownership_copied_string_receivers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, "struct Holder { text string }
fn (holder &Holder) view() string { return holder.text }
fn carrier() (Holder, voidptr) {
 local := 'carrier'.to_owned()
 ptr := &local
 return Holder{text: *ptr}, unsafe { voidptr(local.str) }
}
fn wrapper() (string, voidptr) {
 local := Holder{text: 'wrapper'.to_owned()}
 return local.view(), unsafe { voidptr(local.text.str) }
}
fn sliced() (string, voidptr) {
 local := 'prefix/suffix'.to_owned()
 return unsafe { local.substr_unsafe(7, local.len) }, unsafe { voidptr(local.str + 7) }
}
fn main() {
 held, carrier_source := carrier()
 assert held.text == 'carrier'
 assert unsafe { voidptr(held.text.str) != carrier_source }
 wrapped, wrapper_source := wrapper()
 assert wrapped == 'wrapper'
 assert unsafe { voidptr(wrapped.str) != wrapper_source }
 escaped, slice_source := sliced()
 assert escaped == 'suffix'
 assert unsafe { voidptr(escaped.str) != slice_source }
 println('ok')
}
")!
	for mode in ['-no-parallel', ''] {
		result := run_owned_string_storage(source, mode, 'run')
		assert result.exit_code == 0, '${mode}: ${result.output}'
		assert result.output.trim_space() == 'ok', result.output
	}
}

fn run_owned_string_storage(source string, mode string, command string) os.Result {
	mut args := [@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-ownership', '-d',
		'ownership']
	if mode != '' { args << mode }
	args << [command, source]
	return os.exec(args)
}
