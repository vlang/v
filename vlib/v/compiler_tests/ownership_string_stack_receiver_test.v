import os

fn test_ownership_stack_string_receiver_views_keep_local_storage_diagnostics() {
	root := os.join_path(os.vtmp_dir(), 'ownership_stack_string_receivers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixtures := {
		'direct.v': 'fn escaped_vstring() string {
 local := [u8(97), 98, 99, 0]!
 return unsafe { (&local[0]).vstring() }
}
'
		'view.v':   'fn escaped_view() string {
 local := [u8(97), 98, 99, 0]!
 view := unsafe { (&local[0]).vstring() }
 return view
}
'
		'holder.v': 'struct Holder { text string }
fn escaped_holder() Holder {
 local := [u8(97), 98, 99, 0]!
 holder := Holder{text: unsafe { (&local[0]).vstring() }}
 return holder
}
'
	}
	expected := 'cannot return a reference to local storage `local`'
	// Custom flags can limit diagnostics or change their format.
	if os.getenv('VFLAGS').trim_space() !in ['', '-gc none'] {
		source := os.join_path(root, 'main.v')
		for _, fixture in fixtures {
			os.write_file(source, fixture + 'fn main() {}\n')!
			for mode in ['-no-parallel', ''] {
				result := run_owned_string_storage(source, mode, '-check')
				assert result.exit_code != 0, result.output
				assert result.output.contains(expected), result.output
			}
		}
		return
	}
	os.write_file(os.join_path(root, 'main.v'), 'fn main() {}\n')!
	for file, fixture in fixtures {
		os.write_file(os.join_path(root, file), fixture)!
	}
	for mode in ['-no-parallel', ''] {
		result := run_owned_string_storage(root, mode, '-check', '-nocolor')
		assert result.exit_code != 0, result.output
		diagnostics := result.output.split_into_lines()
		for file, _ in fixtures {
			assert diagnostics.any(it.contains('${file}:') && it.ends_with(': error: ' + expected)), result.output
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

fn run_owned_string_storage(source string, mode string, command string, flags ...string) os.Result {
	mut args := [@VEXE, '-no-retry-compilation', '-nocache', '-ownership', '-d', 'ownership']
	if mode != '' { args << mode }
	args << [...flags, command, source]
	return os.exec(args)
}
