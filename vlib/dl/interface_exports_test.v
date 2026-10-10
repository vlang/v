module dl

import os

type InterfaceFixtureReader = fn () int

fn registered_interface_table_refs(entries voidptr) int {
	mut table := g_dl_interface_exports
	for table != unsafe { nil } {
		if voidptr(table.entries) == entries {
			return table.refs
		}
		table = table.next
	}
	return 0
}

fn build_empty_interface_table_library(directory string, name string, value int) !string {
	source := os.join_path(directory, '${name}.v')
	library := os.join_path(directory, get_libname(name))
	// An unused interface emits a table containing only its terminating entry.
	os.write_file(source, 'module main
interface Unused {
	read() int
}
@[export: "read_value"]
pub fn read_value() int {
	return ${value}
}
')!
	result := os.exec([@VEXE, '-b', 'c', '-shared', '-o', library, source])
	if result.exit_code != 0 {
		return error(result.output)
	}
	return library
}

fn test_distinct_interface_export_tables_keep_independent_library_references() ! {
	directory := os.join_path(os.vtmp_dir(), 'dl-interface-tables-${os.getpid()}')
	os.mkdir_all(directory)!
	defer {
		os.rmdir_all(directory) or {}
	}
	first_library := build_empty_interface_table_library(directory, 'first', 11)!
	second_library := build_empty_interface_table_library(directory, 'second', 22)!
	first := open_opt(first_library, rtld_now | rtld_local)!
	first_again := open_opt(first_library, rtld_now | rtld_local)!
	second := open_opt(second_library, rtld_now | rtld_local)!
	assert first == first_again
	assert first != second
	first_entries := sym_opt(first, interface_exports_symbol)!
	second_entries := sym_opt(second, interface_exports_symbol)!
	assert first_entries != second_entries
	assert registered_interface_table_refs(first_entries) == 2
	assert registered_interface_table_refs(second_entries) == 1
	first_read := InterfaceFixtureReader(sym_opt(first, 'read_value')!)
	second_read := InterfaceFixtureReader(sym_opt(second, 'read_value')!)
	assert first_read() == 11
	assert second_read() == 22
	assert close(first)
	assert registered_interface_table_refs(first_entries) == 1
	assert first_read() == 11
	assert second_read() == 22
	assert close(first_again)
	assert registered_interface_table_refs(first_entries) == 0
	assert registered_interface_table_refs(second_entries) == 1
	assert second_read() == 22
	assert close(second)
	assert registered_interface_table_refs(second_entries) == 0
}
