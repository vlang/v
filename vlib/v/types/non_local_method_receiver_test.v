module types

import os

const non_local_receiver_module = 'module foreign

pub enum Mode {
 first
 second
}

pub struct Record {}

pub struct Box[T] {
 value T
}

pub struct Pair[T, U] {
 first T
 second U
}

pub interface Readable {
 read() int
}

pub type Value = Record | int

pub type Count = int

pub fn (mode Mode) label() string {
 return "mode"
}
'

fn check_non_local_receiver(name string, source string, flags []string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'non_local_receiver_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'foreign')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'non_local_receiver' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'foreign', 'foreign.v'), non_local_receiver_module) or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	return os.exec([@VEXE, '-new-compiler', ...flags, '-check', root])
}

fn test_methods_on_imported_named_types_are_rejected() {
	for flags in [[]string{}, ['-no-parallel'], ['-prod']] {
		for receiver in ['foreign.Mode', '&foreign.Mode', '&&foreign.Mode', 'foreign.Record',
			'&foreign.Record', '&&foreign.Record', '&&&foreign.Record', 'mut foreign.Record',
			'mut &&foreign.Record', 'shared &&foreign.Record', 'foreign.Readable', '&&foreign.Readable',
			'foreign.Value', '&&foreign.Value', 'foreign.Count', '&&foreign.Count', 'foreign.Box[T]',
			'&&foreign.Box[T]'] {
			type_text := receiver.trim_string_left('mut ').trim_string_left('shared ')
			parameter := if receiver.starts_with('mut ') || receiver.starts_with('shared ') {
				'${receiver.all_before(' ')} value ${type_text}'
			} else {
				'value ${type_text}'
			}
			result := check_non_local_receiver('imported', 'import foreign

struct Record {}

pub fn (${parameter}) str() string {
 return "foreign"
}

fn main() {}
', flags)
			assert result.exit_code != 0, '${receiver}: ${result.output}'
			assert result.output.contains('cannot define new methods on non-local type ${type_text}'), result.output
			assert result.output.contains('Define an alias and use that instead'), result.output
		}
	}
}

fn test_import_alias_does_not_make_a_receiver_local() {
	result := check_non_local_receiver('import_alias', 'import foreign as other

pub fn (mod &other.Mode) str() string {
 return match mod {
  .first { "first" }
  .second { "second" }
 }
}

fn main() {}
', [])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot define new methods on non-local type &other.Mode'), result.output
}

fn test_alias_hints_declare_only_the_receivers_generic_parameters() {
	for flags in [[]string{}, ['-no-parallel'], ['-prod']] {
		for receiver, params in {
			'foreign.Box[T]':          'T'
			'&&foreign.Box[T]':        'T'
			'shared &&foreign.Box[T]': 'T'
			'foreign.Box[[]T]':        'T'
			'foreign.Pair[T, U]':      'T, U'
			'foreign.Pair[U, T]':      'T, U'
			'foreign.Pair[string, T]': 'T'
			'foreign.Box[int]':        ''
			'foreign.Record':          ''
		} {
			type_text := receiver.trim_string_left('shared ')
			parameter := if receiver.starts_with('shared ') {
				'shared value ${type_text}'
			} else {
				'value ${type_text}'
			}
			result := check_non_local_receiver('alias_hint', 'import foreign

fn (${parameter}) label[X](other X) string { return "local" }

fn main() {}
', flags)
			assert result.exit_code != 0, result.output
			alias_name := if params == '' { 'AliasName' } else { 'AliasName[${params}]' }
			assert result.output.contains('Define an alias and use that instead like `type ${alias_name} = ${type_text}`'), result.output
			if params != '' {
				assert result.output.contains('use `${alias_name}` as the receiver'), result.output
			}
			alias_declaration := result.output.all_after('instead like `').all_before('`')
			method_params := if params == '' { 'X' } else { '${params}, X' }
			fixed := check_non_local_receiver('alias_hint_fixed', 'import foreign

${alias_declaration}

fn (value ${alias_name}) label[${method_params}](other X) string { return "local" }

fn main() {}
', flags)
			assert fixed.exit_code == 0, fixed.output
		}
	}
}

fn test_selectively_imported_receiver_is_not_local() {
	result := check_non_local_receiver('selective_import', 'import foreign { Mode }

fn (mode Mode) label() string { return "foreign" }

fn main() {}
', [])
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot define new methods on non-local type Mode'), result.output
}

fn test_local_aliases_and_same_named_types_can_have_methods() {
	result := check_non_local_receiver('local', 'import foreign

type LocalMode = foreign.Mode
type LocalRecord = foreign.Record
type LocalCount = foreign.Count

struct Record {}

fn (value LocalMode) label() string { return "local mode" }
fn (value &LocalRecord) label() string { return "local record" }
fn (value LocalCount) label() string { return "local count" }
fn (value Record) label() string { return "record" }

fn main() {
 assert foreign.Mode.first.label() == "mode"
 assert LocalMode(foreign.Mode.first).label() == "local mode"
 assert LocalRecord(foreign.Record{}).label() == "local record"
 assert LocalCount(foreign.Count(1)).label() == "local count"
 assert Record{}.label() == "record"
}
', [])
	assert result.exit_code == 0, result.output
}

fn test_unknown_receivers_keep_the_unknown_type_diagnostic() {
	for receiver in ['Abc', 'foreign.Missing', '&foreign.Missing', '&&foreign.Missing'] {
		result := check_non_local_receiver('unknown', 'import foreign

fn (value ${receiver}) label() string { return "unknown" }

fn main() {}
', [])
		assert result.exit_code != 0, result.output
		assert result.output.contains('unknown type'), result.output
		assert !result.output.contains('cannot define new methods on non-local type'), result.output
	}
}

fn test_local_receiver_ownership_across_files() {
	root := os.join_path(os.vtmp_dir(), 'local_receiver_files_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'counter')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'local_receiver_files' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'counter', 'data.v'), non_local_receiver_module.replace('module foreign',
		'module counter')) or { panic(err) }
	mut methods := 'module counter\n\npub fn (value Record) read() int { return 1 }\n'
	for receiver in ['Record', 'Mode', 'Count', 'Readable', 'Value', 'Box[T]'] {
		methods += 'pub fn (value ${receiver}) description() string { return "local" }\n'
	}
	os.write_file(os.join_path(root, 'counter', 'methods.v'), methods) or { panic(err) }
	main_file := os.join_path(root, 'main.v')
	for module_header in ['', 'module main\n'] {
		os.write_file(os.join_path(root, 'data.v'), '${module_header}struct Local {}
type LocalAlias = Local
') or { panic(err) }
		os.write_file(os.join_path(root, 'methods.v'), '${module_header}
fn (value &&Local) description() string { return "local" }
fn (value &&LocalAlias) description() string { return "alias" }
fn (shared value &&Local) shared_description() string { return "shared" }
') or { panic(err) }
		os.write_file(main_file, '${module_header}import counter

fn main() {
 assert counter.Record{}.description() == "local"
 assert counter.Mode.first.description() == "local"
 assert counter.Count(1).description() == "local"
 assert counter.Readable(counter.Record{}).description() == "local"
 assert counter.Value(counter.Record{}).description() == "local"
 assert counter.Box[int]{}.description() == "local"
}
') or { panic(err) }
		for flags in [[]string{}, ['-no-parallel'], ['-prod']] {
			result := os.exec([@VEXE, '-new-compiler', ...flags, '-check', root])
			assert result.exit_code == 0, result.output
		}
	}
}
