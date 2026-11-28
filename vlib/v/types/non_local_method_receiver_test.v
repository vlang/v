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
	return os.exec([@VEXE, ...flags, '-check', root])
}

// check_non_local_receiver_cases checks every source, a `main` file that imports `foreign`,
// as a module of one project. It returns the diagnostics of each case, in order.
fn check_non_local_receiver_cases(name string, sources []string, flags []string) (os.Result, [][]string) {
	root := os.join_path(os.vtmp_dir(), 'non_local_receivers_${name}_${os.getpid()}')
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
	mut imports := []string{cap: sources.len}
	mut calls := []string{cap: sources.len}
	for i, source in sources {
		module_name := 'case_${i}'
		assert source.contains('\nfn main() {}\n'), source
		os.mkdir_all(os.join_path(root, module_name)) or { panic(err) }
		os.write_file(os.join_path(root, module_name, '${module_name}.v'), 'module ${module_name}\n' +
			source.replace('\nfn main() {}\n', '\npub fn touch() {}\n')) or { panic(err) }
		imports << 'import ${module_name}'
		calls << '\t${module_name}.touch()'
	}
	os.write_file(os.join_path(root, 'main.v'), '${imports.join('\n')}\n\nfn main() {\n${calls.join('\n')}\n}\n') or {
		panic(err)
	}
	result := os.exec([@VEXE, ...flags, '-message-limit', '10000', '-check', root])
	lines := result.output.split_into_lines()
	mut diagnostics := [][]string{len: sources.len}
	for i in 0 .. sources.len {
		diagnostics[i] = lines.filter(it.contains('case_${i}.v:'))
	}
	return result, diagnostics
}

fn test_methods_on_imported_named_types_are_rejected() {
	receivers := ['foreign.Mode', '&foreign.Mode', '&&foreign.Mode', 'foreign.Record', '&foreign.Record',
		'&&foreign.Record', '&&&foreign.Record', 'mut foreign.Record', 'mut &&foreign.Record',
		'shared &&foreign.Record', 'foreign.Readable', '&&foreign.Readable', 'foreign.Value',
		'&&foreign.Value', 'foreign.Count', '&&foreign.Count', 'foreign.Box[T]', '&&foreign.Box[T]']
	mut sources := []string{cap: receivers.len}
	for receiver in receivers {
		type_text := receiver.trim_string_left('mut ').trim_string_left('shared ')
		parameter := if receiver.starts_with('mut ') || receiver.starts_with('shared ') {
			'${receiver.all_before(' ')} value ${type_text}'
		} else {
			'value ${type_text}'
		}
		sources << 'import foreign

struct Record {}

pub fn (${parameter}) str() string {
 return "foreign"
}

fn main() {}
'
	}
	for flags in [[]string{}, ['-no-parallel'], ['-prod']] {
		result, diagnostics := check_non_local_receiver_cases('imported', sources, flags)
		assert result.exit_code != 0, result.output
		for i, receiver in receivers {
			type_text := receiver.trim_string_left('mut ').trim_string_left('shared ')
			assert diagnostics[i].any(it.contains('cannot define new methods on non-local type ${type_text}')
				&& it.contains('Define an alias and use that instead')), '${receiver}: ${result.output}'
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
	hints := {
		'foreign.Box[T]':          'T'
		'&&foreign.Box[T]':        'T'
		'shared &&foreign.Box[T]': 'T'
		'foreign.Box[[]T]':        'T'
		'foreign.Pair[T, U]':      'T, U'
		'foreign.Pair[U, T]':      'T, U'
		'foreign.Pair[string, T]': 'T'
		'foreign.Box[int]':        ''
		'foreign.Record':          ''
	}
	receivers := hints.keys()
	mut sources := []string{cap: receivers.len}
	for receiver in receivers {
		type_text := receiver.trim_string_left('shared ')
		parameter := if receiver.starts_with('shared ') {
			'shared value ${type_text}'
		} else {
			'value ${type_text}'
		}
		sources << 'import foreign

fn (${parameter}) label[X](other X) string { return "local" }

fn main() {}
'
	}
	for flags in [[]string{}, ['-no-parallel'], ['-prod']] {
		result, diagnostics := check_non_local_receiver_cases('alias_hint', sources, flags)
		assert result.exit_code != 0, result.output
		mut fixed_sources := []string{cap: receivers.len}
		for i, receiver in receivers {
			params := hints[receiver]
			type_text := receiver.trim_string_left('shared ')
			alias_name := if params == '' { 'AliasName' } else { 'AliasName[${params}]' }
			hint := diagnostics[i].filter(it.contains('Define an alias and use that instead like `type ${alias_name} = ${type_text}`'))
			assert hint.len > 0, '${receiver}: ${result.output}'
			if params != '' {
				assert hint[0].contains('use `${alias_name}` as the receiver'), hint[0]
			}
			alias_declaration := hint[0].all_after('instead like `').all_before('`')
			method_params := if params == '' { 'X' } else { '${params}, X' }
			fixed_sources << 'import foreign

${alias_declaration}

fn (value ${alias_name}) label[${method_params}](other X) string { return "local" }

fn main() {}
'
		}
		fixed, _ := check_non_local_receiver_cases('alias_hint_fixed', fixed_sources, flags)
		assert fixed.exit_code == 0, fixed.output
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
	receivers := ['Abc', 'foreign.Missing', '&foreign.Missing', '&&foreign.Missing']
	sources := receivers.map('import foreign

fn (value ${it}) label() string { return "unknown" }

fn main() {}
')
	result, diagnostics := check_non_local_receiver_cases('unknown', sources, [])
	assert result.exit_code != 0, result.output
	for i, receiver in receivers {
		assert diagnostics[i].any(it.contains('unknown type')), '${receiver}: ${result.output}'
	}
	assert !result.output.contains('cannot define new methods on non-local type'), result.output
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
			result := os.exec([@VEXE, ...flags, '-check', root])
			assert result.exit_code == 0, result.output
		}
	}
}
