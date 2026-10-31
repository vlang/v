import os
import v.cmdexec

const interface_visibility_module = 'module holder

pub interface Downloader {
 read() int
mut:
 change() int
}

pub interface Base {
 read() int
mut:
 change() int
}

pub interface InheritedDownloader {
 Base
}

interface Hidden {
 read() int
}

pub struct Record {
pub mut:
 value int
}

fn (r &Record) read() int { return r.value }
fn (mut r Record) change() int { r.value++; return r.value }
fn (r Downloader) hidden_default() int { return r.read() }
fn (r Base) hidden_default() int { return r.read() }

pub fn exercise() int {
 mut record := Record{value: 2}
 mut reader := Downloader(&record)
 pointer := &reader
 before := reader.read() + pointer.read()
 changed := reader.change()
 mut mutable_pointer := &reader
 // Private concrete methods remain callable in their own module too.
 assert record.read() == changed
 return before + changed + mutable_pointer.change()
}

pub fn consume_hidden(r Hidden) int { return r.read() }

pub fn exercise_inherited() int {
 mut record := Record{value: 2}
 mut reader := InheritedDownloader(&record)
 pointer := &reader
 before := reader.read() + pointer.read()
 changed := reader.change()
 mut mutable_pointer := &reader
 return before + changed + mutable_pointer.change()
}
'

fn compile_interface_visibility_case(name string, source string, run bool) !os.Result {
	root := os.join_path(os.vtmp_dir(), 'interface_method_visibility_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'holder'))!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'holder', 'holder.v'), interface_visibility_module)!
	file := os.join_path(root, 'main.v')
	os.write_file(file, 'import holder\n' + source)!
	mode := if run { 'run' } else { '-check' }
	return cmdexec.run(@VEXE, ['-new-compiler', '-nocache', mode, file])
}

fn test_interface_methods_ignore_private_concrete_short_name_collisions() {
	result := compile_interface_visibility_case('collision', 'struct Downloader {}
fn (d &Downloader) read() int { return 7 }
fn (mut d Downloader) change() int { return 8 }
fn main() {
 assert holder.exercise() == 11
 mut own := Downloader{}
 assert own.read() == 7
 assert own.change() == 8
}', true)!
	assert result.exit_code == 0, result.output
}

fn test_inherited_interface_methods_ignore_private_concrete_short_name_collisions() {
	result := compile_interface_visibility_case('inherited_collision', 'struct InheritedDownloader {}
fn (d &InheritedDownloader) read() int { return 7 }
fn (mut d InheritedDownloader) change() int { return 8 }
fn main() {
 assert holder.exercise_inherited() == 11
 mut own := InheritedDownloader{}
 assert own.read() == 7
 assert own.change() == 8
}', true)!
	assert result.exit_code == 0, result.output
}

fn test_concrete_private_methods_remain_private_through_values_and_pointers() {
	for i, source in [
		'fn main() { r := holder.Record{}; println(r.read()) }',
		'fn main() { r := &holder.Record{}; println(r.read()) }',
		'fn main() { mut r := holder.Record{}; println(r.change()) }',
		'fn main() { mut r := &holder.Record{}; println(r.change()) }',
	] {
		result := compile_interface_visibility_case('private_${i}', source, false)!
		assert result.exit_code != 0, result.output
		assert result.output.contains('is private'), result.output
		assert result.output.contains('Record.'), result.output
	}
}

fn test_private_interface_type_remains_private() {
	result := compile_interface_visibility_case('private_interface', 'struct Reader {}
fn (r Reader) read() int { return 1 }
fn main() { println(holder.consume_hidden(Reader{})) }
', false)!
	assert result.exit_code != 0, result.output
	assert result.output.contains('cannot implement private interface'), result.output
}

fn test_private_interface_default_methods_remain_private() {
	for i, name in ['Downloader', 'InheritedDownloader'] {
		result := compile_interface_visibility_case('private_default_${i}', 'fn inspect(r holder.${name}) int {
 return r.hidden_default()
}
fn main() {}
', false)!
		assert result.exit_code != 0, result.output
		assert result.output.contains('is private'), result.output
		assert result.output.contains('hidden_default'), result.output
	}
}
