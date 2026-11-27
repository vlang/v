// vtest ownership
import os

fn imported_stats_fixture_vexe() string {
	return os.getenv_opt('VTEST_OWNERSHIP_COMPILER') or { @VEXE }
}

fn write_imported_stats_fixture(root string, marker string) {
	write_imported_stats_model(root, 'model', marker)
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'imported_stats_return' }") or {
		panic(err)
	}
}

fn write_imported_stats_model(root string, name string, marker string) {
	dir := os.join_path(root, name)
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, '${name}.v'), 'module ${name}

pub struct Stats ${marker} {
pub mut:
 value int
 bytes []u8
}

pub struct Sink[T] {
 stats_ Stats
 token T
}

pub fn new_sink[T](token T) Sink[T] {
 return Sink[T]{stats_: Stats{value: 42, bytes: [u8(2), 3]}, token: token}
}

pub fn (sink &^a Sink[T]) stats[^a]() &^a Stats {
 return &sink.stats_
}
') or { panic(err) }
}

const imported_stats_named_cast_fixture = 'struct Box[T] { value T }
fn main() {
 owner := Box[int]{value: 42}
 copied := Box[int](owner)
 assert copied.value == 42
}
'

fn test_default_clone_preserves_imported_generic_borrowed_return_type() {
	root := os.join_path(os.vtmp_dir(), 'imported_stats_clone_${os.getpid()}')
	write_imported_stats_fixture(root, 'implements IClone')
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	compiler := imported_stats_fixture_vexe()
	os.write_file(source, 'import model

struct Stats {
 unrelated bool
}

fn clone_stats[T](sink &model.Sink[T]) model.Stats {
 return sink.stats().clone()
}

fn main() {
 _ = Stats{}
 owner := model.new_sink(7)
 mut copied := clone_stats(&owner)
 assert copied.value == 42
 assert copied.bytes == [u8(2), 3]
 assert unsafe { copied.bytes.data != owner.stats().bytes.data }
 copied.bytes[0] = 9
 assert owner.stats().bytes[0] == 2
 assert copied.bytes[0] == 9
 check_named_generic_type_cast()
}

' + imported_stats_named_cast_fixture.replace('fn main()', 'fn check_named_generic_type_cast()'))!
	for mode in ['-no-parallel', ''] {
		check := os.execute('${os.quoted_path(compiler)} -no-memory-limit -nocache -gc none -ownership ${mode} -check ${os.quoted_path(source)}')
		assert check.exit_code == 0, '${mode}: ${check.output}'
		output := os.join_path(root, 'program_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
		compile := os.execute('${os.quoted_path(compiler)} -no-memory-limit -nocache -gc none -ownership -cc clang ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, '${mode}: ${run.output}'
	}
}

fn test_imported_generic_borrowed_return_rejects_missing_method_and_clone() {
	root := os.join_path(os.vtmp_dir(), 'imported_stats_rejections_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	write_imported_stats_fixture(root, '')
	write_imported_stats_model(root, 'marked_model', 'implements IClone')
	compiler := imported_stats_fixture_vexe()
	os.write_file(os.join_path(root, 'main.v'), 'import model
struct Stats implements IClone { value int }
fn invalid[T](sink &model.Sink[T]) int {
 copied := sink.stats().clone()
 return copied.value
}
fn main() { _ = Stats{}; owner := model.new_sink(7); _ = invalid(&owner); check_missing_method() }
')!
	os.write_file(os.join_path(root, 'missing_method.v'), 'import marked_model
fn invalid_missing_method[T](sink &marked_model.Sink[T]) int { return sink.stats().missing_method() }
fn check_missing_method() { owner := marked_model.new_sink(7); _ = invalid_missing_method(&owner) }
')!
	for mode in [['-no-parallel'], []string{}] {
		check := os.exec([compiler, '-no-memory-limit', '-nocache', '-gc', 'none', '-ownership',
			...mode, '-check', root])
		assert check.exit_code != 0, '${mode}: ${check.output}'
		diagnostics := check.output.split_into_lines().filter(it.contains('error: unknown method or field:'))
		assert diagnostics.any(it.contains('main.v:') && it.contains('.clone`')), '${mode}: ${check.output}'
		assert diagnostics.any(it.contains('missing_method.v:') && it.contains('.missing_method`')), '${mode}: ${check.output}'
	}
}

fn test_named_generic_type_cast_retains_cast_behavior() {
	root := os.join_path(os.vtmp_dir(), 'named_generic_cast_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	compiler := @VEXE
	os.write_file(source, imported_stats_named_cast_fixture)!
	for mode in ['-no-parallel', ''] {
		flags := '-no-memory-limit -nocache -gc none ${mode}'
		check := os.execute('${os.quoted_path(compiler)} ${flags} -check ${os.quoted_path(source)}')
		assert check.exit_code == 0, '${flags}: ${check.output}'
		output := os.join_path(root, 'program_${mode.len}')
		compile := os.execute('${os.quoted_path(compiler)} ${flags} -cc clang -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert compile.exit_code == 0, '${flags}: ${compile.output}'
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, '${flags}: ${run.output}'
	}
}
