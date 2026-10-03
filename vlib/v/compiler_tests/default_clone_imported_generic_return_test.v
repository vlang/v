import os

fn imported_stats_fixture_vexe() string {
	return os.getenv_opt('VTEST_OWNERSHIP_COMPILER') or { @VEXE }
}

fn write_imported_stats_fixture(root string, marker string) {
	os.mkdir_all(os.join_path(root, 'model')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'imported_stats_return' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'model', 'model.v'), 'module model

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
}
')!
	for mode in ['-no-parallel', ''] {
		check := os.execute('${os.quoted_path(compiler)} -new-compiler -no-memory-limit -nocache -gc none -ownership ${mode} -check ${os.quoted_path(source)}')
		assert check.exit_code == 0, '${mode}: ${check.output}'
		output := os.join_path(root, 'program_${if mode.len > 0 { 'serial' } else { 'parallel' }}')
		compile := os.execute('${os.quoted_path(compiler)} -new-compiler -no-memory-limit -nocache -gc none -ownership -cc clang ${mode} -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
		assert compile.exit_code == 0, '${mode}: ${compile.output}'
		run := os.execute(os.quoted_path(output))
		assert run.exit_code == 0, '${mode}: ${run.output}'
	}
}

fn test_imported_generic_borrowed_return_rejects_unknown_method() {
	root := os.join_path(os.vtmp_dir(), 'imported_stats_unknown_${os.getpid()}')
	write_imported_stats_fixture(root, 'implements IClone')
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	compiler := imported_stats_fixture_vexe()
	os.write_file(source, 'import model
fn invalid[T](sink &model.Sink[T]) int { return sink.stats().missing_method() }
fn main() { owner := model.new_sink(7); _ = invalid(&owner) }
')!
	for mode in ['-no-parallel', ''] {
		check := os.execute('${os.quoted_path(compiler)} -new-compiler -no-memory-limit -nocache -gc none -ownership ${mode} -check ${os.quoted_path(source)}')
		assert check.exit_code != 0, '${mode}: ${check.output}'
		assert check.output.contains('unknown method or field:'), check.output
		assert check.output.contains('.missing_method`'), check.output
	}
}

fn test_default_clone_does_not_use_marker_from_same_named_caller_type() {
	root := os.join_path(os.vtmp_dir(), 'imported_stats_marker_${os.getpid()}')
	write_imported_stats_fixture(root, '')
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	compiler := imported_stats_fixture_vexe()
	os.write_file(source, 'import model
struct Stats implements IClone { value int }
fn invalid[T](sink &model.Sink[T]) int {
 copied := sink.stats().clone()
 return copied.value
}
fn main() { _ = Stats{}; owner := model.new_sink(7); _ = invalid(&owner) }
')!
	for mode in ['-no-parallel', ''] {
		check := os.execute('${os.quoted_path(compiler)} -new-compiler -no-memory-limit -nocache -gc none -ownership ${mode} -check ${os.quoted_path(source)}')
		assert check.exit_code != 0, '${mode}: ${check.output}'
		assert check.output.contains('unknown method or field:'), check.output
		assert check.output.contains('.clone`'), check.output
	}
}

fn test_named_generic_type_cast_retains_cast_behavior() {
	root := os.join_path(os.vtmp_dir(), 'named_generic_cast_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'struct Box[T] { value T }
fn main() {
 owner := Box[int]{value: 42}
 copied := Box[int](owner)
 assert copied.value == 42
}
')!
	for ownership in ['', '-ownership'] {
		compiler := if ownership.len > 0 { imported_stats_fixture_vexe() } else { @VEXE }
		for mode in ['-no-parallel', ''] {
			flags := '-new-compiler -no-memory-limit -nocache -gc none ${ownership} ${mode}'
			check := os.execute('${os.quoted_path(compiler)} ${flags} -check ${os.quoted_path(source)}')
			assert check.exit_code == 0, '${flags}: ${check.output}'
			output := os.join_path(root, 'program_${ownership.len}_${mode.len}')
			compile := os.execute('${os.quoted_path(compiler)} ${flags} -cc clang -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
			assert compile.exit_code == 0, '${flags}: ${compile.output}'
			run := os.execute(os.quoted_path(output))
			assert run.exit_code == 0, '${flags}: ${run.output}'
		}
	}
}
