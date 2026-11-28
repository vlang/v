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

const alias_call_returns_fixture = 'import time

type Span = i64

fn (span Span) seconds() f64 { return f64(span) }
fn new_span() Span { return Span(3) }
fn optional_span() ?Span { return Span(5) }
fn result_span() !Span { return Span(7) }

fn total[T](value T) f64 {
 _ = value
 span := new_span()
 copied := span
 optional := optional_span() or { Span(0) }
 result := result_span() or { Span(0) }
 elapsed := time.since(time.now())
 assert elapsed.seconds() >= 0
 return span.seconds() + copied.seconds() + optional.seconds() + result.seconds()
}

fn main() {
 assert total(1) == 18
 assert total(true) == 18
}
'

fn test_generic_call_returns_keep_borrowed_and_nominal_types() {
	root := os.join_path(os.vtmp_dir(), 'generic_call_returns_${os.getpid()}')
	defer { os.rmdir_all(root) or {} }
	for ownership in [[]string{}, ['-ownership']] {
		dir := os.join_path(root, if ownership.len > 0 { 'ownership' } else { 'regular' })
		os.mkdir_all(dir)!
		source := os.join_path(dir, 'main.v')
		mut input := source
		mut compiler := @VEXE
		if ownership.len > 0 {
			compiler = imported_stats_fixture_vexe()
			input = dir
			write_imported_stats_fixture(dir, 'implements IClone')
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
 check_generic_alias_call_returns()
}

' + imported_stats_named_cast_fixture.replace_once('fn main()', 'fn check_named_generic_type_cast()'))!
			os.write_file(os.join_path(dir, 'alias_returns.v'), alias_call_returns_fixture.replace_once('fn main()', 'fn check_generic_alias_call_returns()'))!
		} else {
			os.write_file(source, alias_call_returns_fixture)!
		}
		for mode in [['-no-parallel'], []string{}] {
			flags := ['-no-memory-limit', '-nocache', '-gc', 'none', ...ownership, ...mode]
			check := os.exec([compiler, ...flags, '-check', input])
			assert check.exit_code == 0, '${flags}: ${check.output}'
			output := os.join_path(dir, 'program_${mode.len}')
			compile := os.exec([compiler, ...flags, '-cc', 'clang', '-o', output, input])
			assert compile.exit_code == 0, '${flags}: ${compile.output}'
			run := os.exec([output])
			assert run.exit_code == 0, '${flags}: ${run.output}'
		}
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
