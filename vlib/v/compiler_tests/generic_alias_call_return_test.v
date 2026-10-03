import os

fn test_generic_alias_call_returns_keep_nominal_local_types() {
	root := os.join_path(os.vtmp_dir(), 'generic_alias_return_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import time

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
')!
	for ownership in ['', '-ownership'] {
		compiler := if ownership.len > 0 {
			os.getenv_opt('VTEST_OWNERSHIP_COMPILER') or { @VEXE }
		} else {
			@VEXE
		}
		for parallel in ['', '-no-parallel'] {
			flags := '-new-compiler -no-memory-limit -nocache -gc none ${ownership} ${parallel}'
			check := os.execute('${os.quoted_path(compiler)} ${flags} -check ${os.quoted_path(source)}')
			assert check.exit_code == 0, '${flags}: ${check.output}'
			output := os.join_path(root, 'program_${ownership.len}_${parallel.len}')
			compile := os.execute('${os.quoted_path(compiler)} ${flags} -cc clang -o ${os.quoted_path(output)} ${os.quoted_path(source)}')
			assert compile.exit_code == 0, '${flags}: ${compile.output}'
			run := os.execute(os.quoted_path(output))
			assert run.exit_code == 0, '${flags}: ${run.output}'
		}
	}
}

fn test_generic_alias_call_returns_reject_unknown_members() {
	root := os.join_path(os.vtmp_dir(), 'generic_alias_missing_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'type Span = i64
fn (span Span) seconds() f64 { return f64(span) }
fn new_span() Span { return Span(3) }
fn invalid[T](value T) f64 { _ = value; span := new_span(); return span.missing_method() }
fn main() { _ = invalid(1) }
')!
	for ownership in ['', '-ownership'] {
		compiler := if ownership.len > 0 {
			os.getenv_opt('VTEST_OWNERSHIP_COMPILER') or { @VEXE }
		} else {
			@VEXE
		}
		for parallel in ['', '-no-parallel'] {
			flags := '-new-compiler -no-memory-limit -nocache -gc none ${ownership} ${parallel}'
			check := os.execute('${os.quoted_path(compiler)} ${flags} -check ${os.quoted_path(source)}')
			assert check.exit_code != 0, '${flags}: ${check.output}'
			assert check.output.contains('unknown method or field:'), check.output
			assert check.output.contains('.missing_method`'), check.output
		}
	}
}
