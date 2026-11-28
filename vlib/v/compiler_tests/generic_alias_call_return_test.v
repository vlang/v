// vtest ownership
import os

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
			flags := '-no-memory-limit -nocache -gc none ${ownership} ${parallel}'
			check := os.execute('${os.quoted_path(compiler)} ${flags} -check ${os.quoted_path(source)}')
			assert check.exit_code != 0, '${flags}: ${check.output}'
			assert check.output.contains('unknown method or field:'), check.output
			assert check.output.contains('.missing_method`'), check.output
		}
	}
}
