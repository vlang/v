module driver

import v.parser
import v.pref
import v.errors as compiler_errors

fn report_deprecated_integer_types(paths []string, prefs &pref.Preferences, warns_are_errors bool) bool {
	mut failed := false
	mut diagnostics := compiler_errors.DiagnosticRun{}
	for path in paths {
		mut p := parser.Parser.new(prefs)
		a := p.parse_file(path)
		for diagnostic in p.diagnostics {
			if diagnostic.severity != 'warning:'
				|| !diagnostic.message.starts_with('`isize` is deprecated;')
					&& !diagnostic.message.starts_with('`usize` is deprecated;') {
				continue
			}
			severity := if warns_are_errors { 'error:' } else { 'warning:' }
			if compiler_errors.json_output() {
				eprintln(compiler_errors.json_parser_diagnostic(severity, diagnostic.message, []string{}, a, diagnostic.pos))
			} else {
				eprintln(diagnostics.formatted_parser_diagnostic(severity, diagnostic.message, a, diagnostic.pos))
			}
			failed = failed || warns_are_errors
		}
	}
	return failed
}
