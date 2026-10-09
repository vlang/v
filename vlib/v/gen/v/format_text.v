module v

import v.parser
import v.pref

// format_text formats V source text held in memory, without reading or writing
// any file. It parses `src` as if it were a file named `main.v` and returns
// the formatted source, or an error when the text has parser errors. Useful
// where no filesystem exists, like the Emscripten build.
pub fn format_text(src string) !string {
	return format_text_with_options(src, FormatOptions{})
}

// format_text_with_options formats in-memory V source with explicit options.
pub fn format_text_with_options(src string, options FormatOptions) !string {
	mut prefs := pref.new_preferences()
	prefs.is_fmt = true
	prefs.preserve_comptime_conditionals = true
	prefs.supports_inline_asm = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_text('main.v', src)
	for d in p.diagnostics {
		if d.severity == '' || d.severity == 'error:' {
			return error('${d.file}:${d.line}:${d.column}: ${d.message}')
		}
	}
	return format_with_options(a, options)
}
