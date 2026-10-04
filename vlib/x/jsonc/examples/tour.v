module main

import x.jsonc

struct CompilerOptions {
	out_dir string @[json5: 'outDir']
	strict  bool
}

struct TsConfig {
	// A tsconfig spells its keys in camelCase while V fields are snake_case, so
	// the key is named explicitly.
	compiler_opts CompilerOptions @[json5: 'compilerOptions']
	include       []string
}

fn main() {
	// A tsconfig.json: JSON with comments, which plain JSON parsers reject.
	text := '{
	// where the output goes
	"compilerOptions": {
		"outDir": "dist", /* the only option that matters here */
		"strict": true
	},
	"include": ["src"]
}'

	println('valid: ${jsonc.is_valid(text)}')

	cfg := jsonc.decode[TsConfig](text) or { panic(err) }
	println('outDir: ${cfg.compiler_opts.out_dir}')
	println('strict: ${cfg.compiler_opts.strict}')
	println('include: ${cfg.include}')

	// JSON5 syntax that JSONC does not allow is refused, with a position.
	for bad in ['{a: 1}', '{"n": 0x10}', '{"n": Infinity}', '{"n": [1,]}'] {
		jsonc.parse_text(bad) or {
			println('${bad} -> ${err.msg()}')
			continue
		}
	}

	// A trailing comma is a single option, for the files that allow one.
	opts := jsonc.ParseOpts{
		allow_trailing_comma: true
	}
	doc := jsonc.parse_text_opts('{"a": [1, 2,],}', opts) or { panic(err) }
	println('with trailing commas: ${doc.str()}')

	// Comments can be blanked out without moving a single byte, which keeps the
	// offsets of the surrounding text valid.
	stripped := jsonc.strip_comments(text)
	println('same length after stripping: ${stripped.len == text.len}')

	// The byte range of a violation is available for an editor to underline.
	if v := jsonc.violation('{a: 1}') {
		println('at ${v.pos.line}:${v.pos.col} bytes ${v.pos.offset}..${v.pos.end_offset}')
	}
}
