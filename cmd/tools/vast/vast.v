module main

import flag
import os
import strings
import time
import v.flat
import v.parser
import v.pref
import v.token

struct Context {
mut:
	is_watch         bool
	is_compile       bool
	is_print         bool
	is_terse         bool
	is_skip_defaults bool
	check            bool
	hide_names       map[string]bool
}

fn main() {
	if os.args.len < 2 {
		eprintln('not enough parameters')
		exit(1)
	}
	mut ctx := Context{}
	mut fp := flag.new_flag_parser(os.args[2..])
	fp.application('v ast')
	fp.usage_example('demo.v       generate demo.json file.')
	fp.usage_example('-w demo.v    generate demo.json file, and watch for changes.')
	fp.usage_example('-c demo.v    generate demo.json *and* a demo.c file, and watch for changes.')
	fp.usage_example('-p demo.v    print the json output to stdout.')
	fp.usage_example('-s demo.v    do NOT show properties having default values.')
	fp.description('Dump a JSON representation of the V AST for a given .v or .vsh file.')
	fp.description('By default, `v ast` saves JSON next to the input file.')
	ctx.is_watch = fp.bool('watch', `w`, false, 'watch a V file and rewrite its JSON when it changes')
	ctx.is_print = fp.bool('print', `p`, false, 'print the AST to stdout')
	ctx.is_compile = fp.bool('compile', `c`, false, 'watch a V file, rewrite its JSON, and generate C whenever it changes')
	ctx.is_terse = fp.bool('terse', `t`, false, 'show only AST node names and structure')
	ctx.is_skip_defaults = fp.bool('skip-defaults', `s`, false, 'skip properties that have default values')
	ctx.check = fp.bool('check', `k`, false, 'type check the input before dumping its AST')
	hfields := fp.string_multi('hide', 0, 'hide fields; specify several by separating them with commas').join(',')
	for field in hfields.split(',') {
		ctx.hide_names[field] = true
	}
	fp.limit_free_args_to_at_least(1)!
	for vfile in fp.remaining_parameters() {
		file := absolute_path(vfile)
		check_file(file)
		ctx.write_file_or_print(file)
		if ctx.is_watch || ctx.is_compile {
			ctx.watch_for_changes(file)
		}
	}
}

fn (ctx Context) write_file_or_print(file string) {
	if ctx.check {
		compiler := os.getenv_opt('VEXE') or { 'v' }
		result := os.execute('${os.quoted_path(compiler)} -check ${os.quoted_path(file)}')
		if result.exit_code != 0 {
			eprint(result.output)
			exit(result.exit_code)
		}
	}
	ast_json := ctx.json(file)
	if ctx.is_print {
		println(ast_json)
	} else {
		out_file := file[..file.len - os.file_ext(file).len] + '.json'
		os.write_file(out_file, ast_json) or { panic(err) }
		println('${time.now()}: AST written to: ${out_file}')
	}
}

fn (ctx Context) watch_for_changes(file string) {
	println('start watching...')
	mut timestamp := i64(0)
	for {
		new_timestamp := os.file_last_mod_unix(file)
		if timestamp != new_timestamp {
			ctx.write_file_or_print(file)
			if ctx.is_compile {
				compiler := os.getenv_opt('VEXE') or { 'v' }
				file_name := file[..file.len - os.file_ext(file).len]
				os.system('${os.quoted_path(compiler)} -o ${os.quoted_path(file_name + '.c')} ${os.quoted_path(file)}')
			}
		}
		timestamp = new_timestamp
		time.sleep(500 * time.millisecond)
	}
}

fn absolute_path(path string) string {
	if os.is_abs_path(path) {
		return path
	}
	if path.starts_with('./') {
		return os.join_path(os.getwd(), path[2..])
	}
	return os.join_path(os.getwd(), path)
}

fn check_file(file string) {
	if os.file_ext(file) !in ['.v', '.vv', '.vsh'] {
		eprintln('the file `${file}` must be a v file or vsh file')
		exit(1)
	}
	if !os.exists(file) {
		eprintln('the v file `${file}` does not exist')
		exit(1)
	}
}

fn (ctx Context) json(file string) string {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.is_fmt = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_file(file)
	mut w := JsonWriter{
		sb: strings.new_builder(64 * 1024)
	}
	w.begin_object()
	if ctx.show_field('files', false) {
		w.key('files')
		w.begin_array()
		for raw_id in a.file_node_ids {
			id := flat.NodeId(raw_id)
			node := a.node(id)
			if node.kind == .file && node.children_count > 0 {
				w.array_item()
				ctx.write_ast_node(mut w, a, id)
			}
		}
		w.end_array()
	}
	if a.comments.len > 0 && ctx.show_field('comments', false) {
		w.key('comments')
		w.begin_array()
		for comment in a.comments {
			w.array_item()
			w.begin_object()
			if ctx.show_field('text', false) {
				w.key('text')
				w.string(comment.text)
			}
			if ctx.show_field('pos', false) {
				w.key('pos')
				w.position(comment.pos)
			}
			w.end_object()
		}
		w.end_array()
	}
	w.end_object()
	return w.sb.str()
}

fn (ctx Context) write_ast_node(mut w JsonWriter, a &flat.FlatAst, id flat.NodeId) {
	node := a.node(id)
	w.begin_object()
	if ctx.show_field('kind', false) {
		w.key('kind')
		w.string('${node.kind}')
	}
	if ctx.show_field('value', node.value == '') {
		w.key('value')
		w.string(node.value)
	}
	if ctx.show_field('type', node.typ == '') {
		w.key('type')
		w.string(node.typ)
	}
	if ctx.show_field('op', node.op == .none) {
		w.key('op')
		w.string('${node.op}')
	}
	if ctx.show_field('is_mut', !node.is_mut) {
		w.key('is_mut')
		w.bool(node.is_mut)
	}
	if ctx.show_field('pos', !node.pos.is_valid()) {
		w.key('pos')
		w.position(node.pos)
	}
	if node.children_count > 0 && ctx.show_field('children', false) {
		w.key('children')
		w.begin_array()
		for child in a.children_of(node) {
			w.array_item()
			ctx.write_ast_node(mut w, a, child)
		}
		w.end_array()
	}
	w.end_object()
}

// show_field reports whether the `key` property is part of the output.
fn (ctx Context) show_field(key string, is_default bool) bool {
	return !(key in ctx.hide_names || ctx.is_terse && key !in ['kind', 'files', 'children']
		|| ctx.is_skip_defaults && is_default)
}

// JsonWriter writes indented JSON in the layout of cJSON's formatted printer:
// object members one per line, indented with tabs, as `"key":<tab>value`, and
// array elements on one line, separated by `, `.
struct JsonWriter {
mut:
	sb     strings.Builder
	depth  int
	counts []int // number of members/elements written, per open object/array
}

fn (mut w JsonWriter) begin_object() {
	w.sb.write_string('{\n')
	w.depth++
	w.counts << 0
}

fn (mut w JsonWriter) end_object() {
	if w.counts.pop() > 0 {
		w.sb.write_u8(`\n`)
	}
	w.depth--
	w.indent(w.depth)
	w.sb.write_u8(`}`)
}

fn (mut w JsonWriter) begin_array() {
	w.sb.write_u8(`[`)
	w.depth++
	w.counts << 0
}

fn (mut w JsonWriter) end_array() {
	w.counts.pop()
	w.depth--
	w.sb.write_u8(`]`)
}

// key starts the next member of the current object.
fn (mut w JsonWriter) key(name string) {
	if w.counts.last() > 0 {
		w.sb.write_string(',\n')
	}
	w.counts[w.counts.len - 1]++
	w.indent(w.depth)
	w.string(name)
	w.sb.write_string(':\t')
}

// array_item starts the next element of the current array.
fn (mut w JsonWriter) array_item() {
	if w.counts.last() > 0 {
		w.sb.write_string(', ')
	}
	w.counts[w.counts.len - 1]++
}

fn (mut w JsonWriter) indent(depth int) {
	for _ in 0 .. depth {
		w.sb.write_u8(`\t`)
	}
}

fn (mut w JsonWriter) bool(value bool) {
	w.sb.write_string(if value { 'true' } else { 'false' })
}

fn (mut w JsonWriter) position(pos token.Pos) {
	w.begin_object()
	w.key('file_id')
	w.sb.write_string(pos.id.str())
	w.key('offset')
	w.sb.write_string(pos.offset.str())
	w.key('end')
	w.sb.write_string(pos.end.str())
	w.end_object()
}

fn (mut w JsonWriter) string(value string) {
	w.sb.write_u8(`"`)
	for c in value {
		match c {
			`"` { w.sb.write_string('\\"') }
			`\\` { w.sb.write_string('\\\\') }
			8 { w.sb.write_string('\\b') }
			12 { w.sb.write_string('\\f') }
			`\n` { w.sb.write_string('\\n') }
			`\r` { w.sb.write_string('\\r') }
			`\t` { w.sb.write_string('\\t') }
			else {
				if c < 32 {
					w.sb.write_string('\\u00')
					w.sb.write_u8('0123456789abcdef'[c >> 4])
					w.sb.write_u8('0123456789abcdef'[c & 15])
				} else {
					w.sb.write_u8(c)
				}
			}
		}
	}
	w.sb.write_u8(`"`)
}
