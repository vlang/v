// Module `astjson` renders a parsed V file as JSON.
//
// It exists so `v ast` (`cmd/tools/vast`) and the `v_ast` MCP tool
// (`cmd/tools/vmcp`) produce byte-identical output from one implementation: an
// agent that learned to read `v ast -p` output reads the MCP tool the same way.
//
// The layout is the one cJSON's formatted printer produces, which is what
// `v ast` has always emitted: object members one per line, indented with tabs,
// as `"key":<tab>value`, and array elements on one line separated by `, `.
module astjson

import strings
import v.flat
import v.parser
import v.pref
import v.token

// Options selects which properties appear in the output and how deep it goes.
pub struct Options {
pub:
	// terse keeps only the node kinds and the tree shape, dropping every detail.
	terse bool
	// skip_defaults drops properties holding a zero value, such as `[]`, `{}`,
	// `false`, `0` and `""`.
	skip_defaults bool
	// hidden names properties to leave out entirely.
	hidden []string
}

// parse reads one V file into a fresh AST with the preferences `v ast` uses.
pub fn parse(path string) &flat.FlatAst {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.is_fmt = true
	mut p := parser.Parser.new(prefs)
	return p.parse_file(path)
}

// dump renders the AST of the already parsed `a` as JSON.
//
// Every `.file` node with at least one child becomes an entry of the top level
// `files` array; the collected comments follow under `comments`. That mirrors
// what `v ast` writes, so its output stays stable.
pub fn dump(a &flat.FlatAst, opts Options) string {
	mut w := Writer{}
	w.begin_object()
	if opts.show('files', false) {
		w.key('files')
		w.begin_array()
		for raw_id in a.file_node_ids {
			id := flat.NodeId(raw_id)
			node := a.node(id)
			if node.kind == .file && node.children_count > 0 {
				w.array_item()
				write_node(mut w, a, id, opts)
			}
		}
		w.end_array()
	}
	if a.comments.len > 0 && opts.show('comments', false) {
		w.key('comments')
		w.begin_array()
		for comment in a.comments {
			w.array_item()
			w.begin_object()
			if opts.show('text', false) {
				w.key('text')
				w.string(comment.text)
			}
			if opts.show('pos', false) {
				w.key('pos')
				w.position(comment.pos)
			}
			w.end_object()
		}
		w.end_array()
	}
	w.end_object()
	return w.str()
}

// write_node renders one AST node and its children.
fn write_node(mut w Writer, a &flat.FlatAst, id flat.NodeId, opts Options) {
	node := a.node(id)
	w.begin_object()
	if opts.show('kind', false) {
		w.key('kind')
		w.string('${node.kind}')
	}
	if opts.show('value', node.value == '') {
		w.key('value')
		w.string(node.value)
	}
	if opts.show('type', node.typ == '') {
		w.key('type')
		w.string(node.typ)
	}
	if opts.show('op', node.op == .none) {
		w.key('op')
		w.string('${node.op}')
	}
	if opts.show('is_mut', !node.is_mut) {
		w.key('is_mut')
		w.boolean(node.is_mut)
	}
	if opts.show('pos', !node.pos.is_valid()) {
		w.key('pos')
		w.position(node.pos)
	}
	if node.children_count > 0 && opts.show('children', false) {
		w.key('children')
		w.begin_array()
		for child in a.children_of(node) {
			w.array_item()
			write_node(mut w, a, child, opts)
		}
		w.end_array()
	}
	w.end_object()
}

// show reports whether the `key` property is part of the output. `is_default`
// says whether the property holds a zero value, which `skip_defaults` drops.
pub fn (opts Options) show(key string, is_default bool) bool {
	return !(key in opts.hidden || opts.terse && key !in ['kind', 'files', 'children']
		|| opts.skip_defaults && is_default)
}

// Writer writes indented JSON in the layout `v ast` uses: object members one
// per line, indented with tabs, as `"key":<tab>value`, and array elements on one
// line, separated by `, `.
pub struct Writer {
mut:
	sb     strings.Builder
	depth  int
	counts []int // number of members/elements written, per open object/array
}

// begin_object opens a JSON object.
pub fn (mut w Writer) begin_object() {
	w.sb.write_string('{\n')
	w.depth++
	w.counts << 0
}

// end_object closes the open JSON object.
pub fn (mut w Writer) end_object() {
	if w.counts.pop() > 0 {
		w.sb.write_u8(`\n`)
	}
	w.depth--
	w.indent(w.depth)
	w.sb.write_u8(`}`)
}

// begin_array opens a JSON array.
pub fn (mut w Writer) begin_array() {
	w.sb.write_u8(`[`)
	w.depth++
	w.counts << 0
}

// end_array closes the open JSON array.
pub fn (mut w Writer) end_array() {
	w.counts.pop()
	w.depth--
	w.sb.write_u8(`]`)
}

// key starts the next member of the current object.
pub fn (mut w Writer) key(name string) {
	if w.counts.last() > 0 {
		w.sb.write_string(',\n')
	}
	w.counts[w.counts.len - 1]++
	w.indent(w.depth)
	w.string(name)
	w.sb.write_string(':\t')
}

// array_item starts the next element of the current array.
pub fn (mut w Writer) array_item() {
	if w.counts.last() > 0 {
		w.sb.write_string(', ')
	}
	w.counts[w.counts.len - 1]++
}

// indent writes `depth` levels of tabs.
fn (mut w Writer) indent(depth int) {
	for _ in 0 .. depth {
		w.sb.write_u8(`\t`)
	}
}

// boolean writes a JSON boolean.
pub fn (mut w Writer) boolean(value bool) {
	w.sb.write_string(if value { 'true' } else { 'false' })
}

// number writes a JSON integer.
pub fn (mut w Writer) number(value int) {
	w.sb.write_string(value.str())
}

// key_raw starts the next member of the current object, writing a pre-rendered
// JSON fragment as its value. It is how a nested object or array is inserted
// without re-parsing what a helper already rendered.
pub fn (mut w Writer) key_raw(name string, value_json string) {
	w.key(name)
	w.sb.write_string(value_json)
}

// array_raw starts the next element of the current array, writing a pre-rendered
// JSON fragment as its value.
pub fn (mut w Writer) array_raw(value_json string) {
	w.array_item()
	w.sb.write_string(value_json)
}

// position writes a source position as a file id with its byte offsets.
pub fn (mut w Writer) position(pos token.Pos) {
	w.begin_object()
	w.key('file_id')
	w.sb.write_string(pos.id.str())
	w.key('offset')
	w.sb.write_string(pos.offset.str())
	w.key('end')
	w.sb.write_string(pos.end.str())
	w.end_object()
}

// string writes a quoted, escaped JSON string.
pub fn (mut w Writer) string(value string) {
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

// str returns the accumulated JSON.
pub fn (w &Writer) str() string {
	return w.sb.str()
}
