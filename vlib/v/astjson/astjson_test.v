module main

import os
import v.astjson
import v.token

// write_fixture writes a small V file to a fixed name inside the temp
// directory and returns its path.
fn write_fixture(source string) !string {
	dir := os.join_path(os.vtmp_dir(), 'v_astjson_test_${os.getpid()}')
	os.mkdir_all(dir)!
	path := os.join_path(dir, 'fixture.v')
	os.write_file(path, source)!
	return path
}

// hello_world is the fixture every AST test renders.
const hello_world = "import os\n\n// hello world\nfn main() {\n\tprintln('hello world')\n}\n"

fn dump_of(path string, opts astjson.Options) string {
	return astjson.dump(astjson.parse(path), opts)
}

// The layout is the one cJSON's formatted printer produces, which is what
// `v ast` has always emitted. These expectations pin it so a refactor cannot
// silently change a format agents and tooling already parse.
fn test_dump_writes_the_documented_layout() {
	out := dump_of(write_fixture(hello_world)!, astjson.Options{})
	// A node inside the `files` array is indented by one tab per enclosing
	// object plus one for the array itself.
	assert out.starts_with('{\n\t"files":\t[{\n\t\t\t"kind":\t"file",')
	// Object members go on their own line, indented with tabs, as `"key":<tab>`.
	// Each nesting level adds one more tab, so a child of the file node sits
	// five levels in.
	assert out.contains('\n\t\t\t\t\t"kind":\t"import_decl",')
	assert out.contains('\n\t\t\t\t\t\t"file_id":\t1,')
	// Array elements stay on one line and are separated by `, `.
	assert out.contains(', {\n')
	assert out.contains('"comments":\t[{')
	assert out.ends_with('}')
}

fn test_a_file_with_no_statements_yields_an_empty_files_array() {
	// Only comments: the renderer skips a `.file` node with no children, but the
	// comments are still reported.
	out := dump_of(write_fixture('// only a comment\n')!, astjson.Options{})
	assert out.starts_with('{\n\t"files":\t[],\n\t"comments":\t[{')
}

fn test_a_completely_empty_file_renders_an_empty_array() {
	out := dump_of(write_fixture('\n')!, astjson.Options{})
	assert out == '{\n\t"files":\t[]\n}'
}

fn test_a_comment_keeps_its_whole_text_and_position() {
	out := dump_of(write_fixture('// hello\n')!, astjson.Options{})
	// The comment text is kept verbatim, markers included, and its position is
	// reported as byte offsets into the file.
	assert out.contains('"text":\t"// hello"')
	assert out.contains('"file_id":\t1')
	assert out.contains('"offset":\t0')
	assert out.contains('"end":\t8')
}

fn test_terse_keeps_only_kinds_and_the_tree_shape() {
	out := dump_of(write_fixture(hello_world)!, astjson.Options{ terse: true })
	assert out.contains('"kind":\t"file"')
	assert out.contains('"children":\t[{')
	// `value`, `type`, `op`, `is_mut`, `pos` and `comments` are all details.
	for detail in ['"value"', '"type"', '"op"', '"is_mut"', '"pos"', '"comments"'] {
		assert !out.contains(detail), detail
	}
}

fn test_skip_defaults_drops_zero_valued_properties() {
	out := dump_of(write_fixture(hello_world)!, astjson.Options{ skip_defaults: true })
	// Every node's `type` is `""` and its `op` is `.none` on this fixture.
	assert !out.contains('"type":\t""')
	assert !out.contains('"op":\t"none"')
	// A non-default value survives.
	assert out.contains('"value":\t"main"')
}

fn test_hidden_names_remove_a_property() {
	out := dump_of(write_fixture(hello_world)!, astjson.Options{
		hidden: ['pos', 'value']
	})
	assert !out.contains('"pos"')
	assert !out.contains('"value"')
	assert out.contains('"kind"')
}

fn test_show_combines_the_three_filters() {
	// Terse keeps `kind` but drops every other detail key.
	assert astjson.Options{ terse: true }.show('kind', false)
	assert !astjson.Options{ terse: true }.show('value', false)
	// Terse keeps the structural keys.
	assert astjson.Options{ terse: true }.show('files', false)
	assert astjson.Options{ terse: true }.show('children', false)
	// An explicit hidden name wins over terse.
	assert !astjson.Options{ hidden: ['kind'] }.show('kind', false)
	// `skip_defaults` drops a property only when it holds a zero value.
	assert astjson.Options{ skip_defaults: true }.show('value', false)
	assert !astjson.Options{ skip_defaults: true }.show('value', true)
	// Plain options keep everything.
	assert astjson.Options{}.show('value', true)
}

fn test_writer_escapes_control_and_quote_characters() {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('text')
	w.string('a\n"b"\\c\td\x01')
	w.end_object()
	// An object always breaks after `{`, even with a single member.
	assert w.str() == '{\n\t"text":\t"a\\n\\"b\\"\\\\c\\td\\u0001"\n}'
}

fn test_writer_keeps_an_empty_object_and_array_inline() {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('a')
	w.begin_array()
	w.end_array()
	w.key('b')
	w.begin_object()
	w.end_object()
	w.end_object()
	assert w.str() == '{\n\t"a":\t[],\n\t"b":\t{\n\t}\n}'
}

fn test_writer_joins_array_elements_on_one_line() {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('items')
	w.begin_array()
	for value in ['one', 'two'] {
		w.array_item()
		w.string(value)
	}
	w.end_array()
	w.end_object()
	assert w.str() == '{\n\t"items":\t["one", "two"]\n}'
}

fn test_writer_writes_a_position_as_unquoted_offsets() {
	mut w := astjson.Writer{}
	w.position(token.Pos{
		offset: 10
		end:    20
		id:     7
	})
	assert w.str() == '{\n\t"file_id":\t7,\n\t"offset":\t10,\n\t"end":\t20\n}'
}

fn test_writer_writes_a_boolean() {
	mut w := astjson.Writer{}
	w.begin_object()
	w.key('is_mut')
	w.boolean(true)
	w.key('off')
	w.boolean(false)
	w.end_object()
	assert w.str() == '{\n\t"is_mut":\ttrue,\n\t"off":\tfalse\n}'
}

fn test_the_repo_fixture_parses_and_dumps() {
	// Guards against the shared renderer breaking on a real compiler source
	// file, which exercises far more node kinds than the small fixtures above.
	fixture := os.join_path(@VEXEROOT, 'examples', 'hello_world.v')
	assert os.is_file(fixture)
	out := dump_of(fixture, astjson.Options{})
	assert out.len > 500
	assert out.contains('"kind":\t"file"')
}
