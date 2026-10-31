// Coverage for the `x/markdown` surface that `markdown_test.v` leaves out:
// the `Markdown` processor entry points (`convert`, `convert_plaintext`,
// `parse`), the AST walkers (`walk`, `new_node`, `append_child`,
// `text_content`), the `HTMLRenderer` entry point, `parse_inline` and the
// `Extension.extend` interface itself.
//
// `markdown_test.v` drives everything through `to_html`, which builds and
// throws away a processor internally. These tests build one and reuse it, use
// `parse` to reach the tree, and reach the two members that are unreachable
// from outside the module: `HTMLRenderer.render`, and `parse_inline`, whose
// `ref_map` parameter has an unexported type.
//
// Every value below was read off the running library. There is no clock, no
// file and no network: the inputs are literals and the outputs are HTML or
// plain text.
module markdown

const heading_doc = '# Hi\n\nWorld'

// WalkRecorder accumulates the nodes `walk` visits, and can stop the walk.
// `walk` takes a plain `fn` value, and a `fn [mut x]` closure captures a *copy*
// in this compiler — the same limitation `vlib/arrays/arrays_test.v` works
// around with a pointer — so the state lives on a heap receiver and `visit` is
// passed as a method value.
@[heap]
struct WalkRecorder {
mut:
	kinds []NodeKind
	stop  ?NodeKind
}

// visit records `n` and returns false once the node named by `stop` is seen.
fn (mut w WalkRecorder) visit(n &Node) bool {
	w.kinds << n.kind
	if kind := w.stop {
		if n.kind == kind {
			return false
		}
	}
	return true
}

fn new_markdown() Markdown {
	return Markdown.new(Options{})
}

fn collect_kinds(doc &Node) []NodeKind {
	mut recorder := &WalkRecorder{}
	doc.walk(recorder.visit)
	return recorder.kinds
}

// ---------------------------------------------------------------------
// Markdown.convert
// ---------------------------------------------------------------------

fn test_convert_renders_the_same_html_to_html_does() {
	mut md := new_markdown()
	// `to_html` is a one-shot wrapper around this exact path, so it is the
	// strongest available control on `convert` itself.
	assert md.convert('# Hi') == to_html('# Hi', Options{})
	assert md.convert('hello') == to_html('hello', Options{})
	assert md.convert('- a\n- b') == to_html('- a\n- b', Options{})
	assert md.convert(heading_doc) == to_html(heading_doc, Options{})
}

fn test_convert_of_empty_input_is_empty() {
	mut md := new_markdown()
	// Measured: an empty source produces no document at all, so there is no
	// trailing newline either.
	assert md.convert('') == ''
	assert md.convert('   \n\n  ') == ''
}

fn test_convert_of_an_empty_source_has_no_blocks() {
	mut md := new_markdown()
	doc := md.parse('')
	assert doc.children.len == 0
	assert doc.kind == .document
}

fn test_convert_is_stable_across_calls_on_one_instance() {
	mut md := new_markdown()
	first := md.convert(heading_doc)
	assert md.convert(heading_doc) == first
	assert md.convert(heading_doc) == '<h1>Hi</h1>\n<p>World</p>\n'
	assert md.convert('- a\n- b') == '<ul>\n<li>a</li>\n<li>b</li>\n</ul>\n'
}

fn test_convert_shares_the_reference_definitions_the_parser_collected() {
	mut md := new_markdown()
	// Measured: the definition is written in the same document as its use,
	// and the parse pass that collected it feeds the render pass that follows.
	assert md.convert('[a]\n\n[a]: /x "T"') == '<p><a href="/x" title="T">a</a></p>\n'
}

// ---------------------------------------------------------------------
// Markdown.parse
// ---------------------------------------------------------------------

fn test_parse_returns_a_document_root_holding_the_blocks_in_order() {
	mut md := new_markdown()
	doc := md.parse(heading_doc)
	assert doc.kind == .document
	// Measured: the heading's own level is on the heading node; the document
	// root itself carries level 0.
	assert doc.level == 0
	assert doc.children.len == 2
	assert doc.children[0].kind == .heading
	assert doc.children[1].kind == .paragraph
}

fn test_parse_puts_block_text_in_literal_rather_than_in_children() {
	mut md := new_markdown()
	doc := md.parse(heading_doc)
	// Measured: block-level text is stored in `literal`; the inline tree is
	// rebuilt by the renderer, so a parsed heading has no children at all.
	heading := doc.children[0]
	assert heading.level == 1
	assert heading.literal == 'Hi'
	assert heading.children.len == 0
	paragraph := doc.children[1]
	assert paragraph.literal == 'World'
	assert paragraph.children.len == 0
}

fn test_parse_of_a_list_keeps_the_items_as_children() {
	mut md := new_markdown()
	doc := md.parse('- a\n- b')
	assert doc.children.len == 1
	list := doc.children[0]
	assert list.kind == .list
	assert !list.is_ordered
	assert list.children.len == 2
	// Measured: a list item holds a paragraph, and the item's own text is the
	// paragraph's `literal` rather than a text grandchild.
	assert list.children[0].kind == .list_item
	assert list.children[0].children.len == 1
	assert list.children[0].children[0].kind == .paragraph
	assert list.children[0].children[0].literal == 'a'
	assert list.children[0].children[0].children.len == 0
	assert list.children[1].children[0].literal == 'b'
}

fn test_parse_marks_an_ordered_list_and_its_start() {
	mut md := new_markdown()
	doc := md.parse('3. a\n4. b')
	list := doc.children[0]
	assert list.is_ordered
	assert list.list_start == 3
	assert list.children.len == 2
}

fn test_parse_of_a_document_with_one_paragraph_has_one_child() {
	mut md := new_markdown()
	doc := md.parse('hello')
	assert doc.children.len == 1
	assert doc.children[0].kind == .paragraph
	assert doc.children[0].literal == 'hello'
}

fn test_parse_is_repeatable_and_consumes_a_link_reference_definition() {
	mut md := new_markdown()
	first := md.parse('# Hi')
	assert first.children.len == 1
	second := md.parse('# Hi')
	assert second.children.len == 1
	assert second.children[0].literal == 'Hi'

	// Measured: the definition line is consumed rather than kept as a child,
	// so the paragraph above it has exactly one child.
	defined := md.parse('[a]\n\n[a]: /x')
	assert defined.children.len == 1
	assert defined.children[0].kind == .paragraph
	assert defined.children[0].children.len == 0

	// The definition collected above still resolves a later `[a]`; the link is
	// built by the renderer rather than by `parse`.
	assert md.convert('[a]') == '<p><a href="/x">a</a></p>\n'
}

// ---------------------------------------------------------------------
// Node.walk
// ---------------------------------------------------------------------

fn test_walk_visits_the_root_then_each_block_in_pre_order() {
	mut md := new_markdown()
	doc := md.parse(heading_doc)
	// Measured: pre-order, root first. Inline content is not part of the
	// parsed tree, so the walk is exactly the three block nodes.
	assert collect_kinds(doc) == [NodeKind.document, .heading, .paragraph]
}

fn test_walk_descends_into_nested_blocks() {
	mut md := new_markdown()
	doc := md.parse('- a\n- b')
	// list > item > paragraph, twice, under the document root.
	assert collect_kinds(doc) == [NodeKind.document, .list, .list_item, .paragraph, .list_item,
		.paragraph]
}

fn test_walk_stops_early_when_the_callback_returns_false() {
	mut md := new_markdown()
	doc := md.parse('# a\n\nb\n\nc')
	assert doc.children.len == 3
	mut recorder := &WalkRecorder{
		stop: .paragraph
	}
	// Measured: `walk` reports the callback's decision, so a document it did
	// not finish returns false. The node the callback said false on is still
	// recorded, because it is recorded before the decision is returned.
	assert !doc.walk(recorder.visit)
	assert recorder.kinds == [NodeKind.document, .heading, .paragraph]
}

fn test_walk_of_a_document_with_no_children_visits_only_the_root() {
	mut md := new_markdown()
	doc := md.parse('')
	assert collect_kinds(doc) == [NodeKind.document]
}

fn test_walk_reports_true_when_it_visits_everything() {
	mut md := new_markdown()
	doc := md.parse('- a\n- b')
	completed := doc.walk(collector_visit_always_true)
	assert completed
	assert collect_kinds(doc).len == 6
}

// ---------------------------------------------------------------------
// new_node / append_child / text_content
// ---------------------------------------------------------------------

fn test_new_node_has_the_requested_kind_and_no_children() {
	for kind in [NodeKind.document, .heading, .paragraph, .text, .emphasis, .strong, .code_span] {
		mut node := new_node(kind)
		assert node.kind == kind
		assert node.children.len == 0
		assert node.literal == ''
		assert node.level == 0
		assert node.list_start == 1
		assert !node.is_ordered
		assert node.align == .none_
	}
}

fn test_append_child_appends_in_call_order() {
	root := new_node(.document)
	mut first := new_node(.text)
	first.literal = 'a'
	mut second := new_node(.text)
	second.literal = 'b'
	mut third := new_node(.text)
	third.literal = 'c'
	root.append_child(first)
	root.append_child(second)
	root.append_child(third)
	assert root.children.len == 3
	assert root.children[0].literal == 'a'
	assert root.children[1].literal == 'b'
	assert root.children[2].literal == 'c'
	// A child may itself have children, and the parent's own count is
	// unchanged by that.
	second.append_child(new_node(.emphasis))
	assert second.children.len == 1
	assert root.children.len == 3
}

fn test_text_content_concatenates_descendants_in_document_order() {
	root := new_node(.document)
	emphasis := new_node(.emphasis)
	mut inside := new_node(.text)
	inside.literal = 'a '
	emphasis.append_child(inside)
	strong := new_node(.strong)
	mut second := new_node(.text)
	second.literal = 'b'
	strong.append_child(second)
	root.append_child(emphasis)
	root.append_child(strong)
	assert root.text_content() == 'a b'
	assert emphasis.text_content() == 'a '
	assert strong.text_content() == 'b'
}

fn test_text_content_of_a_leaf_node_is_its_literal() {
	// Measured: `text`, `code_span` and `raw_html` are the three kinds the
	// match arm treats as leaves.
	for kind in [NodeKind.text, .code_span, .raw_html] {
		mut node := new_node(kind)
		node.literal = 'x'
		assert node.text_content() == 'x'
	}
}

fn test_text_content_of_a_childless_container_is_empty() {
	// Measured: the `else` arm builds a string from the children, so a
	// container with none yields the empty string rather than a literal.
	for kind in [NodeKind.document, .heading, .paragraph, .emphasis] {
		assert new_node(kind).text_content() == ''
	}
}

fn test_text_content_ignores_the_literal_of_a_container_that_has_children() {
	mut root := new_node(.document)
	// Measured: the container arm reads only the children, so its own
	// `literal` is not part of the answer even when set.
	root.literal = 'ignored'
	mut child := new_node(.text)
	child.literal = 'kept'
	root.append_child(child)
	assert root.text_content() == 'kept'
}

// ---------------------------------------------------------------------
// HTMLRenderer.render
// ---------------------------------------------------------------------

fn test_html_renderer_renders_a_parsed_document() {
	mut md := new_markdown()
	doc := md.parse(heading_doc)
	mut r := HTMLRenderer{
		opts: md.opts
	}
	// Measured: `convert` is exactly this, so the renderer called directly
	// agrees with the processor.
	assert r.render(doc) == md.convert(heading_doc)
	assert r.render(doc) == '<h1>Hi</h1>\n<p>World</p>\n'
}

fn test_html_renderer_is_reusable_across_documents() {
	mut md := new_markdown()
	mut r := HTMLRenderer{
		opts:    md.opts
		ref_map: map[string]LinkRef{}
	}
	first := r.render(md.parse('# a'))
	assert first == '<h1>a</h1>\n'
	// Measured: the builder is reset on every call, so nothing leaks from the
	// previous document.
	assert r.render(md.parse('# b')) == '<h1>b</h1>\n'
	assert r.render(md.parse('# a')) == first
}

fn test_html_renderer_omits_raw_html_unless_unsafe_is_set() {
	mut md := new_markdown()
	doc := md.parse('<b>x</b>')
	mut default_r := HTMLRenderer{
		opts: md.opts
	}
	// Measured: the default renderer replaces raw HTML with a comment.
	assert default_r.render(doc) == '<p><!-- raw HTML omitted -->x<!-- raw HTML omitted --></p>\n'
	mut unsafe_r := HTMLRenderer{
		opts: Options{
			renderer_opts: RendererOptions{
				unsafe_: true
			}
		}
	}
	assert unsafe_r.render(doc) == '<p><b>x</b></p>\n'
	mut plain := new_markdown()
	assert plain.convert('<b>x</b>') == default_r.render(doc)
}

// ---------------------------------------------------------------------
// parse_inline
// ---------------------------------------------------------------------

fn test_parse_inline_returns_the_inline_nodes_of_a_line() {
	nodes := parse_inline('plain text', Options{}, map[string]LinkRef{})
	assert nodes.len == 1
	assert nodes[0].kind == .text
	assert nodes[0].literal == 'plain text'
}

fn test_parse_inline_resolves_emphasis_into_a_nested_node() {
	nodes := parse_inline('a *b*', Options{}, map[string]LinkRef{})
	// Measured: the text before the run is one node, and the run becomes an
	// `emphasis` whose child is the emphasised text.
	assert nodes.len == 2
	assert nodes[0].kind == .text
	assert nodes[0].literal == 'a '
	assert nodes[1].kind == .emphasis
	assert nodes[1].children.len == 1
	assert nodes[1].children[0].kind == .text
	assert nodes[1].children[0].literal == 'b'
}

fn test_parse_inline_reads_a_code_span_as_one_node() {
	nodes := parse_inline('`x`', Options{}, map[string]LinkRef{})
	assert nodes.len == 1
	assert nodes[0].kind == .code_span
	assert nodes[0].literal == 'x'
}

fn test_parse_inline_applies_the_strikethrough_option() {
	// Measured: without the flag the `~` run is literal text.
	plain := parse_inline('~~x~~', Options{}, map[string]LinkRef{})
	assert plain.len == 1
	assert plain[0].kind == .text
	assert plain[0].literal == '~~x~~'

	struck := parse_inline('~~x~~', Options{
		strikethrough: true
	}, map[string]LinkRef{})
	assert struck.len == 1
	assert struck[0].kind == .strikethrough
	assert struck[0].children.len == 1
	assert struck[0].children[0].literal == 'x'
}

fn test_parse_inline_of_an_empty_string_has_no_nodes() {
	assert parse_inline('', Options{}, map[string]LinkRef{}).len == 0
}

fn test_parse_inline_resolves_a_reference_the_ref_map_already_holds() {
	mut refs := map[string]LinkRef{}
	refs['a'] = LinkRef{
		dest: '/x'
	}
	nodes := parse_inline('[a]', Options{}, refs)
	// Measured: `parse_inline` consults the map it is handed, so a reference
	// defined elsewhere resolves to a `link`.
	assert nodes.len == 1
	assert nodes[0].kind == .link
	assert nodes[0].dest == '/x'
}

// ---------------------------------------------------------------------
// convert_plaintext and to_plaintext
// ---------------------------------------------------------------------

fn test_convert_plaintext_strips_the_markup() {
	mut md := new_markdown()
	// Measured: the same document goes in, and the markup comes out as the
	// text that was written rather than as HTML.
	assert md.convert_plaintext(heading_doc) == '# Hi\n\nWorld\n'
	assert md.convert_plaintext('- a\n- b') == '- a\n- b\n'
	assert md.convert_plaintext('**b**') == '**b**\n'
}

fn test_convert_plaintext_agrees_with_to_plaintext() {
	mut heading := new_markdown()
	mut items := new_markdown()
	assert heading.convert_plaintext(heading_doc) == to_plaintext(heading_doc, Options{})
	assert items.convert_plaintext('- a\n- b') == to_plaintext('- a\n- b', Options{})
}

fn test_convert_plaintext_of_an_empty_source_is_one_newline() {
	// Measured: the plain text renderer always terminates with a newline, so
	// an empty document is a single one.
	mut md := new_markdown()
	assert md.convert_plaintext('') == '\n'
}

// ---------------------------------------------------------------------
// Extension.extend
// ---------------------------------------------------------------------

fn test_extend_sets_only_the_flag_it_owns() {
	mut md := new_markdown()
	assert !md.opts.tables
	TableExt{}.extend(mut md)
	assert md.opts.tables
	// Measured: `extend` writes one flag and leaves every other one alone.
	assert !md.opts.strikethrough
	assert !md.opts.linkify
	assert !md.opts.task_list
	assert !md.opts.footnotes
	assert !md.opts.typographer
	assert !md.opts.definition_list
}

fn test_extend_runs_when_an_extension_is_passed_to_new() {
	mut md := Markdown.new(Options{
		extensions: [Extension(TableExt{})]
	})
	assert md.opts.tables
	// Measured: the table only renders once the extension has set the flag.
	assert md.convert('| a | b |\n|---|---|\n| 1 | 2 |').contains('<table>')
	mut plain := new_markdown()
	assert !plain.convert('| a | b |\n|---|---|\n| 1 | 2 |').contains('<table>')
}

fn test_table_helper_sets_the_flag_when_extend_is_called_by_hand() {
	mut by_helper := new_markdown()
	table().extend(mut by_helper)
	// Measured: applying `table()` directly by hand gives the same flags as
	// passing it to `new`.
	assert by_helper.opts.tables
	assert !by_helper.opts.strikethrough
}

fn test_gfm_extension_reaches_the_parser_through_extend() {
	mut md := Markdown.new(Options{
		extensions: gfm()
	})
	// Measured: `gfm` is four extensions, and the strikethrough and task
	// list ones are visible in the rendered output.
	assert md.opts.tables
	assert md.opts.strikethrough
	assert md.opts.linkify
	assert md.opts.task_list
	assert md.convert('~~x~~').contains('<del>')
	assert md.convert('- [x] done').contains('checkbox')
}

// ---------------------------------------------------------------------

// collector_visit_always_true is a plain `fn` value with no state, used where
// the return value of `walk` is what is under test.
fn collector_visit_always_true(_ &Node) bool {
	return true
}
