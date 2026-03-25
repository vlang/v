// vtest build: tinyc && !musl? && !sanitized_job?
import document as doc

// fn test_generate_with_pos() {}
// fn test_generate() {}
// fn test_generate_from_ast() {}
fn test_generate_from_mod() {
	nested_mod_name := 'net.http.chunked'
	nested_mod_doc := doc.generate_from_mod(nested_mod_name, false, true) or {
		eprintln(err)
		assert false
		doc.Doc{}
	}
	assert nested_mod_doc.head.name == nested_mod_name
	assert nested_mod_doc.head.content == 'module ${nested_mod_name}'
	assert nested_mod_doc.contents.len == 3
	assert nested_mod_doc.contents['ChunkScanner'].children.len == 3
}

fn test_tags_with_flag_struct_attribute() {
	mod_name := 'gg'
	mod_doc := doc.generate_from_mod(mod_name, false, true) or {
		eprintln(err)
		assert false
		doc.Doc{}
	}
	assert mod_doc.head.name == mod_name

	mouse_buttons := mod_doc.contents['MouseButtons']!
	assert mouse_buttons.content == '@[flag]
pub enum MouseButtons {
	left
	right
	middle
}'
	assert mouse_buttons.attrs == {
		'flag': '@[flag]'
	}
	assert mouse_buttons.tags == ['@[flag]']

	end_options := mod_doc.contents['EndOptions']
	assert end_options.content == '@[params]
pub struct EndOptions {
pub:
	how EndEnum
}'
	assert end_options.attrs == {
		'params': '@[params]'
	}
	assert end_options.tags == ['@[params]']

	pipeline_container := mod_doc.contents['PipelineContainer']
	assert pipeline_container.content == '@[heap]
pub struct PipelineContainer {
pub mut:
	alpha sgl.Pipeline
	add   sgl.Pipeline
}'
	assert pipeline_container.attrs == {
		'heap': '@[heap]'
	}
	assert pipeline_container.tags == ['@[heap]']
}

fn test_generated_enum_helpers_are_documented() {
	mod_doc := doc.generate_from_mod('gg', false, true) or {
		eprintln(err)
		assert false
		doc.Doc{}
	}

	mouse_buttons := mod_doc.contents['MouseButtons']!
	mouse_button_methods := mouse_buttons.children.filter(it.kind == .method).map(it.name)
	assert 'all' in mouse_button_methods
	assert 'has' in mouse_button_methods
	assert 'is_empty' in mouse_button_methods

	mouse_buttons_from := mod_doc.contents['MouseButtons.from']!
	assert mouse_buttons_from.kind == .method
	assert mouse_buttons_from.content.contains('MouseButtons.from')

	mouse_buttons_zero := mod_doc.contents['MouseButtons.zero']!
	assert mouse_buttons_zero.kind == .method
	assert mouse_buttons_zero.content.contains('MouseButtons.zero')
}

fn test_merge_doc_comments_keeps_blockquotes_on_separate_lines() {
	comments := [
		doc.DocComment{
			text: '> **Note**\n> line one\n> line two'
		},
	]
	assert doc.merge_doc_comments(comments).trim_space() == '> **Note**\n> line one\n> line two'
}
