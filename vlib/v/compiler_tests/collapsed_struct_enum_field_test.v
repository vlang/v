import os

const collapsed_enum_fixture = 'module ui2

pub enum MessageBoxStyle {
	info
	warning
	error
	question
}

pub enum MessageBoxButtons {
	ok
	ok_cancel
	yes_no
}

pub enum MessageBoxResult {
	ok
	cancel
	yes
	no
}

pub struct MessageBoxConfig {
pub:
	title   string
	text    string
	style   MessageBoxStyle   = .info
	buttons MessageBoxButtons = .ok
}

pub fn message_box(cfg MessageBoxConfig) MessageBoxResult {
	assert cfg.title == "title"
	assert cfg.text == "text"
	if cfg.style == .question && cfg.buttons == .yes_no {
		return .yes
	}
	return .no
}

pub fn question_style() MessageBoxStyle {
	return .question
}

pub fn wrong_style() MessageBoxButtons {
	return .yes_no
}
'

fn collapsed_enum_test_project(name string, extra_module_source string) string {
	root := os.join_path(os.temp_dir(), 'v_collapsed_enum_${name}_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'ui2')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "collapsed_enum_test" }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'ui2', 'message_box.v'), collapsed_enum_fixture +
		extra_module_source) or { panic(err) }
	return root
}

fn test_collapsed_struct_enum_shorthand_uses_field_context() {
	root := collapsed_enum_test_project('valid', '
pub fn confirm(title string, text string) bool {
	return message_box(title: title, text: text, style: .question, buttons: .yes_no) == .yes
}
')
	defer {
		os.rmdir_all(root) or {}
	}
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, 'import ui2

fn main() {
	assert ui2.confirm("title", "text")
	assert ui2.message_box(title: "title", text: "text", style: .question, buttons: .yes_no) == .yes
	assert ui2.message_box(buttons: .yes_no, style: .question, text: "text", title: "title") == .yes
	assert ui2.message_box(title: "title", text: "text", style: ui2.MessageBoxStyle.question,
		buttons: ui2.MessageBoxButtons.yes_no) == .yes
	assert ui2.message_box(title: "title", text: "text", style: ui2.question_style(),
		buttons: .yes_no) == .yes
	assert ui2.message_box(ui2.MessageBoxConfig{
		title: "title"
		text: "text"
		style: .question
		buttons: .yes_no
	}) == .yes
}
') or { panic(err) }
	for flags in ['', '-no-parallel'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} run ${os.quoted_path(main_path)}')
		assert result.exit_code == 0, 'flags=${flags}: ${result.output}'
	}
}

fn check_invalid_collapsed_enum_field(name string, expression string, diagnostic string) {
	root := collapsed_enum_test_project(name, '')
	defer {
		os.rmdir_all(root) or {}
	}
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, 'import ui2

fn main() {
	_ := ui2.message_box(title: "title", text: "text", style: ${expression},
		buttons: ui2.MessageBoxButtons.yes_no)
}
') or { panic(err) }
	for flags in ['', '-no-parallel'] {
		result := os.execute('${os.quoted_path(@VEXE)} ${flags} -check ${os.quoted_path(main_path)}')
		assert result.exit_code != 0, '${name}, flags=${flags}: invalid enum field was accepted'
		assert result.output.contains(diagnostic), '${name}, flags=${flags}: ${result.output}'
	}
}

fn test_collapsed_struct_enum_field_rejects_integer() {
	check_invalid_collapsed_enum_field('integer', '0',
		'cannot assign to field `style`: expected `ui2.MessageBoxStyle`, not `int literal`')
}

fn test_collapsed_struct_enum_field_rejects_other_enum() {
	check_invalid_collapsed_enum_field('other_enum', 'ui2.MessageBoxButtons.yes_no',
		'cannot assign to field `style`: expected `ui2.MessageBoxStyle`, not `ui2.MessageBoxButtons`')
}

fn test_collapsed_struct_enum_field_rejects_other_enum_call() {
	check_invalid_collapsed_enum_field('other_enum_call', 'ui2.wrong_style()',
		'cannot assign to field `style`: expected `ui2.MessageBoxStyle`, not `ui2.MessageBoxButtons`')
}

fn test_collapsed_struct_enum_field_rejects_unknown_shorthand() {
	check_invalid_collapsed_enum_field('unknown_shorthand', '.missing',
		'unknown enum field `missing` for `ui2.MessageBoxStyle`')
}
