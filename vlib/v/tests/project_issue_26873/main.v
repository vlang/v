module main

fn main() {
	content := new_content('data')
	assert content.@type == .block
	assert temp_root().len > 0
	assert project_root.len > 0
	assert sum_type_count() == 2
	assert make_options(path: project_root).path == project_root
	assert header_comment_ok() == 1
}
