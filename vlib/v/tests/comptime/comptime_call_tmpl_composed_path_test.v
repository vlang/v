import os

const tmpl_dir_name = 'templates'
const tmpl_file_name = 'comptime_call_tmpl_composed_path_test.txt'
const tmpl_plus_path = tmpl_dir_name + '/' + tmpl_file_name
const tmpl_separator_path = tmpl_dir_name + os.path_separator + tmpl_file_name
const tmpl_joined_path = os.join_path(tmpl_dir_name, tmpl_file_name)

fn test_comptime_tmpl_resolves_plus_path_const() {
	value := 'plus'
	rendered := $tmpl(tmpl_plus_path)
	assert rendered.contains(value)
}

fn test_comptime_tmpl_resolves_path_separator_const() {
	value := 'separator'
	rendered := $tmpl(tmpl_separator_path)
	assert rendered.contains(value)
}

fn test_comptime_tmpl_resolves_join_path_const() {
	value := 'joined const'
	rendered := $tmpl(tmpl_joined_path)
	assert rendered.contains(value)
}

fn test_comptime_tmpl_resolves_join_path_variable() {
	value := 'joined var'
	tmpl_path := os.join_path(tmpl_dir_name, tmpl_file_name)
	rendered := $tmpl(tmpl_path)
	assert rendered.contains(value)
}
