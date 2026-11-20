import os

// The v-veb skill documents preserving include paths and adding .html only
// when the included filename has no extension.
fn test_veb_skill_template_include_paths() {
	root := os.join_path(os.temp_dir(), 'v_skill_template_include_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.mkdir_all(os.join_path(root, 'partials.txt'))!
	os.mkdir_all(os.join_path(root, 'partials.html'))!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'skill_template_include' }\n")!
	os.write_file(os.join_path(root, 'main.v'), "fn main() { print(\$tmpl('page.txt')) }\n")!
	os.write_file(os.join_path(root, 'page.txt'), '@include "partials.txt/header.txt"\n@include "partials.html/footer"\n')!
	os.write_file(os.join_path(root, 'partials.txt', 'header.txt'), 'header\n')!
	os.write_file(os.join_path(root, 'partials.html', 'footer.html'), 'footer\n')!
	result := os.exec([@VEXE, '-nocache', '-gc', 'none', 'run', os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	assert result.output == 'header\nfooter\n'
}
