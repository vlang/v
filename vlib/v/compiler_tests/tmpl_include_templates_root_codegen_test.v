import os
import v.tempname

fn tir_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

// A nested template under `templates/` that `@include`s a shared partial by bare name resolves it
// against the containing `templates/` root, not only the including file's own directory:
// `templates/pages/index.html` including `'header'` finds `templates/header.html` (there is no
// `templates/pages/header.html`). This mirrors v1's include resolver; without the fallback such
// shared-partial templates fail to compile under v.
fn test_nested_template_includes_shared_root_partial() {
	root := os.join_path(os.vtmp_dir(), 'v3_tmpl_include_templates_root_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tir' }\n") or { panic(err) }
	// The shared partial lives at the templates root; the page lives in a subdirectory.
	tir_write_file(root, 'templates/header.html', 'HEADER_PARTIAL')
	tir_write_file(root, 'templates/pages/index.html', '@include "header"\nPAGE_BODY')
	source := "module main\n\nfn render() string {\n\treturn \$tmpl('templates/pages/index.html').replace('\\n', '|')\n}\n\nfn main() {\n\tprintln(render())\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	// The root partial was expanded above the page body (trailing `|` is the template's final
	// newline turned into `|`).
	assert run.output.trim_space() == 'HEADER_PARTIAL|PAGE_BODY|', run.output
}
