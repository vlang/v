import os

const tir_vexe = @VEXE
const tir_tests_dir = os.dir(@FILE)
const tir_v3_dir = os.dir(tir_tests_dir)
const tir_vlib_dir = os.dir(tir_v3_dir)
const tir_v3_src = os.join_path(tir_v3_dir, 'v3.v')

fn tir_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_tmpl_include_templates_root_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${tir_vexe} -gc none -path "${tir_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${tir_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn tir_write_file(root string, rel string, source string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, source) or { panic(err) }
}

// A nested template under `templates/` that `@include`s a shared partial by bare name resolves it
// against the containing `templates/` root, not only the including file's own directory:
// `templates/pages/index.html` including `'header'` finds `templates/header.html` (there is no
// `templates/pages/header.html`). This mirrors v1's include resolver; without the fallback such
// shared-partial templates fail to compile under v3.
fn test_nested_template_includes_shared_root_partial() {
	v3_bin := tir_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_tmpl_include_templates_root_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'tir' }\n") or { panic(err) }
	// The shared partial lives at the templates root; the page lives in a subdirectory.
	tir_write_file(root, 'templates/header.html', 'HEADER_PARTIAL')
	tir_write_file(root, 'templates/pages/index.html', '@include "header"\nPAGE_BODY')
	source := "module main\n\nfn render() string {\n\treturn \$tmpl('templates/pages/index.html').replace('\\n', '|')\n}\n\nfn main() {\n\tprintln(render())\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_tmpl_include_templates_root_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	// The root partial was expanded above the page body (trailing `|` is the template's final
	// newline turned into `|`).
	assert run.output.trim_space() == 'HEADER_PARTIAL|PAGE_BODY|', run.output
}
