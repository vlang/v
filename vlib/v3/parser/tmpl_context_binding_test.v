module parser

import os
import v3.pref

fn test_veb_template_preserves_user_ctx_binding() {
	root := os.join_path(os.temp_dir(), 'v3_tmpl_context_binding_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	template_path := os.join_path(root, 'title.html')
	source_path := os.join_path(root, 'main.v')
	os.write_file(template_path, '@ctx\n') or { panic(err) }
	os.write_file(source_path,
		"module main\n\nstruct Context {}\nstruct Result {}\n\nfn handler(mut context Context) Result {\n\tctx := 'title'\n\treturn \$veb.html('title.html')\n}\n") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	mut p := Parser.new(prefs)
	a := p.parse_file(source_path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut found_template_ctx := false
	for node in a.nodes {
		if node.kind != .ident || node.value != 'ctx' {
			continue
		}
		if position := a.source_position(node.pos) {
			if os.real_path(position.filename) == os.real_path(template_path) {
				found_template_ctx = true
				break
			}
		}
	}
	assert found_template_ctx
}
