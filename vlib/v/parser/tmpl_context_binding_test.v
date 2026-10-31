module parser

import os
import v.pref

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

fn test_template_lookup_stops_at_project_boundaries() {
	root := os.join_path(os.real_path(os.vtmp_dir()), 'v3_tmpl_boundary_${os.getpid()}')
	os.rmdir_all(root) or {}
	source_dir := os.join_path(root, 'child', 'src')
	os.mkdir_all(source_dir) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "parent" }\n')!
	os.mkdir_all(os.join_path(root, 'templates'))!
	explicit_template := os.join_path(root, 'templates', 'shared.html')
	implicit_template := os.join_path(root, 'templates', 'handler.html')
	os.write_file(explicit_template, 'parent template')!
	os.write_file(implicit_template, 'parent handler')!
	source := os.join_path(source_dir, 'handler.v')
	os.write_file(source, 'module main\n')!
	canonical_source_dir := os.dir(os.real_path(source))
	canonical_root := os.dir(os.dir(canonical_source_dir))
	mut p := Parser.new(pref.new_preferences())
	p.cur_file = source
	p.cur_fn = 'handler'
	assert p.resolve_veb_template_path(false, 'shared.html') == os.join_path(canonical_root,
		'templates', 'shared.html')
	assert p.resolve_veb_template_path(true, '') == os.join_path(canonical_root, 'templates',
		'handler.html')
	for marker_name in ['.v.mod.stop', '.git'] {
		marker := os.join_path(root, 'child', marker_name)
		os.write_file(marker, '')!
		assert p.resolve_veb_template_path(false, 'shared.html') == os.join_path(canonical_source_dir,
			'shared.html')
		assert p.resolve_veb_template_path(true, '') == os.join_path(canonical_source_dir,
			'handler.html')
		os.rm(marker)!
	}
}
