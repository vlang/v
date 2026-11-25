import os
import v.tempname

// A `$veb.html()` template interpolation is rendered by `veb.filter_html`, which emits a
// trusted `veb.RawHtml` value verbatim but HTML-escapes everything else. A user-defined
// alias that merely shares the short name `RawHtml` (here `main.RawHtml`) must NOT be
// treated as trusted, or user-controlled data in it would render verbatim (XSS). The type
// arg must therefore stay the caller's own alias rather than merging into veb's `RawHtml`
// specialization.
fn test_filter_html_only_trusts_veb_rawhtml() {
	root := os.join_path(os.vtmp_dir(), 'v3_rawhtml_escape_${tempname.unique_token()}_project')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'rawesc' }\n") or { panic(err) }
	source := "module main\n\nimport veb\n\ntype RawHtml = string\n\nfn main() {\n\ttrusted := veb.RawHtml('<b>ok</b>')\n\tuntrusted := RawHtml('<script>x</script>')\n\tplain := '<i>p</i>'\n\tprintln(veb.filter_html(trusted))\n\tprintln(veb.filter_html(untrusted))\n\tprintln(veb.filter_html(plain))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(root, 'app')
	compile :=
		os.exec([@VEXE, '-no-memory-limit', os.join_path(root, 'main.v'), '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split('\n')
	assert lines.len == 3, run.output
	// The trusted veb.RawHtml value is emitted verbatim.
	assert lines[0] == '<b>ok</b>', run.output
	// A non-veb.RawHtml alias holding user data is HTML-escaped, not emitted verbatim.
	assert lines[1] == '&lt;script&gt;x&lt;/script&gt;', run.output
	// A plain string is HTML-escaped too.
	assert lines[2] == '&lt;i&gt;p&lt;/i&gt;', run.output
}
