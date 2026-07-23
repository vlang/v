import os

const rawesc_vexe = @VEXE
const rawesc_tests_dir = os.dir(@FILE)
const rawesc_v3_dir = os.dir(rawesc_tests_dir)
const rawesc_vlib_dir = os.dir(rawesc_v3_dir)
const rawesc_v3_src = os.join_path(rawesc_v3_dir, 'v3.v')

fn rawesc_build_v3() string {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_rawhtml_escape_test_${pid}')
	os.rm(v3_bin) or {}
	build := os.execute('${rawesc_vexe} -gc none -path "${rawesc_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${rawesc_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A `$veb.html()` template interpolation is rendered by `veb.filter_html`, which emits a
// trusted `veb.RawHtml` value verbatim but HTML-escapes everything else. A user-defined
// alias that merely shares the short name `RawHtml` (here `main.RawHtml`) must NOT be
// treated as trusted, or user-controlled data in it would render verbatim (XSS). The type
// arg must therefore stay the caller's own alias rather than merging into veb's `RawHtml`
// specialization.
fn test_filter_html_only_trusts_veb_rawhtml() {
	v3_bin := rawesc_build_v3()
	pid := os.getpid()
	root := os.join_path(os.temp_dir(), 'v3_rawhtml_escape_${pid}_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'rawesc' }\n") or { panic(err) }
	source := "module main\n\nimport veb\n\ntype RawHtml = string\n\nfn main() {\n\ttrusted := veb.RawHtml('<b>ok</b>')\n\tuntrusted := RawHtml('<script>x</script>')\n\tplain := '<i>p</i>'\n\tprintln(veb.filter_html(trusted))\n\tprintln(veb.filter_html(untrusted))\n\tprintln(veb.filter_html(plain))\n}\n"
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_rawhtml_escape_bin_${pid}')
	compile := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
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
