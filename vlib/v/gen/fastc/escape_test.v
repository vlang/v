module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_escape_key_and_ansi_strings_keep_escape_bytes() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_escape_bytes_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn C._vinit(argc int, argv voidptr)
const keys = [`\0`, `\e`, `\n`, `\\`, `é`]
fn main() {
	$if vinix { C._vinit(0, unsafe { nil }) }
	assert keys[0] == 0 && keys[1] == 27 && keys[2] == 10
	assert keys[3] == 92 && keys[4] == 233
	text := "\e7A"
	assert text.len == 3 && text[0] == 27 && text[1] == `7` && text[2] == `A`
	c_text := c"\e7A"
	assert unsafe { c_text[0] } == 27
	assert unsafe { c_text[1] } == `7`
	raw := r"\e7A"
	assert raw.len == 4 && raw[0] == `\\` && raw[1] == `e`
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	for target_libc in [false, true] {
		prefs.target_libc_headers = target_libc
		prefs.target = if target_libc {
			pref.target_from('vinix', pref.host_arch()) or { panic(err) }
		} else {
			pref.host_target()
		}
		c_source := generate_files([root], prefs) or { panic(err) }
		c_file := os.join_path(root, 'main.c')
		program := os.join_path(root, 'program')
		os.write_file(c_file, c_source) or { panic(err) }
		build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_file])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}

// V escapes `$` as `\$` to keep it out of an interpolation. C has no `\$` escape (TinyCC
// and clang warn about an unknown escape sequence), so the C literal holds a plain `$`.
fn test_fastc_escaped_dollar_is_a_plain_dollar_in_c() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_escaped_dollar_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn main() {
	price := 5
	plain := "cost: \$5"
	quoted := "x\${y}"
	mixed := "n=${price} \$price \${price}"
	assert plain == "cost: " + "$" + "5"
	assert quoted.len == 5 && quoted[1] == `$` && quoted[2] == `{`
	assert mixed == "n=5 " + "$" + "price " + "$" + "{price}"
	println(plain)
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	assert c_source.contains(r'_S("cost: $5")'), 'plain dollar literal'
	assert c_source.contains(r'_S("x${y}")'), 'dollar before a brace'
	assert c_source.contains(r'_S(" $price ${price}")'), 'dollars in an interpolation segment'
	assert !c_source.contains(r'\$'), r'C has no `\$` escape'
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == r'cost: $5', run.output
}
