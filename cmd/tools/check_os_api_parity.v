module main

import os
import v.util
import v.util.diff
import v.pref
import v.builder
import v.builder.cbuilder
import v.ast
import term

const base_os = pref.get_host_os()
const os_list = [pref.OS.linux, .macos, .windows, .freebsd, .openbsd, .solaris, .termux]
const skip_modules = [
	'builtin.bare',
	'builtin.linux_bare.old',
	'builtin.js',
	'builtin.wasm',
	'strconv',
	'strconv.ftoa',
	'hash',
	'strings',
	'crypto.rand',
	'os.bare',
	'os2',
	'szip',
	'v.eval',
]
const is_verbose = os.getenv('VERBOSE') != ''

fn main() {
	vexe := os.real_path(os.getenv_opt('VEXE') or { @VEXE })
	vroot := os.dir(vexe)
	util.set_vroot_folder(vroot)
	os.chdir(vroot)!
	modules := if os.args.len > 1 { os.args[1..] } else { all_vlib_modules() }
	mut diff_modules := map[string]bool{}
	other_os_list := os_list.filter(it != base_os)
	for m in modules {
		if !is_verbose {
			eprintln('Checking module: ${m} ...')
		}
		api_base := gen_api_for_module_in_os(m, base_os)
		for other_os in other_os_list {
			api_os := gen_api_for_module_in_os(m, other_os)
			if api_base == api_os {
				continue
			}
			diff_modules[m] = true
			summary := 'Different APIs found for module: `${m}`, between OS base: `${base_os}` and OS: `${other_os}`'
			eprintln(term.header(summary, '-'))
			diff_ := diff.compare_text(api_base, api_os) or { continue }
			println(diff_)
			eprintln(term.h_divider('-'))
		}
	}
	if diff_modules.len > 0 {
		eprintln(term.header('Found ${diff_modules.len} modules with different APIs',
			'='))
		for m in diff_modules.keys() {
			eprintln('Module: ${m}')
		}
		exit(1)
	}
}

fn all_vlib_modules() []string {
	mut vlib_v_files := os.walk_ext('vlib', '.v')
	mut vmodulesmap := map[string]int{}
	for f in vlib_v_files {
		if f.contains('/tests/') || f.ends_with('_test.v') {
			continue
		}
		vmodulename := os.dir(f).replace('/', '.').replace('vlib.', '')
		if vmodulename in skip_modules {
			continue
		}
		vmodulesmap[vmodulename] = vmodulesmap[vmodulename] + 1
	}
	mut modules := vmodulesmap.keys()
	modules.sort()
	return modules
}

fn gen_api_for_module_in_os(mod_name string, os_ pref.OS) string {
	if is_verbose {
		eprintln('Checking module: ${mod_name:-30} for OS: ${os_:-10} ...')
	}
	os_name := os_.str().to_lower()
	mpath := os.join_path('vlib', mod_name.replace('.', '/'))
	tmpname := '/tmp/${mod_name}_${os_name}.c'
	prefs, _ := pref.parse_args([], ['-os', os_name, '-o', tmpname, '-shared', mpath])
	mut b := builder.new_builder(prefs)
	cbuilder.compile_c(mut b)
	mut res := []string{}
	for f in b.parsed_files {
		for s in f.stmts {
			if s is ast.FnDecl && s.is_pub {
				fn_mod := s.modname()
				if fn_mod == mod_name {
					fn_signature := b.table.stringify_fn_decl(&s, mod_name, map[string]string{},
						false)
					fline := '${fn_mod}: ${fn_signature}'
					res << fline
				}
			}
		}
	}
	res.sort()
	return res.join('\n')
}
