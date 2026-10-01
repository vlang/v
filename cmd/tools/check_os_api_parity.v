module main

import os
import json2
import v.util
import v.util.diff
import v.pref
import term

struct VDocModule {
	contents []VDocNode
}

struct VDocNode {
	content string
	kind    string
	is_pub  bool @[json: public]
}

const os_list = ['linux', 'macos', 'windows', 'freebsd', 'openbsd', 'solaris', 'termux']
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
]
const is_verbose = os.getenv('VERBOSE') != ''

fn main() {
	vexe := os.real_path(os.getenv_opt('VEXE') or { @VEXE })
	vroot := os.dir(vexe)
	base_os := pref.host_os_name()
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
		eprintln(term.header('Found ${diff_modules.len} modules with different APIs', '='))
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

fn gen_api_for_module_in_os(mod_name string, os_name string) string {
	if is_verbose {
		eprintln('Checking module: ${mod_name:-30} for OS: ${os_name:-10} ...')
	}
	vexe := os.real_path(os.getenv_opt('VEXE') or { @VEXE })
	result := os.execute('${os.quoted_path(vexe)} doc -f json -o - -os ${os_name} ${os.quoted_path(mod_name)}')
	if result.exit_code != 0 {
		panic('failed to document `${mod_name}` for `${os_name}`:\n${result.output}')
	}
	doc := json2.decode[VDocModule](result.output) or {
		panic('failed to decode documentation for `${mod_name}` on `${os_name}`: ${err}')
	}
	mut res := []string{}
	for node in doc.contents {
		if node.is_pub && node.kind in ['function', 'method'] {
			res << '${mod_name}: ${node.content}'
		}
	}
	res.sort()
	return res.join('\n')
}
