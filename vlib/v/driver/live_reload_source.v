module driver

import os
import v.flat

fn v3_v_string_literal(s string) string {
	mut out := '"'
	for ch in s {
		if ch == `\\` {
			out += '\\\\'
		} else if ch == `"` {
			out += '\\"'
		} else if ch == `\n` {
			out += '\\n'
		} else if ch == `\r` {
			out += '\\r'
		} else if ch == `\t` {
			out += '\\t'
		} else {
			out += ch.ascii_str()
		}
	}
	out += '"'
	return out
}

fn v3_live_reload_source(a &flat.FlatAst, input_file string, vexe string, rebuild_opts []string) string {
	mut cur_file := ''
	mut live_mangled := []string{}
	mut seen := map[string]bool{}
	for i in 0 .. a.nodes.len {
		node := a.nodes[i]
		kind_id := int(node.kind)
		if kind_id == 77 {
			cur_file = node.value_text()
		}
		if node.kind == .directive && node.value.starts_with('@attributes:') {
			target_idx := node.value['@attributes:'.len..].int()
			if target_idx >= 0 && target_idx < a.nodes.len && a.nodes[target_idx].kind == .fn_decl {
				attrs := node.generic_params()
				if 'live' in attrs {
					decl_name := a.nodes[target_idx].value_text()
					mangled := decl_name.replace('.', '__')
					if mangled !in seen {
						seen[mangled] = true
						live_mangled << mangled
					}
				}
			}
		}
	}
	if live_mangled.len == 0 {
		return ''
	}
	mut source_file := ''
	cur_file = ''
	for i in 0 .. a.nodes.len {
		node := a.nodes[i]
		kind_id := int(node.kind)
		if kind_id == 77 {
			cur_file = node.value_text()
		}
		if node.kind == .directive && node.value.starts_with('@attributes:') {
			target_idx := node.value['@attributes:'.len..].int()
			if target_idx >= 0 && target_idx < a.nodes.len && a.nodes[target_idx].kind == .fn_decl {
				attrs := node.generic_params()
				if 'live' in attrs && source_file == '' {
					source_file = cur_file
				}
			}
		}
	}
	if source_file == '' {
		source_file = input_file
	}
	source_file = os.real_path(source_file)
	opts_str := rebuild_opts.map(os.quoted_path(it)).join(' ') + ' -enable-globals'

	mut live_param_types := [][]string{}
	mut live_return_types := []string{}
	for m in live_mangled {
		mut params := []string{}
		mut ret := ''
		for i in 0 .. a.nodes.len {
			node := a.nodes[i]
			if node.kind == .fn_decl && node.value_text().replace('.', '__') == m {
				for ci in 0 .. node.children_count {
					child := a.child_node(&node, ci)
					if child.kind == .param {
						params << flat.source_type_name(child.typ)
					}
				}
				ret = flat.source_type_name(node.typ)
				break
			}
		}
		live_param_types << params
		live_return_types << ret
	}

	mut out := 'import os\n\n'
	out += 'fn C.mmap(addr voidptr, len uint, prot int, flags int, fd int, offset i64) voidptr\n'
	out += 'fn C.mprotect(addr voidptr, len uint, prot int) int\n'
	out += 'fn C.sys_icache_invalidate(start voidptr, size uint)\n'
	out += 'fn C.__clear_cache(start voidptr, end voidptr)\n'
	out += 'fn C.VirtualAlloc(addr voidptr, len uint, typ uint, prot uint) voidptr\n'
	out += 'fn C.VirtualProtect(addr voidptr, len uint, new_prot uint, old_prot &uint) bool\n'
	out += 'fn C.FlushInstructionCache(proc voidptr, addr voidptr, size uint)\n'
	out += 'fn C.GetCurrentProcess() voidptr\n'
	out += 'fn C.fopen(path &u8, mode &u8) voidptr\n'
	out += 'fn C.fread(buf voidptr, size uint, count uint, f voidptr) uint\n'
	out += 'fn C.fseek(f voidptr, offset i64, whence int) int\n'
	out += 'fn C.ftell(f voidptr) i64\n'
	out += 'fn C.fclose(f voidptr) int\n'
	out += 'fn C.system(cmd &u8) int\n'
	out += 'fn C.printf(fmt &u8) int\n'
	out += 'fn C.fflush(f voidptr) int\n\n'

	out += '__global live_mtime i64\n'
	out += '__global live_code_page voidptr\n'
	out += '__global live_code_offset i64\n'
	for idx, m in live_mangled {
		mut param_list := live_param_types[idx].join(', ')
		ret := live_return_types[idx]
		out += 'type LiveFn_${m} = fn (${param_list})'
		if ret != '' && ret != 'void' {
			out += ' ${ret}'
		}
		out += '\n'
		out += '__global live_${m} LiveFn_${m} = ${m}\n'
	}
	out += '\n'

	out += 'const live_source_file = ${v3_v_string_literal(source_file)}\n'
	out += 'const live_vexe = ${v3_v_string_literal(vexe)}\n'
	out += 'const live_opts = ${v3_v_string_literal(opts_str)}\n\n'

	out += 'fn live_page_alloc() voidptr {\n'
	out += '\t$if macos {\n'
	out += '\t\treturn C.mmap(unsafe { nil }, uint(0x4000), 3, 0x1802, -1, 0)\n'
	out += '\t} $else $if linux {\n'
	out += '\t\treturn C.mmap(unsafe { nil }, uint(0x4000), 3, 0x22, -1, 0)\n'
	out += '\t} $else $if windows {\n'
	out += '\t\treturn C.VirtualAlloc(unsafe { nil }, uint(0x4000), 0x3000, 0x04)\n'
	out += '\t} $else {\n'
	out += '\t\treturn unsafe { nil }\n'
	out += '\t}\n'
	out += '}\n\n'

	out += 'fn live_page_protect_rw(p voidptr) {\n'
	out += '\t$if macos || linux {\n'
	out += '\t\tC.mprotect(p, uint(0x4000), 3)\n'
	out += '\t} $else $if windows {\n'
	out += '\t\tmut old := uint(0)\n'
	out += '\t\tC.VirtualProtect(p, uint(0x4000), 0x04, &old)\n'
	out += '\t}\n'
	out += '}\n\n'

	out += 'fn live_page_protect_rx(p voidptr) {\n'
	out += '\t$if macos || linux {\n'
	out += '\t\tC.mprotect(p, uint(0x4000), 5)\n'
	out += '\t} $else $if windows {\n'
	out += '\t\tmut old := uint(0)\n'
	out += '\t\tC.VirtualProtect(p, uint(0x4000), 0x20, &old)\n'
	out += '\t}\n'
	out += '}\n\n'

	out += 'fn live_page_flush(p voidptr) {\n'
	out += '\t$if macos {\n'
	out += '\t\tC.sys_icache_invalidate(p, uint(0x4000))\n'
	out += '\t} $else $if linux {\n'
	out += '\t\tC.__clear_cache(p, voidptr(u64(p) + u64(0x4000)))\n'
	out += '\t} $else $if windows {\n'
	out += '\t\tC.FlushInstructionCache(C.GetCurrentProcess(), p, uint(0x4000))\n'
	out += '\t}\n'
	out += '}\n\n'

	out += 'fn init() {\n'
	for m in live_mangled {
		out += '\tlive_${m} = ${m}\n'
	}
	out += '\tlive_code_page = live_page_alloc()\n'
	out += '\tlive_mtime = os.file_last_mod_unix(live_source_file)\n'
	out += '}\n\n'

	out += 'fn live_reload_check() {\n'
	out += '\tlive_nm := os.file_last_mod_unix(live_source_file)\n'
	out += '\tif live_nm > live_mtime {\n'
	out += '\t\tlive_mtime = live_nm\n'
	out += '\t\tC.printf(c"[live] reloading...\\n")\n'
	out += '\t\tC.fflush(unsafe { nil })\n'
	for idx, m in live_mangled {
		bin := '/tmp/_hot_${m}.bin'
		out += '\t\t{\n'
		out += '\t\t\tcmd := "' + '$' + '{live_vexe} ' + '$' + '{live_opts} -hot-fn ' + m + ' -o ' + bin + ' ' + '$' + '{live_source_file}"\n'
		out += '\t\t\tC.system(cmd.str)\n'
		out += '\t\t\tf := C.fopen(c"' + bin + '", c"rb")\n'
		out += '\t\t\tif f != unsafe { nil } {\n'
		out += '\t\t\t\tC.fseek(f, 0, 2)\n'
		out += '\t\t\t\tsz := int(C.ftell(f))\n'
		out += '\t\t\t\tC.fseek(f, 0, 0)\n'
		out += '\t\t\t\tdest := voidptr(u64(live_code_page) + u64(live_code_offset))\n'
		out += '\t\t\t\tlive_page_protect_rw(live_code_page)\n'
		out += '\t\t\t\tC.fread(dest, uint(1), uint(sz), f)\n'
		out += '\t\t\t\tC.fclose(f)\n'
		out += '\t\t\t\tlive_page_protect_rx(live_code_page)\n'
		out += '\t\t\t\tlive_page_flush(live_code_page)\n'
		out += '\t\t\t\tlive_${m} = LiveFn_${m}(dest)\n'
		out += '\t\t\t\tlive_code_offset = live_code_offset + i64(sz)\n'
		out += '\t\t\t}\n'
		out += '\t\t}\n'
	}
	out += '\t}\n'
	out += '}\n'

	return out
}
PY