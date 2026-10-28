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
		} else if ch == `$` {
			out += '\\' + ch.ascii_str()
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
		if node.kind == .file {
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
		if node.kind == .file {
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
	mut live_source_names := []string{}
	for m in live_mangled {
		mut params := []string{}
		mut ret := ''
		mut source_name := m
		for i in 0 .. a.nodes.len {
			node := a.nodes[i]
			if node.kind == .fn_decl && node.value_text().replace('.', '__') == m {
				source_name = node.value_text()
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
		live_source_names << source_name
	}

	mut out := 'import os\n\n'
	out += 'fn C.fopen(path &u8, mode &u8) voidptr\n'
	out += 'fn C.fread(buf voidptr, size uint, count uint, f voidptr) uint\n'
	out += 'fn C.fseek(f voidptr, offset i64, whence int) int\n'
	out += 'fn C.ftell(f voidptr) i64\n'
	out += 'fn C.fclose(f voidptr) int\n'
	out += 'fn C.system(cmd &u8) int\n'
	out += 'fn C.puts(s &u8) int\n'
	out += 'fn C.remove(path &u8) int\n'
	out += 'fn C.fflush(f voidptr) int\n'
	out += 'fn C.getpid() int\n'
	out += '\n__global live_mtime i64 = os.file_last_mod_unix(live_source_file)\n__global live_code_pages []voidptr\n'
	for idx, m in live_mangled {
		storage := m.bytes().hex()
		params := live_param_types[idx].join(', ')
		ret := live_return_types[idx]
		out += 'type LiveFn_${m} = fn (${params})'
		if ret != '' && ret != 'void' {
			out += ' ${ret}'
		}
		out += '\n__global live_${storage} LiveFn_${m} = ${live_source_names[idx]}\n'
	}
	out += '\nconst live_source_file = ${v3_v_string_literal(source_file)}\n'
	out += 'const live_command_prefix = ${v3_v_string_literal(os.quoted_path(vexe) + ' ' + opts_str)}\n'
	out += 'const live_quoted_source = ${v3_v_string_literal(os.quoted_path(source_file))}\n\n'
	out += 'fn live_reload_check() {\n\tlive_nm := os.file_last_mod_unix(live_source_file)\n\tif live_nm <= live_mtime { return }\n\tmut live_ok := true\n'
	for m in live_mangled {
		storage := m.bytes().hex()
		out += '\tmut live_next_${storage} := voidptr(unsafe { nil })\n'
	}
	for m in live_mangled {
		storage := m.bytes().hex()
		out += '\t{\n'
		out += '\t\tbin := "/tmp/_hot_' + '$' + '{C.getpid()}_${m}.bin"\n'
		out += '\t\tcmd := "' + '$' + '{live_command_prefix} -hot-fn ${m} -o ' + '$' + '{bin} ' + '$' + '{live_quoted_source}"\n'
		out += '\t\tC.remove(bin.str)\n'
		out += '\t\tbuild_rc := C.system(cmd.str)\n'
		out += '\t\tif build_rc != 0 {\n'
		out += '\t\t\tC.puts(c"[live] rebuild failed for ${m}; will retry")\n'
		out += '\t\t\tlive_ok = false\n'
		out += '\t\t} else {\n'
		out += '\t\t\tf := C.fopen(bin.str, c"rb")\n'
		out += '\t\t\tif f == unsafe { nil } {\n'
		out += '\t\t\t\tlive_ok = false\n'
		out += '\t\t\t} else {\n'
		out += '\t\t\t\tC.fseek(f, 0, 2)\n'
		out += '\t\t\t\tsz := C.ftell(f)\n'
		out += '\t\t\t\tC.fseek(f, 0, 0)\n'
		out += '\t\t\t\tif sz <= 0 || sz > 0x4000 {\n'
		out += '\t\t\t\t\tlive_ok = false\n'
		out += '\t\t\t\t} else {\n'
		out += '\t\t\t\t\tpage := live_page_alloc()\n'
		out += '\t\t\t\t\tif live_page_failed(page) {\n'
		out += '\t\t\t\t\t\tlive_ok = false\n'
		out += '\t\t\t\t\t} else if C.fread(page, uint(1), uint(sz), f) != uint(sz) {\n'
		out += '\t\t\t\t\t\tlive_page_free(page)\n'
		out += '\t\t\t\t\t\tlive_ok = false\n'
		out += '\t\t\t\t\t} else if !live_page_protect_rx(page) {\n'
		out += '\t\t\t\t\t\tlive_page_free(page)\n'
		out += '\t\t\t\t\t\tlive_ok = false\n'
		out += '\t\t\t\t\t} else {\n'
		out += '\t\t\t\t\t\tlive_page_flush(page)\n'
		out += '\t\t\t\t\t\tlive_next_${storage} = page\n'
		out += '\t\t\t\t\t}\n'
		out += '\t\t\t\t}\n'
		out += '\t\t\t\tC.fclose(f)\n'
		out += '\t\t\t}\n'
		out += '\t\t}\n'
		out += '\t\tC.remove(bin.str)\n'
		out += '\t}\n'
	}
	out += '\tif live_ok {\n'
	for m in live_mangled {
		storage := m.bytes().hex()
		// Publish only after every extraction succeeds; retained pages protect captured pointers.
		out += '\t\tlive_code_pages << live_next_${storage}\n'
		out += '\t\tlive_${storage} = LiveFn_${m}(live_next_${storage})\n'
	}
	out += '\t\tlive_mtime = live_nm\n\t} else {\n'
	for m in live_mangled {
		storage := m.bytes().hex()
		out += '\t\tif live_next_${storage} != unsafe { nil } { live_page_free(live_next_${storage}) }\n'
	}
	out += '\t\tC.puts(c"[live] reload incomplete; will retry")\n\t}\n\tC.fflush(unsafe { nil })\n}\n'

	return out
}

fn v3_live_reload_os_shim(target_os string) string {
	mut out := ''
	match target_os {
		'macos' {
			out += 'fn C.mmap(addr voidptr, len uint, prot int, flags int, fd int, offset i64) voidptr\n'
			out += 'fn C.munmap(addr voidptr, len uint) int\n'
			out += 'fn C.mprotect(addr voidptr, len uint, prot int) int\n'
			out += 'fn C.sys_icache_invalidate(start voidptr, size uint)\n\n'
			out += 'fn live_page_alloc() voidptr {\n'
			out += '\treturn C.mmap(unsafe { nil }, uint(0x4000), 3, 0x1802, -1, 0)\n'
			out += '}\n\n'
			out += 'fn live_page_failed(p voidptr) bool {\n'
			out += '\treturn p == unsafe { nil } || u64(p) == ~u64(0)\n'
			out += '}\n\n'
			out += 'fn live_page_free(p voidptr) {\n'
			out += '\tC.munmap(p, uint(0x4000))\n'
			out += '}\n\n'
			out += 'fn live_page_protect_rw(p voidptr) bool {\n'
			out += '\treturn C.mprotect(p, uint(0x4000), 3) == 0\n'
			out += '}\n\n'
			out += 'fn live_page_protect_rx(p voidptr) bool {\n'
			out += '\treturn C.mprotect(p, uint(0x4000), 5) == 0\n'
			out += '}\n\n'
			out += 'fn live_page_flush(p voidptr) {\n'
			out += '\tC.sys_icache_invalidate(p, uint(0x4000))\n'
			out += '}\n'
		}
		'linux' {
			out += 'fn C.mmap(addr voidptr, len uint, prot int, flags int, fd int, offset i64) voidptr\n'
			out += 'fn C.munmap(addr voidptr, len uint) int\n'
			out += 'fn C.mprotect(addr voidptr, len uint, prot int) int\n'
			out += 'fn C.__clear_cache(start voidptr, end voidptr)\n\n'
			out += 'fn live_page_alloc() voidptr {\n'
			out += '\treturn C.mmap(unsafe { nil }, uint(0x4000), 3, 0x22, -1, 0)\n'
			out += '}\n\n'
			out += 'fn live_page_failed(p voidptr) bool {\n'
			out += '\treturn p == unsafe { nil } || u64(p) == ~u64(0)\n'
			out += '}\n\n'
			out += 'fn live_page_free(p voidptr) {\n'
			out += '\tC.munmap(p, uint(0x4000))\n'
			out += '}\n\n'
			out += 'fn live_page_protect_rw(p voidptr) bool {\n'
			out += '\treturn C.mprotect(p, uint(0x4000), 3) == 0\n'
			out += '}\n\n'
			out += 'fn live_page_protect_rx(p voidptr) bool {\n'
			out += '\treturn C.mprotect(p, uint(0x4000), 5) == 0\n'
			out += '}\n\n'
			out += 'fn live_page_flush(p voidptr) {\n'
			out += '\tC.__clear_cache(p, voidptr(u64(p) + u64(0x4000)))\n'
			out += '}\n'
		}
		'windows' {
			out += 'fn live_page_free(p voidptr) { C.VirtualFree(p, uint(0), uint(0x8000)) }\n'
			out += 'fn C.VirtualAlloc(addr voidptr, len uint, typ uint, prot uint) voidptr\n'
			out += 'fn C.VirtualFree(addr voidptr, len uint, typ uint) bool\n'
			out += 'fn C.VirtualProtect(addr voidptr, len uint, new_prot uint, old_prot &uint) bool\n'
			out += 'fn C.FlushInstructionCache(proc voidptr, addr voidptr, size uint)\n'
			out += 'fn C.GetCurrentProcess() voidptr\n\n'
			out += 'fn live_page_alloc() voidptr {\n'
			out += '\treturn C.VirtualAlloc(unsafe { nil }, uint(0x4000), 0x3000, 0x04)\n'
			out += '}\n\n'
			out += 'fn live_page_failed(p voidptr) bool {\n'
			out += '\treturn p == unsafe { nil }\n'
			out += '}\n\n'
			out += 'fn live_page_protect_rw(p voidptr) bool {\n'
			out += '\tmut old := uint(0)\n'
			out += '\treturn C.VirtualProtect(p, uint(0x4000), 0x04, &old)\n'
			out += '}\n\n'
			out += 'fn live_page_protect_rx(p voidptr) bool {\n'
			out += '\tmut old := uint(0)\n'
			out += '\treturn C.VirtualProtect(p, uint(0x4000), 0x20, &old)\n'
			out += '}\n\n'
			out += 'fn live_page_flush(p voidptr) {\n'
			out += '\tC.FlushInstructionCache(C.GetCurrentProcess(), p, uint(0x4000))\n'
			out += '}\n'
		}
		else {
			out += 'fn live_page_free(p voidptr) {}\n'
			out += 'fn live_page_alloc() voidptr {\n'
			out += '\treturn unsafe { nil }\n'
			out += '}\n\n'
			out += 'fn live_page_failed(p voidptr) bool {\n'
			out += '\treturn true\n'
			out += '}\n\n'
			out += 'fn live_page_protect_rw(p voidptr) bool {\n'
			out += '\treturn false\n'
			out += '}\n\n'
			out += 'fn live_page_protect_rx(p voidptr) bool {\n'
			out += '\treturn false\n'
			out += '}\n\n'
			out += 'fn live_page_flush(p voidptr) {\n'
			out += '}\n'
		}
	}
	return out
}
