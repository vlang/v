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
			out += ch.str()
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
	opts_str := rebuild_opts.map(os.quoted_path(it)).join(' ')

	mut out := 'import time\n\n'
	out += 'fn C.stat(path &u8, buf &u8) int\n'
	out += 'fn C.mmap(addr voidptr, len usize, prot int, flags int, fd int, offset i64) voidptr\n'
	out += 'fn C.mprotect(addr voidptr, len usize, prot int) int\n'
	out += 'fn C.sys_icache_invalidate(start voidptr, size usize)\n'
	out += 'fn C.fopen(path &u8, mode &u8) voidptr\n'
	out += 'fn C.fread(buf voidptr, size usize, count usize, f voidptr) usize\n'
	out += 'fn C.fseek(f voidptr, offset i64, whence int) int\n'
	out += 'fn C.ftell(f voidptr) i64\n'
	out += 'fn C.fclose(f voidptr) int\n'
	out += 'fn C.system(cmd &u8) int\n'
	out += 'fn C.printf(fmt &u8) int\n'
	out += 'fn C.fflush(f voidptr) int\n'
	out += 'fn C.clock_gettime_nsec_np(clock_id int) u64\n\n'

	out += '__global __live_mtime i64\n'
	out += '__global __live_code_page voidptr\n'
	out += '__global __live_code_offset i64\n'
	for m in live_mangled {
		out += '__global __live_${m} voidptr\n'
	}
	out += '\n'

	out += 'const live_source_file = ${v3_v_string_literal(source_file)}\n'
	out += 'const live_vexe = ${v3_v_string_literal(vexe)}\n'
	out += 'const live_opts = ${v3_v_string_literal(opts_str)}\n\n'

	out += 'fn init() {\n'
	for m in live_mangled {
		out += '\t__live_${m} = voidptr(&${m})\n'
	}
	out += '\t__live_code_page = C.mmap(unsafe { nil }, usize(0x4000), 3, 0x1802, -1, 0)\n'
	out += '\tmut __live_sb := [144]u8{}\n'
	out += '\tif C.stat(live_source_file.str, &__live_sb[0]) == 0 {\n'
	out += '\t\t__live_mtime = unsafe { *(&i64(&__live_sb[48])) }\n'
	out += '\t}\n'
	out += '\tspawn live_reload_loop()\n'
	out += '}\n\n'

	out += 'fn live_reload_check() {\n'
	out += '\tmut __live_ns := [144]u8{}\n'
	out += '\tif C.stat(live_source_file.str, &__live_ns[0]) == 0 {\n'
	out += '\t\t__live_nm := unsafe { *(&i64(&__live_ns[48])) }\n'
	out += '\t\tif __live_nm > __live_mtime {\n'
	out += '\t\t\t__live_mtime = __live_nm\n'
	out += '\t\t\tC.printf(c"[live] reloading...\\n")\n'
	out += '\t\t\tC.fflush(unsafe { nil })\n'
	for m in live_mangled {
		bin := '/tmp/_hot_${m}.bin'
		out += '\t\t\t{\n'
		out += '\t\t\t\tcmd := "${vexe} ${opts_str} -hot-fn ${m} -o ${bin} ${source_file}"\n'
		out += '\t\t\t\tC.system(cmd.str)\n'
		out += '\t\t\t\tf := C.fopen(c"${bin}", c"rb")\n'
		out += '\t\t\t\tif f != unsafe { nil } {\n'
		out += '\t\t\t\t\tC.fseek(f, 0, 2)\n'
		out += '\t\t\t\t\tsz := int(C.ftell(f))\n'
		out += '\t\t\t\t\tC.fseek(f, 0, 0)\n'
		out += '\t\t\t\t\tdest := voidptr(u64(__live_code_page) + u64(__live_code_offset))\n'
		out += '\t\t\t\t\tC.mprotect(__live_code_page, usize(0x4000), 3)\n'
		out += '\t\t\t\t\tC.fread(dest, usize(1), usize(sz), f)\n'
		out += '\t\t\t\t\tC.fclose(f)\n'
		out += '\t\t\t\t\tC.mprotect(__live_code_page, usize(0x4000), 5)\n'
		out += '\t\t\t\t\tC.sys_icache_invalidate(__live_code_page, usize(0x4000))\n'
		out += '\t\t\t\t\t__live_${m} = dest\n'
		out += '\t\t\t\t\t__live_code_offset = __live_code_offset + i64(sz)\n'
		out += '\t\t\t\t}\n'
		out += '\t\t\t}\n'
	}
	out += '\t\t}\n'
	out += '\t}\n'
	out += '}\n\n'

	out += 'fn live_reload_loop() {\n'
	out += '\tfor {\n'
	out += '\t\tlive_reload_check()\n'
	out += '\t\ttime.sleep(100 * time.millisecond)\n'
	out += '\t}\n'
	out += '}\n'

	return out
}
PY