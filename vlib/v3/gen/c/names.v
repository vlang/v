module c

import strings
import v3.gen.c.naming

// c_name converts c name data for c.
fn c_name(name string) string {
	return naming.c_name(name)
}

fn c_local_name(name string) string {
	local_name := if name.contains('.') { name.all_after_last('.') } else { name }
	return c_name(local_name)
}

// c_escape supports c escape handling for c.
fn c_escape(s string) string {
	return s.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n').replace('\t', '\\t').replace('\r',
		'\\r')
}

fn c_byte_string_escape(s string) string {
	mut out := strings.new_builder(s.len * 4)
	for b in s.bytes() {
		v := int(b)
		out.write_u8(`\\`)
		out.write_u8(u8(`0` + ((v >> 6) & 7)))
		out.write_u8(u8(`0` + ((v >> 3) & 7)))
		out.write_u8(u8(`0` + (v & 7)))
	}
	return out.str()
}
