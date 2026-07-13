module generator

import math
import strings
import x.kdl.document

const lf = 10
const cr = 13
const tab = 9
const qt = 34
const bs = 92
const lp = 40
const rp = 41
const sp = 32
const eq = 61

struct Generator {
mut:
	sb strings.Builder
}

fn new_generator() Generator {
	return Generator{
		sb: strings.new_builder(256)
	}
}

// format serializes a Document to KDL text.
pub fn format(doc document.Document) !string {
	mut g := new_generator()
	return g.generate(doc)
}

fn (mut g Generator) generate(doc document.Document) string {
	g.sb = strings.new_builder(256)
	for i, node in doc.nodes {
		if i > 0 { g.sb.write_u8(lf) }
		g.gen_node(node)
	}
	if doc.nodes.len > 0 { g.sb.write_u8(lf) }
	return g.sb.str()
}

fn (mut g Generator) gen_node(node document.Node) {
	// Output before-comment if present
	if comment := node.comment {
		if comment.before.len > 0 {
			g.sb.write_string(comment.before.trim_right(' \t\n\r'))
			g.sb.write_u8(lf)
		}
	}
	if node.type_name.len > 0 {
		g.gen_type_annotation(node.type_name)
	}
	g.gen_string(node.name)
	for entry in node.entries {
		g.sb.write_u8(sp)
		match entry {
			document.Argument {
				if entry.type_name.len > 0 {
					g.gen_type_annotation(entry.type_name)
				}
				g.gen_value(entry.value)
			}
			document.Property {
				g.gen_string(entry.key)
				g.sb.write_u8(eq)
				if entry.type_name.len > 0 {
					g.gen_type_annotation(entry.type_name)
				}
				g.gen_value(entry.value)
			}
		}
	}
	if node.children.len > 0 {
		g.sb.write_u8(sp)
		g.sb.write_string('{')
		g.sb.write_u8(lf)
		for child in node.children {
			g.sb.write_u8(tab)
			g.gen_node(child)
			g.sb.write_u8(lf)
		}
		g.sb.write_string('}')
	}
}

fn (mut g Generator) gen_type_annotation(type_name string) {
	g.sb.write_u8(lp)
	g.gen_string(type_name)
	g.sb.write_u8(rp)
}

fn (mut g Generator) gen_string(s string) {
	if document.can_be_bare_identifier(s) {
		g.sb.write_string(s)
	} else {
		g.write_quoted(s)
	}
}

fn (mut g Generator) gen_value(v document.Value) {
	match v {
		document.StringVal {
			match v.flag {
				.raw {
					g.sb.write_string(document.raw_string(v.value))
				}
				.quoted {
					g.write_quoted(v.value)
				}
				.bare {
					if document.can_be_bare_identifier(v.value) {
						g.sb.write_string(v.value)
					} else {
						g.write_quoted(v.value)
					}
				}
				else {
					g.write_quoted(v.value)
				}
			}
		}
		document.IntVal {
			match v.flag {
				.hex {
					if v.value < 0 {
						g.sb.write_string('-0x')
						g.sb.write_string(abs_i64_as_u64(v.value).hex())
					} else {
						g.sb.write_string('0x')
						g.sb.write_string(u64(v.value).hex())
					}
				}
				.octal {
					if v.value < 0 {
						g.sb.write_string('-0o')
						g.sb.write_string(format_oct(abs_i64_as_u64(v.value)))
					} else {
						g.sb.write_string('0o')
						g.sb.write_string(format_oct(u64(v.value)))
					}
				}
				.binary {
					if v.value < 0 {
						g.sb.write_string('-0b')
						g.sb.write_string(format_bin(abs_i64_as_u64(v.value)))
					} else {
						g.sb.write_string('0b')
						g.sb.write_string(format_bin(u64(v.value)))
					}
				}
				else {
					g.sb.write_string(v.value.str())
				}
			}
		}
		document.FloatVal {
			if math.is_inf(v.value, 0) {
				if v.value > 0 {
					g.sb.write_string('#inf')
				} else {
					g.sb.write_string('#-inf')
				}
			} else if math.is_nan(v.value) {
				g.sb.write_string('#nan')
			} else if v.flag == .scientific {
				g.sb.write_string(v.value.str())
			} else {
				g.sb.write_string(v.value.str())
			}
		}
		document.BoolVal {
			if v.value {
				g.sb.write_string('#true')
			} else {
				g.sb.write_string('#false')
			}
		}
		document.NullVal {
			g.sb.write_string('#null')
		}
	}
}

fn (mut g Generator) write_quoted(s string) {
	g.sb.write_u8(qt)
	for r in s.runes() {
		if r == `\n` {
			g.sb.write_string('\\n')
		} else if r == `\r` {
			g.sb.write_string('\\r')
		} else if r == `\t` {
			g.sb.write_string('\\t')
		} else if r == `"` {
			g.sb.write_string('\\"')
		} else if r == `\\` {
			g.sb.write_string('\\\\')
		} else if r == `\b` {
			g.sb.write_string('\\b')
		} else if r == `\f` {
			g.sb.write_string('\\f')
		} else if needs_unicode_escape(r) {
			g.sb.write_string('\\u{')
			g.sb.write_string(r.hex())
			g.sb.write_u8(125)
		} else {
			g.sb.write_rune(r)
		}
	}
	g.sb.write_u8(qt)
}

fn needs_unicode_escape(r rune) bool {
	return (r >= 0 && r <= 0x07) || r == 0x0b || (r >= 0x0e && r <= 0x1f)
		|| r == 0x7f || r == 0x85 || r == 0x2028 || r == 0x2029
		|| (r >= 0x200e && r <= 0x200f) || (r >= 0x202a && r <= 0x202e)
		|| (r >= 0x2066 && r <= 0x2069) || r == 0xfeff
}

fn abs_i64_as_u64(val i64) u64 {
	if val == -9223372036854775807 - 1 {
		return u64(1) << 63
	}
	if val < 0 {
		return u64(-val)
	}
	return u64(val)
}

fn format_oct(val u64) string {
	if val == 0 { return '0' }
	mut v := val
	mut buf := []u8{}
	for v > 0 {
		digit := u8(48 + (v & 7))
		buf.insert(0, digit)
		v >>= 3
	}
	return buf.bytestr()
}

fn format_bin(val u64) string {
	if val == 0 { return '0' }
	mut v := val
	mut buf := []u8{}
	for v > 0 {
		digit := u8(48 + (v & 1))
		buf.insert(0, digit)
		v >>= 1
	}
	return buf.bytestr()
}
