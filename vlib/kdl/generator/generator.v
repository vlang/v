module generator

import strings
import kdl.document

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
	g.sb.write_u8(lf)
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
		g.sb.write_u8(lp)
		g.sb.write_string(node.type_name)
		g.sb.write_u8(rp)
	}
	if document.can_be_bare_identifier(node.name) {
		g.sb.write_string(node.name)
	} else {
		g.write_quoted(node.name)
	}
	for entry in node.entries {
		g.sb.write_u8(sp)
		match entry {
			document.Argument {
				if entry.type_name.len > 0 {
					g.sb.write_u8(lp)
					g.sb.write_string(entry.type_name)
					g.sb.write_u8(rp)
				}
				g.gen_value(entry.value)
			}
			document.Property {
				if entry.type_name.len > 0 {
					g.sb.write_u8(lp)
					g.sb.write_string(entry.type_name)
					g.sb.write_u8(rp)
				}
				if document.can_be_bare_identifier(entry.key) {
					g.sb.write_string(entry.key)
				} else {
					g.write_quoted(entry.key)
				}
				g.sb.write_u8(eq)
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

fn (mut g Generator) gen_value(v document.Value) {
	match v {
		document.StringVal {
			match v.flag {
				.raw {
					g.sb.write_string('#"')
					g.sb.write_string(v.value)
					g.sb.write_string('"#')
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
					g.sb.write_string('0x')
					g.sb.write_string(v.value.hex())
				}
				.octal {
					g.sb.write_string('0o')
					g.sb.write_string(format_oct(v.value))
				}
				.binary {
					g.sb.write_string('0b')
					g.sb.write_string(format_bin(v.value))
				}
				else {
					g.sb.write_string(v.value.str())
				}
			}
		}
		document.FloatVal {
			if v.flag == .scientific {
				g.sb.write_string(v.value.strsci(6))
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
	for c in s.bytes() {
		if c == lf {
			g.sb.write_string('\\n')
		} else if c == cr {
			g.sb.write_string('\\r')
		} else if c == tab {
			g.sb.write_string('\\t')
		} else if c == qt {
			g.sb.write_string('\\"')
		} else if c == bs {
			g.sb.write_string('\\\\')
		} else if c == 8 {
			g.sb.write_string('\\b')
		} else if c == 12 {
			g.sb.write_string('\\f')
		} else if c < 32 {
			g.sb.write_string('\\u{${c.hex()}}')
		} else {
			g.sb.write_u8(c)
		}
	}
	g.sb.write_u8(qt)
}

fn format_oct(val i64) string {
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

fn format_bin(val i64) string {
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
