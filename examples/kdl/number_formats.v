// KDL: number formats — decimal, hex, octal, binary, scientific, underscore separators
import x.kdl
import x.kdl.document

fn main() {
	src := 'nums 42 -17 +99 0xFF 0o77 0b1010 3.14 1e10 1.5e-3 1_000_000 0xFF_FF'
	doc := kdl.parse(src)!
	node := doc.nodes[0]

	for entry in node.entries {
		match entry {
			document.Argument {
				v := entry.value
				if kdl.is_null(v) {
					println('#null')
				} else {
					s := kdl.as_string(v)
					i := kdl.as_int(v)
					f := kdl.as_f64(v)
					println('${s}  →  int=${i}  f64=${f}')
				}
			}
			document.Property {}
		}
	}
}
