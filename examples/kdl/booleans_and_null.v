// KDL: booleans, null, and keyword numbers (#inf, #-inf, #nan)
import kdl
import kdl.document

fn main() {
	src := 'flags enabled=#true debug=#false cache=#null pos-inf=#inf neg-inf=#-inf not-a-number=#nan'
	doc := kdl.parse(src)!
	node := doc.nodes[0]

	for entry in node.entries {
		match entry {
			document.Property {
				v := entry.value
				println('${entry.key} = ${kdl.as_string(v)}  (bool=${kdl.as_bool(v)}, null=${kdl.is_null(v)})')
			}
			document.Argument {}
		}
	}

	// #-inf and #nan as arguments
	doc2 := kdl.parse('special #inf #-inf #nan')!
	println('\nkeyword numbers:')
	node2 := doc2.nodes[0]
	for entry in node2.entries {
		match entry {
			document.Argument { println('  ${kdl.as_string(entry.value)}') }
			document.Property {}
		}
	}
}
