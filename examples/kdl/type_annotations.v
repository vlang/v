// KDL: type annotations on nodes, arguments, and properties
import x.kdl
import x.kdl.document

fn main() {
	src := '
(published)date "2025-01-15"
(contributor)person name="Alice" (u8)42 (f64)3.14
config max-connections=(u16)65535
'
	doc := kdl.parse(src)!

	// Node with type annotation
	node1 := doc.nodes[0]
	println('node: (${node1.type_name})${node1.name} = ${kdl.as_string(node1.entries[0].value)}')

	// Arguments with type annotations
	node2 := doc.nodes[1]
	println('\nnode: ${node2.name}')
	for entry in node2.entries {
		match entry {
			document.Argument {
				println('  (${entry.type_name}) arg: ${kdl.as_string(entry.value)}')
			}
			document.Property {
				println('  prop: ${entry.key}=${kdl.as_string(entry.value)} (type_name=${entry.type_name})')
			}
		}
	}
}
